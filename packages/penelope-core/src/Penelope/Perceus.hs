-- |
-- Module      : Penelope.Perceus
-- Copyright   : (c) 2026 The Penelope Authors
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Dynamic reference count and in-place memory reuse analysis for Penelope.
-- Generalizes Lean 4's Perceus reference counting paradigm and Koka's drop-reuse
-- optimizations across polycategorical hypergraphs, synthesizing optimal placement
-- schedules ('PlacePlan') for multiple runtime profiles.
--
-- {- Note [Perceus Polycategorical Memory Inference]
-- Traditional reference counting systems suffer from atomic synchronization bottlenecks
-- and excessive retain/drop thrashing. The Perceus algorithm (Reinking et al., PLDI 2021)
-- proves that reference counting can be made highly competitive by pairing drops of
-- dead values with allocations of new values of the same size class ('PlacePerceusReuse').
--
-- In Penelope, we adapt Perceus to open row hypergraphs ('Penelope') through a two-pass
-- inference engine:
--
-- 1. Pass 1 (Bottom-Up Liveness): 'analyzePerceusLiveness' computes the exact set of
--    frame slots read along each control-flow continuation, propagating live sets
--    backwards across multi-exit branches ('BranchCont').
--
-- 2. Pass 2 (Node-Local Schedule Synthesis): 'computeNodePerceusPlan' analyzes the read
--    histogram ('gatherHistogram') against the incoming live sets:
--    - For slots read multiple times or surviving along future branches, it emits
--      pre-retains ('pnPreRetains') where necessary.
--    - For slots whose last use occurs at this node, it identifies candidate memory
--      buffers matching the size class of downstream outputs ('reusablePool').
--    - When a size-matched buffer is available, it synthesizes an in-place mutation
--      or reuse plan ('PlaceInPlaceLinear' or 'PlacePerceusReuse') rather than a fresh
--      heap allocation ('PlaceFreshAlloc').
--    - Dead slots that cannot be reused are scheduled for immediate drop ('pnEntryDrops')
--      on entry to branch continuations.
--
-- In freestanding kernel mode ('RTSFreestandingLinear'), reference count updates and
-- atomic branches are completely elided, turning all drops and reuses into static
-- register or in-place struct updates.
--
-- {- Note [Size-Class Bin-Packing and Liveness Invariants]
-- Dynamic reuse algorithms depend on precise size-class matching to ensure memory safety:
--
-- 1. **Size-Class Matching Invariant:**
--    A dead input memory buffer can only be repurposed for an output port if their
--    payload capacities ('SizeClass') match exactly:
--    @
--      sizeClass(inputSlot) == sizeClass(outputPort)
--    @
--    If the output requires fewer slots, shrinking the buffer would risk memory corruption
--    if downstream consumers expect the full allocation size. If the output requires more
--    slots, writing beyond the allocated bounds causes buffer overflows.
--    When no exact match exists in 'reusablePool', the planner falls back to 'PlaceFreshAlloc'.
--
-- 2. **Multi-Exit Continuation Liveness:**
--    When an operation branches into $k$ distinct exits, liveness analysis must account
--    for diverging consumption:
--    - A slot live along branch 0 but dead along branch 1 cannot be reclaimed before
--      the branch decision is made.
--    - 'computeNodePerceusPlan' emits per-branch drop sets ('pnEntryDrops'), inserting
--      drops into branch 1 immediately upon entry without affecting branch 0.
--    - Slots live across any branch ('liveAnyBranch') are protected from premature reuse.
-- -}
-- -}
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes #-}

module Penelope.Perceus
  ( -- * Target Runtime Profiles
    RTSProfile(..)
  , SizeClass(..)
  , SlotDisp(..)
  , PlacePlan(..)
  , PerceusNote(..)

    -- * Liveness Analysis and Schedule Synthesis
  , gatherHistogram
  , analyzePerceusLiveness
  , computeNodePerceusPlan
  ) where

import Data.Functor.Identity (Identity(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Primitive.PrimArray (sizeofPrimArray, indexPrimArray)
import Data.Primitive.SmallArray (SmallArray, sizeofSmallArray, indexSmallArray)
import GHC.Exts (Any, unsafeCoerce#)

import Polytropos (Record)
import Polytropos.Core (Rep(..), RowRep(..))
import Polytropos.RowMorph (RowMorph(..))
import Penelope.Core.Syntax (Penelope(..), BranchCont(..))

-- | Target runtime memory execution profile.
--   Enables the same polycategorical specification to target freestanding microkernels,
--   thread-local runtimes, concurrent architectures, or managed garbage-collected platforms.
data RTSProfile
  = RTSFreestandingLinear
    -- ^ Profile 0 (@#[freestanding_linear]@): Zero RC object header, zero atomic operations.
    --   Drops and reuses fuse into 100% static in-place struct and register writes for
    --   freestanding OS kernels and bare-metal firmware.
  | RTSLean4ThreadLocal
    -- ^ Profile 1 (@#[thread_local_perceus]@): Lean 4 @m_rc > 0@ header with statically
    --   elided multi-thread check branches, using non-atomic inc/dec paired with Perceus reuse.
  | RTSLean4Concurrent
    -- ^ Profile 2 (@#[concurrent_shared]@): Full Lean 4 concurrent reference counting with
    --   Release/Acquire atomic memory fences and multi-threaded sharing markings.
  | RTSManagedGC
    -- ^ Profile 3 (@#[managed_gc]@): Managed execution target (JVM, WasmGC, or GHC GC).
    --   Reference count operations are treated as no-ops while structural in-place reuse
    --   is preserved where safe.
  deriving (Show, Eq, Ord)

-- | Allocation size class measuring the number of contiguous payload slots
--   in an unboxed 'Polytropos.Core.Rep' memory buffer.
newtype SizeClass = SizeClass { unSizeClass :: Int }
  deriving (Show, Eq, Ord)

-- | Disposition of an input frame slot at a specific hypergraph node.
data SlotDisp
  = DispBorrowedRead
    -- ^ Slot is read immutably without retaining or consuming ownership.
  | DispRetainBefore {-# UNPACK #-} !Int
    -- ^ Slot ownership must be incremented by the specified count prior to node execution.
  | DispLastUseConsume
    -- ^ Final read of this slot along the current control path; ownership is consumed.
  | DispReuseCandidate {-# UNPACK #-} !SizeClass
    -- ^ Slot is consumed and its underlying memory buffer is eligible for in-place reuse.
  deriving (Show, Eq)

-- | Physical memory placement plan synthesized for an output port register.
data PlacePlan
  = PlaceScalarReg
    -- ^ Output is a primitive unboxed scalar held directly in a CPU register.
  | PlaceFreshAlloc {-# UNPACK #-} !SizeClass
    -- ^ No reusable buffer was available; allocate a fresh heap buffer of the specified size.
  | PlaceInPlaceLinear {-# UNPACK #-} !Int {-# UNPACK #-} !SizeClass
    -- ^ Profile 0 freestanding mode: directly mutate the existing input buffer at the given slot.
  | PlacePerceusReuse {-# UNPACK #-} !Int {-# UNPACK #-} !SizeClass
    -- ^ Profile 1/2 Perceus mode: reuse the input buffer at the given slot if uniquely owned (@m_rc == 1@).
  deriving (Show, Eq)

-- | Complete Perceus and placement schedule inferred for a single 'Penelope' hypergraph node.
data PerceusNote = PerceusNote
  { pnPreRetains   :: !(IntMap Int)
    -- ^ Mapping from @slot_idx@ to required retain count emitted before node execution.
  , pnEntryDrops   :: ![IntSet]
    -- ^ For each of the @k@ exit continuations, the set of dead frame slots to drop immediately upon entry.
  , pnOutputPlaces :: !(IntMap PlacePlan)
    -- ^ Mapping from @out_port_idx@ to its synthesized memory placement plan.
  } deriving (Show, Eq)

-- | Constructs a read-multiplicity histogram mapping each input frame slot
--   to its total occurrence count in the static row morphism.
--
-- == Operational Semantics & Invariants
-- Reads the unboxed 'PrimArray' indices in a single contiguous unboxed pass.
-- If an input register slot appears \(c > 1\) times, the node duplicates the value across
-- ports; under reference-counting profiles, this dictates the exact pre-retain count.
{-# INLINE gatherHistogram #-}
gatherHistogram :: RowMorph opIns ins -> IntMap Int
gatherHistogram (RowMorph indices) =
  let !n = sizeofPrimArray indices
      go !idx !acc
        | idx >= n  = acc
        | otherwise =
            let !slot = indexPrimArray indices idx
            in go (idx + 1) (IM.insertWith (+) slot 1 acc)
  in go 0 IM.empty

-- | Pass 1 (Bottom-Up): Computes the exact set of frame slots in @ins@ read along
--   any downstream control-flow continuation of a 'Penelope' hypergraph using phantom frames.
--
-- == Invariants
-- Uses 'phantomFrame' to traverse continuation functions without evaluating values.
-- The resulting 'IntSet' represents the exact union of all downstream live register slots.
analyzePerceusLiveness :: Penelope eh ef ins outs -> IntSet
analyzePerceusLiveness = go
  where
    go :: Penelope eh ef ins outs -> IntSet
    go (Route inMap) =
      IM.keysSet (gatherHistogram inMap)
    go (LiftPure inMap _ (BranchCont k)) =
      IS.union (IM.keysSet (gatherHistogram inMap)) (go (k phantomFrame))
    go (OpF _ inMap _ (RowRep (Rep _ contArr))) =
      IS.union (IM.keysSet (gatherHistogram inMap)) (foldBranches contArr)
    go (OpH _ inMap _ _ (RowRep (Rep _ contArr))) =
      IS.union (IM.keysSet (gatherHistogram inMap)) (foldBranches contArr)
    go (ScopeRow _ _ inner) =
      go inner

    foldBranches :: SmallArray (Identity Any) -> IntSet
    foldBranches arr =
      let !k = sizeofSmallArray arr
          loop !j !acc
            | j >= k    = acc
            | otherwise =
                let !(Identity contAny) = indexSmallArray arr j
                    !(BranchCont branchFn) = unsafeCoerce# contAny
                in loop (j + 1) (IS.union acc (go (branchFn phantomFrame)))
      in loop 0 IS.empty

    phantomFrame :: Record r
    phantomFrame = unsafeCoerce# ()

-- | Pass 2 (Node-Local Schedule Synthesis): Evaluates liveness, multiplicity, and size classes
--   to synthesize optimal 'ERetain', 'EDrop', and 'EReuse' placement schedules for a node.
--
-- == Operational Invariants & Schedule Synthesis Steps
-- 1. __Multiplicity Histogram Gathering:__
--    Computes the number of times each input slot is projected by 'inMap'.
--    Under RC profiles, multiple uses of a single boxed slot require @need = (c - 1) + extra@
--    retains to maintain reference balance.
-- 2. __Reuse Pool Partitioning:__
--    Slots that are both consumed by this node and dead across all downstream branch continuations
--    (@not (IS.member slot liveAnyBranch)@) are marked as eligible for in-place reuse.
-- 3. __Size-Class Bin Packing:__
--    For each boxed output port, 'findMatchingSlot' attempts to locate an exact size-class
--    match in the reuse pool:
--    - Match found: Emits 'PlacePerceusReuse' (or 'PlaceInPlaceLinear' under Profile 0),
--      recycling the backing buffer without invoking runtime allocators.
--    - Match absent: Emits 'PlaceFreshAlloc'.
-- 4. __Entry Drop Synthesis:__
--    Any leftover dead slots in the reuse pool that were not claimed for output reuse
--    are queued for deterministic reclamation in 'pnEntryDrops'.
computeNodePerceusPlan
  :: RTSProfile
  -> IntMap SizeClass     -- ^ Boxed Polytropos slots in @ins@ mapping to their size class.
  -> IntSet               -- ^ Set of slots live entering this node (@L_in@).
  -> RowMorph opIns ins   -- ^ Static input gathering row morphism for this node.
  -> [IntSet]             -- ^ Downstream live sets @[L_0, ..., L_{k-1}]@ for each exit continuation.
  -> IntMap SizeClass     -- ^ Output ports producing boxed values, with their required size class.
  -> PerceusNote
computeNodePerceusPlan !rts !boxedIn !liveIn !inMap !branchLives !boxedOut =
  let !hist          = gatherHistogram inMap
      !liveAnyBranch = foldl' IS.union IS.empty branchLives

      !preRetains =
        if rts == RTSFreestandingLinear || rts == RTSManagedGC
          then IM.empty
          else IM.mapMaybeWithKey
                 (\slot c ->
                    if not (IM.member slot boxedIn) then Nothing
                    else
                      let !extra = if IS.member slot liveAnyBranch then 1 else 0
                          !need  = (c - 1) + extra
                      in if need > 0 then Just need else Nothing)
                 hist

      !reusablePool =
        IM.filterWithKey
          (\slot _ -> IM.member slot hist && not (IS.member slot liveAnyBranch))
          boxedIn

      (!leftoverReusable, !outputPlaces) =
        IM.mapAccumWithKey
          (\pool _outPort outSc ->
             case findMatchingSlot outSc pool of
               Just (reuseSlot, pool') ->
                 let !plan = case rts of
                               RTSFreestandingLinear -> PlaceInPlaceLinear reuseSlot outSc
                               _                     -> PlacePerceusReuse  reuseSlot outSc
                 in (pool', plan)
               Nothing ->
                 (pool, PlaceFreshAlloc outSc))
          reusablePool
          boxedOut

      !unpairedLastUse = IM.keysSet leftoverReusable
      !entryDrops =
        if rts == RTSFreestandingLinear || rts == RTSManagedGC
          then map (const IS.empty) branchLives
          else map
                 (\bLive ->
                    IS.filter
                      (\slot ->
                         IM.member slot boxedIn
                         && not (IS.member slot bLive)
                         && (IS.member slot liveAnyBranch
                             || not (IM.member slot hist)
                             || IS.member slot unpairedLastUse))
                      liveIn)
                 branchLives
  in PerceusNote
       { pnPreRetains   = preRetains
       , pnEntryDrops   = entryDrops
       , pnOutputPlaces = outputPlaces
       }

-- | Searches a pool of reusable slots for the first slot matching the target size class.
--   When multiple candidates match, chooses the lowest-indexed canonical slot to preserve
--   locality and deterministic allocation schedules across compilation passes.
--   Returns the selected slot index alongside the updated pool with that slot removed.
findMatchingSlot :: SizeClass -> IntMap SizeClass -> Maybe (Int, IntMap SizeClass)
findMatchingSlot !targetSc !pool =
  case IM.toList (IM.filter (== targetSc) pool) of
    ((slot, _) : _) -> Just (slot, IM.delete slot pool)
    []              -> Nothing
