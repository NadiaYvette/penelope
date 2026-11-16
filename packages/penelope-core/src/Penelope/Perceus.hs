-- VERBATIM_CODE_SEED: TP-007A
-- TARGET_REPO: ~/src/penelope
-- TARGET_PACKAGE: penelope-core
-- TARGET_FILES:
--   src/Penelope/Perceus.hs

{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes #-}

module Penelope.Perceus
  ( RTSProfile(..)
  , SizeClass(..)
  , SlotDisp(..)
  , PlacePlan(..)
  , PerceusNote(..)
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

-- | Target Runtime Memory Profile generalizing across Freestanding, Lean 4 Perceus, and GC.
data RTSProfile
  = RTSFreestandingLinear
    -- ^ Profile 0 (#[freestanding_linear]): Zero RC header, zero atomic ops;
    --   EReuse fuses with EDrop into 100% static in-place struct/register writes.
  | RTSLean4ThreadLocal
    -- ^ Profile 1 (#[thread_local_perceus]): Lean 4 `m_rc > 0` header with statically
    --   elided `is_mt` branch (non-atomic inc/dec + Perceus reuse).
  | RTSLean4Concurrent
    -- ^ Profile 2 (#[concurrent_shared]): Lean 4 `m_rc` with `mark_mt` and
    --   Release/Acquire atomic reference counting.
  | RTSManagedGC
    -- ^ Profile 3 (#[managed_gc]): JVM / WasmGC (RC ops become no-ops; unique EReuse still in-place).
  deriving (Show, Eq, Ord)

-- | Allocation size class (number of payload slots in 'Polytropos.Core.Rep').
newtype SizeClass = SizeClass { unSizeClass :: Int }
  deriving (Show, Eq, Ord)

-- | Disposition of an input frame slot at a specific hypergraph operation.
data SlotDisp
  = DispBorrowedRead
  | DispRetainBefore   {-# UNPACK #-} !Int
  | DispLastUseConsume
  | DispReuseCandidate {-# UNPACK #-} !SizeClass
  deriving (Show, Eq)

-- | Physical memory placement plan for an output port 'o'.
data PlacePlan
  = PlaceScalarReg
  | PlaceFreshAlloc    {-# UNPACK #-} !SizeClass
  | PlaceInPlaceLinear {-# UNPACK #-} !Int {-# UNPACK #-} !SizeClass
  | PlacePerceusReuse  {-# UNPACK #-} !Int {-# UNPACK #-} !SizeClass
  deriving (Show, Eq)

-- | Complete Perceus & Place-Inference schedule computed for a single 'Penelope' node.
data PerceusNote = PerceusNote
  { pnPreRetains   :: !(IntMap Int)
    -- ^ `slot_i -> retain_count` emitted before the operation executes.
  , pnEntryDrops   :: ![IntSet]
    -- ^ For each of the `k` exit continuations, dead frame slots to `EDrop` on entry.
  , pnOutputPlaces :: !(IntMap PlacePlan)
    -- ^ `out_port_o -> PlacePlan` (in-place mutation vs. Perceus reuse vs. fresh alloc).
  } deriving (Show, Eq)

-- | Builds a read-multiplicity histogram `slot_i -> read_count` from a static `RowMorph`.
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

-- | Pass 1 (Bottom-Up): Computes the exact set of frame slots in `ins` read along
--   any control-flow path of a `Penelope` hypergraph using phantom port frames.
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

-- | Pass 2 (Node-Local): Synthesizes `ERetain`, `EDrop`, and `EReuse` place plans.
computeNodePerceusPlan
  :: RTSProfile
  -> IntMap SizeClass     -- ^ Boxed `Polytropos` slots in `ins` (`slot_i -> SizeClass`)
  -> IntSet               -- ^ Currently live slots entering this node (`L_in`)
  -> RowMorph opIns ins   -- ^ The node's static input gather map
  -> [IntSet]             -- ^ Downstream live sets `[L_0, ..., L_{k-1}]` for each of the `k` exits
  -> IntMap SizeClass     -- ^ Output ports producing a boxed `Polytropos` value (`out_o -> SizeClass`)
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

findMatchingSlot :: SizeClass -> IntMap SizeClass -> Maybe (Int, IntMap SizeClass)
findMatchingSlot !targetSc !pool =
  case IM.toList (IM.filter (== targetSc) pool) of
    ((slot, _) : _) -> Just (slot, IM.delete slot pool)
    []              -> Nothing