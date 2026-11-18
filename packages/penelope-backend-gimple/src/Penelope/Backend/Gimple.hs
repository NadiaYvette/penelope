-- |
-- Module      : Penelope.Backend.Gimple
-- Copyright   : (c) 2026 The Penelope Authors
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- GCC @-fgimple@ SSA tuple code-generation backend for Penelope.
-- Compiles typed polycategorical hypergraphs directly into GCC SSA tuples
-- with explicit virtual memory state token threading (@# .MEM@) and Perceus
-- in-place memory reuse.
--
-- {- Note [GCC -fgimple Virtual Memory SSA Threading]
-- GCC's `-fgimple` syntax provides a high-level representation of GCC's internal
-- Static Single Assignment (SSA) GIMPLE tuples. In standard C code, memory mutations
-- are implicit side effects. In GIMPLE SSA, however, all memory operations are made
-- strictly explicit through virtual SSA operands:
--
-- 1. Virtual Use (@# VUSE <.MEM_k>@): Emitted before statements that inspect memory
--    without altering its contents.
-- 2. Virtual Definition (@# .MEM_{k+1} = VDEF <.MEM_k>@): Emitted immediately prior
--    to any store instruction, creating a fresh versioned memory token that sequences
--    subsequent reads and writes.
-- 3. Virtual Phi Nodes (@# .MEM_{join} = PHI <.MEM_a, .MEM_b>@): Merges memory versions
--    originating from diverging control paths (e.g. following an in-place mutation diamond).
--
-- In this backend, 'GimpleCarrier' threads an explicit 'memTokenCounter', incrementing
-- it monotonically upon every memory mutation or handler invocation. This preserves
-- strict effect sequencing and prevents GCC optimization passes from illegally
-- hoisting or sinking memory accesses across effect boundaries.
--
-- When targeting 'RTSFreestandingLinear' (Profile 0), the reference-count check
-- is omitted completely, emitting direct in-place stores into @slots[c_idx]@.
-- For dynamic profiles, an @m_rc == 1@ branch diamond is synthesized, copying on write
-- via @__organ_cow_dup@ when shared.
--
-- {- Note [GIMPLE SSA Basic Block Layout and Indirect Effect Vectors]
-- Compiling polycategorical hypergraphs to GCC `-fgimple` requires generating valid
-- SSA form with explicit basic block graphs:
--
-- 1. **Entry Block '__BB(2)':**
--    GCC reserves basic blocks 0 (ENTRY) and 1 (EXIT). Generated C code begins at
--    '__BB(2)', receiving the indirect effect dispatch vector ('ev_vec_2(D)') and
--    the input Polytropos record pointer ('r_3(D)').
--
-- 2. **Indirect Effect Call Dispatch:**
--    Effect handlers are held in an array of function pointers inside 'ev_vec_2'.
--    Each effect operation retrieves its handler by constructor tag:
--    @
--      // # .MEM_{k+1} = VDEF <.MEM_k>
--      ev_res_4 = ev_vec_2(D)->handlers[slotTag] (ev_vec_2(D), r_3(D));
--    @
--    The handler invocation is sequenced through the virtual memory state token '.MEM',
--    ensuring that external I/O or state modifications cannot be hoisted past preceding
--    effect invocations.
--
-- 3. **Terminal VUSE and Return:**
--    The final return statement of the generated function:
--    @
--      // # VUSE <.MEM_{final}>
--      return r_final;
--    @
--    references the ultimate memory token '.MEM_{final}', guaranteeing that all preceding
--    stores into 'r->slots' are recognized by GCC's Dead Store Elimination (DSE) pass
--    as live dependencies of the returned pointer.
-- -}
-- -}
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Backend.Gimple
  ( -- * Symbolic Values and Carrier Monad
    GimpleVal(..)
  , GimpleCarrier(..)

    -- * GIMPLE Store and Memory Emission
  , emitGimpleComponentRefStore

    -- * Carrier Implementation and Backend Plugin
  , gimplePolyCarrier
  , gimpleBackendPlugin
  ) where

import Data.Kind (Type)
import Data.Primitive.SmallArray (sizeofSmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (Any, Int(..), unsafeCoerce#, word2Int#)

import Polytropos (Record)
import Polytropos.Core (Rep(..), RowRep(..), Row)
import Polytropos.RowMorph (gatherRep)
import Penelope.Core.Syntax (BranchCont(..))
import Penelope.Engine (PolyCarrier(..), Handler(..))
import Penelope.Backend.Plugin (MergeStyle(..), BackendPlugin(..))
import Penelope.Perceus (RTSProfile(..), PlacePlan(..), SizeClass(..))

-- | Symbolic GIMPLE scalar or pointer SSA variable name (e.g. @"r_3(D)"@, @"res_5"@).
newtype GimpleVal (a :: Type) = GimpleVal { unGimpleVal :: Text }
  deriving (Show, Eq)

-- | State monad threading compiler counters to emit valid @-fgimple@ SSA tuples.
--
--   Carries:
--   [@ssaVarCounter@]: Next unique SSA register ID (@res_5@, @rc_6@).
--   [@bbCounter@]: Next unique basic block ID (@__BB(4)@).
--   [@memTokenCounter@]: Next virtual memory version ID (@.MEM_3@).
--
--   Yields accumulated tuple lines and the final symbolic result frame.
newtype GimpleCarrier (outs :: Row) = GimpleCarrier
  { runGimpleCarrier
      :: Int -- ^ Initial SSA variable counter
      -> Int -- ^ Initial basic block counter
      -> Int -- ^ Initial virtual memory token version counter
      -> (Int, Int, Int, [Text], Any)
  }

-- | Emits an in-place GIMPLE @COMPONENT_REF@ + @ARRAY_REF@ store (@r->slots[c_idx] = val@).
--
--   Pairs the mutation with a virtual SSA @VDEF@ token update, sequencing memory effects.
--   When running under dynamic Perceus profiles, wraps the mutation in an @m_rc == 1@
--   diamond with a @__PHI@ join node. Under 'RTSFreestandingLinear', emits a direct,
--   branchless in-place store.
emitGimpleComponentRefStore
  :: RTSProfile
     -- ^ Active runtime memory profile (freestanding vs dynamic Perceus).
  -> PlacePlan
     -- ^ Placement plan inferred by the staging engine.
  -> GimpleVal a
     -- ^ Base pointer variable (e.g. @"r_3(D)"@).
  -> Int
     -- ^ Canonical slot index in the Polytropos representation.
  -> GimpleVal b
     -- ^ Value variable to store in the designated slot.
  -> Int
     -- ^ Current SSA variable counter.
  -> Int
     -- ^ Current basic block counter.
  -> Int
     -- ^ Current virtual @.MEM@ version counter.
  -> (Int, Int, Int, [Text], GimpleVal a)
emitGimpleComponentRefStore !rts !plan (GimpleVal !basePtr) !slotIdx (GimpleVal !valName) !ssaC !bbC !memC =
  let !idxText = T.pack (show slotIdx)
  in case (rts, plan) of
       -- Profile 0 (#[freestanding_linear]): Zero `m_rc` check, direct in-place mutation!
       -- In freestanding kernel code, ownership is verified statically at compile time
       -- by 'Penelope.Stage.OrganIR.verifyFreestandingLinear'. Because reference counts
       -- do not exist in this profile, memory writes bypass all atomic operations
       -- and conditional branches, emitting direct structure member assignments.
       (RTSFreestandingLinear, _) ->
         let !memNext = memC + 1
             !linesOut =
               [ "  // #[freestanding_linear] PlaceInPlaceLinear: zero RC branch"
               , "  // # .MEM_" <> T.pack (show memNext) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> basePtr <> "->slots[" <> idxText <> "] = " <> valName <> ";"
               ]
         in (ssaC, bbC, memNext, linesOut, GimpleVal basePtr)

       -- Profiles 1 & 2 (Lean 4 / Perceus `EReuse`): `if (r->m_rc == 1)` diamond with `__PHI` merge.
       -- Inspects the runtime reference counter 'm_rc':
       -- If uniquely owned (m_rc == 1), control jumps directly to '__BB(join)' to mutate in place.
       -- If shared (m_rc > 1), control branches to '__BB(cow)' to duplicate the buffer
       -- via '__organ_cow_dup', unsharing the memory before mutation.
       -- A GIMPLE '__PHI' node reunifies the base pointer and the freshly cloned pointer.
       (_, PlacePerceusReuse _ (SizeClass !sc)) ->
         let !rcVar    = "rc_"    <> T.pack (show ssaC)
             !cowPtr   = "r_cow_" <> T.pack (show (ssaC + 1))
             !dstPtr   = "r_dst_" <> T.pack (show (ssaC + 2))
             !bbCow    = bbC
             !bbJoin   = bbC + 1
             !memCow   = memC + 1
             !memJoin  = memC + 2
             !memFinal = memC + 3
             !linesOut =
               [ "  // # VUSE <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> rcVar <> " = " <> basePtr <> "->m_rc;"
               , "  if (" <> rcVar <> " == 1)"
               , "    goto __BB(" <> T.pack (show bbJoin) <> ");"
               , "  else"
               , "    goto __BB(" <> T.pack (show bbCow) <> ");"
               , ""
               , "  __BB(" <> T.pack (show bbCow) <> "):"
               , "  // # .MEM_" <> T.pack (show memCow) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> cowPtr <> " = __organ_cow_dup" <> T.pack (show sc) <> " (" <> basePtr <> ");"
               , "  goto __BB(" <> T.pack (show bbJoin) <> ");"
               , ""
               , "  __BB(" <> T.pack (show bbJoin) <> "):"
               , "  " <> dstPtr <> " = __PHI (__BB(" <> T.pack (show (bbCow - 1)) <> "): "
                        <> basePtr <> ", __BB(" <> T.pack (show bbCow) <> "): " <> cowPtr <> ");"
               , "  // # .MEM_" <> T.pack (show memJoin) <> " = PHI <.MEM_"
                        <> T.pack (show memC) <> ", .MEM_" <> T.pack (show memCow) <> ">"
               , "  // # .MEM_" <> T.pack (show memFinal) <> " = VDEF <.MEM_" <> T.pack (show memJoin) <> ">"
               , "  " <> dstPtr <> "->slots[" <> idxText <> "] = " <> valName <> ";"
               ]
         in (ssaC + 3, bbC + 2, memFinal, linesOut, GimpleVal dstPtr)

       -- Fallback: Fresh allocation + in-place store.
       -- Used when the staging engine determines that no existing input slot can be safely reused,
       -- or when allocating a completely new data constructor frame.
       (_, _) ->
         let !newPtr   = "r_new_" <> T.pack (show ssaC)
             !memAlloc = memC + 1
             !memStore = memC + 2
             !linesOut =
               [ "  // # .MEM_" <> T.pack (show memAlloc) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> newPtr <> " = __organ_alloc_rep (" <> basePtr <> ");"
               , "  // # .MEM_" <> T.pack (show memStore) <> " = VDEF <.MEM_" <> T.pack (show memAlloc) <> ">"
               , "  " <> newPtr <> "->slots[" <> idxText <> "] = " <> valName <> ";"
               ]
         in (ssaC + 1, bbC, memStore, linesOut, GimpleVal newPtr)

-- | Polycategorical carrier implementation for GCC GIMPLE SSA generation.
--   Lowers static row morphisms and lifted pure transformations into GIMPLE tuples.
--
-- == Operational Pipeline & Invariants
-- 1. __Pure Register Gathering:__
--    When lowering 'pcRoute', 'gatherRep' permutes symbolic SSA register values in
--    linear time without emitting memory load or store instructions.
-- 2. __Pure Node Elimination:__
--    'pcLiftPure' bypasses runtime evaluation entirely during code generation,
--    feedinggathered symbolic input registers directly into downstream continuation leaves.
-- 3. __Carrier State Monad Discipline:__
--    SSA variable indices (@ssaC@), basic block labels (@bbC@), and virtual memory tokens (@memC@)
--    are threaded strictly and monotonically across sequentially composed stages.
gimplePolyCarrier :: PolyCarrier GimpleCarrier
gimplePolyCarrier = PolyCarrier
  { pcRoute = \inMap symFrame -> GimpleCarrier $ \ssaC bbC memC ->
      let !(RowRep inRep) = unsafeCoerce# symFrame :: RowRep GimpleVal Any Any ()
          !outRep         = gatherRep inMap inRep
      in (ssaC, bbC, memC, ["  // Route RowMorph (GIMPLE SSA copy)"], unsafeCoerce# (RowRep outRep))
  , pcLiftPure = \inMap _ (BranchCont k) symFrame -> GimpleCarrier $ \ssaC bbC memC ->
      let !(RowRep inRep) = unsafeCoerce# symFrame :: RowRep GimpleVal Any Any ()
          !opInRep        = gatherRep inMap inRep
          !coercedMid     = unsafeCoerce# (RowRep opInRep) :: Record opOuts
      in runGimpleCarrier (k coercedMid) ssaC bbC memC
  }

-- | Complete 'BackendPlugin' instance for GCC @-fgimple@ SSA generation (@MergePhiNodes@).
--   Produces formatted GIMPLE C functions with basic blocks and explicit virtual memory tokens.
--
--   Key characteristics:
--   * Synthesizes '__GIMPLE (ssa)' annotated function signatures compatible with GCC 13+.
--   * Formats entry blocks with default input pointers:
--     @penelope_gimple_entry (struct PenelopeEffVec * ev_vec_2, struct PolytroposRep * r_3)@
--   * Tracks virtual memory versions across basic blocks, ending with a final @# VUSE <.MEM_k>@.
gimpleBackendPlugin :: BackendPlugin GimpleVal GimpleCarrier Text
gimpleBackendPlugin = BackendPlugin
  { bpName         = "gcc-gimple-ssa-backend"
  , bpMergeStyle   = MergePhiNodes
  , bpCarrier      = gimplePolyCarrier
  , bpCoreHandlers = \_layout !rts ->
      -- Construct vectorized GIMPLE handler array:
      -- Each handler retrieves the corresponding function pointer from the effect vector
      -- (ev_vec_2->handlers[tag]), emits the indirect call with virtual memory tokens,
      -- and schedules an in-place store or Perceus reuse into the result representation.
      let mkGimpleHandler :: Handler GimpleCarrier e
          mkGimpleHandler = Handler $ \tag# _opAny inObj contObj ->
            GimpleCarrier $ \ssaC bbC memC ->
              let !(RowRep (Rep _ inArr))   = unsafeCoerce# inObj :: RowRep GimpleVal Any Any ()
                  !(RowRep (Rep _ contArr)) = unsafeCoerce# contObj :: RowRep Any Any Any ()
                  !slotTag = I# (word2Int# tag#)
                  !basePtr = if sizeofSmallArray inArr > 0
                               then unGimpleVal (unsafeCoerce# (indexSmallArray inArr 0) :: GimpleVal Any)
                               else "r_3(D)"
                  !valExpr = "ev_res_" <> T.pack (show ssaC)
                  !memCall = memC + 1
                  !evCall  =
                    [ "  // # .MEM_" <> T.pack (show memCall) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
                    , "  " <> valExpr <> " = ev_vec_2(D)->handlers[" <> T.pack (show slotTag)
                           <> "] (ev_vec_2(D), " <> basePtr <> ");"
                    ]
                  !plan = case rts of
                    RTSFreestandingLinear -> PlaceInPlaceLinear 0 (SizeClass 3)
                    _                     -> PlacePerceusReuse  0 (SizeClass 3)
                  (!ssaC', !bbC', !memC', !storeLines, !dstVal) =
                    emitGimpleComponentRefStore
                      rts plan (GimpleVal basePtr) slotTag (GimpleVal valExpr)
                      (ssaC + 1) bbC memCall
                  !outFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# dstVal]))
              in if sizeofSmallArray contArr == 0
                   then (ssaC', bbC', memC', evCall ++ storeLines, unsafeCoerce# outFrame)
                   else
                     let !(BranchCont k0) = unsafeCoerce# (indexSmallArray contArr 0)
                         (!ssaF, !bbF, !memF, !downLines, !finalOut) =
                           runGimpleCarrier (k0 (unsafeCoerce# outFrame)) ssaC' bbC' memC'
                     in (ssaF, bbF, memF, evCall ++ storeLines ++ downLines, finalOut)
      in unsafeCoerce# (RowRep (Rep 0 (smallArrayFromList (replicate 16 (unsafeCoerce# mkGimpleHandler)))))
  , bpFinalize = \_layout _rts (GimpleCarrier f) ->
      let (_, _, !memFinal, !bodyLines, outObj) = f 4 3 1
          !(RowRep (Rep _ outArr)) = unsafeCoerce# outObj :: RowRep GimpleVal Any outs ()
          !retVal = if sizeofSmallArray outArr > 0
                      then unGimpleVal (unsafeCoerce# (indexSmallArray outArr 0) :: GimpleVal Any)
                      else "r_3(D)"
      in T.unlines $
           [ "struct PolytroposRep * __GIMPLE (ssa)"
           , "penelope_gimple_entry (struct PenelopeEffVec * ev_vec_2, struct PolytroposRep * r_3)"
           , "{"
           , "  __BB(2):"
           ] ++ bodyLines ++
           [ "  // # VUSE <.MEM_" <> T.pack (show memFinal) <> ">"
           , "  return " <> retVal <> ";"
           , "}"
           ]
  }
