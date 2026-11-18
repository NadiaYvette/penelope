-- |
-- Module      : Penelope.Backend.MLIR
-- Copyright   : (c) 2026 The Penelope Authors
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- MLIR structured dialect SSA code-generation backend for Penelope.
-- Compiles typed polycategorical hypergraphs into MLIR SSA function regions
-- using basic block arguments (@MergeBlockArgs@) and domain-specific @organ@
-- dialect operations with Perceus in-place reuse annotations.
--
-- {- Note [MLIR Region CFG and Organ Dialect Lowering]
-- MLIR (Multi-Level Intermediate Representation) structures intermediate code
-- into nested regions, basic blocks, and operations. Unlike traditional SSA forms
-- that rely on phi nodes, MLIR basic blocks pass values explicitly via block arguments:
--
-- @
--   ^bb_entry(%ev_0: !llvm.ptr, %in_0: !llvm.ptr):
--     ...
-- @
--
-- In this backend, 'MLIRCarrier' tracks the monotonic register counter @cnt@ to
-- synthesize unique SSA value identifiers (@%r_0@, %res_1@).
--
-- Operations lowering mutable record updates emit the specialized instruction
-- 'emitMLIRSlotUpdate':
--
-- @
--   %r_2 = organ.rep.slot_update %r_1[0], %val_0 {reuse = #organ.freestanding_linear} : !organ.rep
-- @
--
-- The @reuse@ attribute communicates the static 'PlacePlan' discovered by the
-- staging engine to downstream MLIR optimization passes:
--
-- * @#organ.freestanding_linear@: Downstream passes can lower directly to an in-place
--   pointer write without any reference-count guards or atomic fences.
-- * @#organ.perceus_reuse@: Downstream passes synthesize a conditional uniqueness
--   branch (@m_rc == 1@), falling back to allocation and copy when shared.
-- * @#organ.fresh_alloc@: A fresh allocation pass is required.
-- -}
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Backend.MLIR
  ( -- * Symbolic Values and Carrier Monad
    SSAVal(..)
  , MLIRCarrier(..)

    -- * MLIR Region and Instruction Emission
  , emitMLIRRegionCFG
  , emitMLIRSlotUpdate

    -- * Carrier Implementation and Backend Plugin
  , mlirPolyCarrier
  , mlirBackendPlugin
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
import Penelope.Perceus (PlacePlan(..))

-- | Symbolic MLIR SSA register identifier (e.g. @"%0"@, @"%arg0"@, @"%ev_vec"@).
newtype SSAVal (a :: Type) = SSAVal { unSSAVal :: Text }
  deriving (Show, Eq)

-- | State-carrying carrier monad for accumulating MLIR instructions and basic blocks.
--
--   Accepts an initial SSA register counter and returns the advanced counter,
--   the accumulated MLIR instruction lines, and the output register frame.
newtype MLIRCarrier (outs :: Row) = MLIRCarrier
  { runMLIRCarrier :: Int -> (Int, [Text], Any) }

-- | Emits a complete MLIR function region CFG enclosing an evaluated 'MLIRCarrier'.
--
--   Constructs the function header with LLVM pointer types, the entry block @^bb_entry@,
--   the accumulated instruction body, and a final @func.return@ terminator.
emitMLIRRegionCFG :: Text -> MLIRCarrier outs -> Text
emitMLIRRegionCFG !fnName (MLIRCarrier f) =
  let (_, !linesOut, outObj) = f 0
      !(RowRep (Rep _ outArr)) = unsafeCoerce# outObj :: RowRep SSAVal Any Any ()
      !retRegs = [ unSSAVal (unsafeCoerce# (indexSmallArray outArr i) :: SSAVal Any)
                 | i <- [0 .. sizeofSmallArray outArr - 1]
                 ]
      !retLine = "    func.return " <> T.intercalate ", " retRegs <> " : !llvm.ptr"
  in T.unlines $
       [ "func.func @" <> fnName <> "(%ev_vec: !llvm.ptr, %arg0: !llvm.ptr) -> !llvm.ptr {"
       , "  ^bb_entry(%ev_0: !llvm.ptr, %in_0: !llvm.ptr):"
       ] ++ linesOut ++ [ retLine, "}" ]

-- | Emits an MLIR @organ.rep.slot_update@ operation annotated with the static 'PlacePlan'.
--
--   Attaches the appropriate @reuse@ attribute (@freestanding_linear@, @perceus_reuse@,
--   or @fresh_alloc@) to inform downstream optimization and lowering pipelines.
emitMLIRSlotUpdate
  :: PlacePlan
     -- ^ Inferred memory placement plan.
  -> SSAVal a
     -- ^ Base representation register name.
  -> Int
     -- ^ Canonical slot index in the Polytropos record.
  -> SSAVal b
     -- ^ New payload register to store into the slot.
  -> (Int -> (Int, [Text], SSAVal a))
emitMLIRSlotUpdate !plan (SSAVal baseReg) !slotIdx (SSAVal valReg) !counter =
  let !outReg = "%r_" <> T.pack (show counter)
      !reuseAttr = case plan of
        PlaceInPlaceLinear _ _ -> "{reuse = #organ.freestanding_linear}"
        PlacePerceusReuse  _ _ -> "{reuse = #organ.perceus_reuse}"
        _                      -> "{reuse = #organ.fresh_alloc}"
      !line = "    " <> outReg <> " = organ.rep.slot_update " <> baseReg
              <> "[" <> T.pack (show slotIdx) <> "], " <> valReg <> " " <> reuseAttr <> " : !organ.rep"
  in (counter + 1, [line], SSAVal outReg)

-- | Polycategorical carrier implementation targeting MLIR SSA generation.
--   Lowers static row morphisms to zero-instruction symbolic register permutations.
--
-- == Operational Pipeline & Invariants
-- 1. __Zero-Instruction Routing:__
--    'pcRoute' gathers symbolic SSA register variables via 'gatherRep' in \(\mathcal{O}(|mid|)\) time,
--    emitting zero intermediate LLVM or MLIR load/store operations.
-- 2. __Pure Wire Propagation:__
--    Pure function nodes ('pcLiftPure') are evaluated symbolically during lowering,
--    passing permuted input registers directly into downstream continuation leaves.
-- 3. __Block Argument Coherence:__
--    Emits SSA register references conformant to MLIR region and basic block scoping rules.
mlirPolyCarrier :: PolyCarrier MLIRCarrier
mlirPolyCarrier = PolyCarrier
  { pcRoute = \inMap symFrame -> MLIRCarrier $ \cnt ->
      let !(RowRep inRep) = unsafeCoerce# symFrame :: RowRep SSAVal Any Any ()
          !outRep         = gatherRep inMap inRep
      in (cnt, ["    // Route RowMorph (zero-instruction SSA register permutation)"], unsafeCoerce# (RowRep outRep))
  , pcLiftPure = \inMap _ (BranchCont k) symFrame -> MLIRCarrier $ \cnt ->
      let !(RowRep inRep) = unsafeCoerce# symFrame :: RowRep SSAVal Any Any ()
          !opInRep        = gatherRep inMap inRep
          !coercedMid     = unsafeCoerce# (RowRep opInRep) :: Record opOuts
      in runMLIRCarrier (k coercedMid) cnt
  }

-- | Complete 'BackendPlugin' instance for MLIR SSA generation (@MergeBlockArgs@).
--   Produces structured MLIR functions with basic blocks and Perceus reuse attributes.
--
--   Key characteristics:
--   * Synthesizes 'func.func' with block arguments (@MergeBlockArgs@) rather than phi nodes.
--   * Dispatches effect operations via '@ev_vec' indirect function pointers ('llvm.getelementptr' + 'llvm.call').
--   * Attaches runtime profile annotations ({rts = "..."}) to function calls for downstream lowering passes.
mlirBackendPlugin :: BackendPlugin SSAVal MLIRCarrier Text
mlirBackendPlugin = BackendPlugin
  { bpName         = "mlir-ssa-backend"
  , bpMergeStyle   = MergeBlockArgs
  , bpCarrier      = mlirPolyCarrier
  , bpCoreHandlers = \_layout !rts ->
      -- Vectorized MLIR handler table:
      -- Each entry calculates the indirect handler pointer offset in '%ev_vec', invokes
      -- the handler function passing the input record register '%in_0', and binds the
      -- result to '%res_N' before invoking continuation branches.
      let mkSlotHandler :: Handler MLIRCarrier e
          mkSlotHandler = Handler $ \tag# _opAny inObj contObj ->
            MLIRCarrier $ \cnt ->
              let !(RowRep (Rep _ inArr))   = unsafeCoerce# inObj :: RowRep SSAVal Any Any ()
                  !(RowRep (Rep _ contArr)) = unsafeCoerce# contObj :: RowRep Any Any Any ()
                  !slotTag = I# (word2Int# tag#)
                  !argReg  = if sizeofSmallArray inArr > 0
                               then unSSAVal (unsafeCoerce# (indexSmallArray inArr 0) :: SSAVal Any)
                               else "%in_0"
                  !resReg  = "%res_" <> T.pack (show cnt)
                  !evLine  = "    %fn_" <> T.pack (show cnt) <> " = llvm.getelementptr inbounds %ev_vec["
                             <> T.pack (show slotTag) <> "] : (!llvm.ptr) -> !llvm.ptr"
                  !callLn  = "    " <> resReg <> " = llvm.call %fn_" <> T.pack (show cnt)
                             <> "(%ev_vec, " <> argReg <> ") {rts = \"" <> T.pack (show rts) <> "\"} : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr"
                  !outFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# (SSAVal resReg)]))
              in if sizeofSmallArray contArr == 0
                   then (cnt + 1, [evLine, callLn], unsafeCoerce# outFrame)
                   else
                     let !(BranchCont k0) = unsafeCoerce# (indexSmallArray contArr 0)
                         (!cnt', !downLines, !finalOut) =
                           runMLIRCarrier (k0 (unsafeCoerce# outFrame)) (cnt + 1)
                     in (cnt', evLine : callLn : downLines, finalOut)
      in unsafeCoerce# (RowRep (Rep 0 (smallArrayFromList (replicate 16 (unsafeCoerce# mkSlotHandler)))))
  , bpFinalize = \_layout _rts carrier ->
      emitMLIRRegionCFG "penelope_entry" carrier
  }
