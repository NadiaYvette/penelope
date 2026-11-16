-- VERBATIM_CODE_SEED: TP-007C
-- TARGET_REPO: ~/src/penelope
-- TARGET_PACKAGE: penelope-backend-mlir, penelope-backend-gimple
-- TARGET_FILES:
--   src/Penelope/Backend/MLIR.hs
--   src/Penelope/Backend/Gimple.hs

{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Backend.MLIR
  ( SSAVal(..)
  , MLIRCarrier(..)
  , emitMLIRRegionCFG
  , emitMLIRSlotUpdate
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

-- | Symbolic MLIR SSA register name (`%0`, `%arg0`, `%ev_vec`).
newtype SSAVal (a :: Type) = SSAVal { unSSAVal :: Text }
  deriving (Show, Eq)

-- | State-carrying MLIR region/block emitter.
newtype MLIRCarrier (outs :: Row) = MLIRCarrier
  { runMLIRCarrier :: Int -> (Int, [Text], Any) }

-- | Emit an MLIR region CFG from an evaluated `MLIRCarrier`.
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

-- | Emit an MLIR `organ.rep.slot_update` instruction respecting the `PlacePlan`.
emitMLIRSlotUpdate :: PlacePlan -> SSAVal a -> Int -> SSAVal b -> (Int -> (Int, [Text], SSAVal a))
emitMLIRSlotUpdate !plan (SSAVal baseReg) !slotIdx (SSAVal valReg) !counter =
  let !outReg = "%r_" <> T.pack (show counter)
      !reuseAttr = case plan of
        PlaceInPlaceLinear _ _ -> "{reuse = #organ.freestanding_linear}"
        PlacePerceusReuse  _ _ -> "{reuse = #organ.perceus_reuse}"
        _                      -> "{reuse = #organ.fresh_alloc}"
      !line = "    " <> outReg <> " = organ.rep.slot_update " <> baseReg
              <> "[" <> T.pack (show slotIdx) <> "], " <> valReg <> " " <> reuseAttr <> " : !organ.rep"
  in (counter + 1, [line], SSAVal outReg)

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

-- | Universal `BackendPlugin` for MLIR SSA with block arguments (`MergeBlockArgs`).
mlirBackendPlugin :: BackendPlugin SSAVal MLIRCarrier Text
mlirBackendPlugin = BackendPlugin
  { bpName         = "mlir-ssa-backend"
  , bpMergeStyle   = MergeBlockArgs
  , bpCarrier      = mlirPolyCarrier
  , bpCoreHandlers = \_layout !rts ->
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
