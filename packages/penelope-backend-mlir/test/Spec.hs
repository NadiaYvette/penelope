{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators, MagicHash #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
module Main (main) where

import Test.Hspec
import qualified Data.Text as T
import GHC.Exts (unsafeCoerce#)
import Penelope.Core
import Penelope.Backend.Plugin
import Penelope.Backend.MLIR
import Penelope.Perceus (PlacePlan(..), RTSProfile(..))
import Polytropos (RecordF)
import Polytropos.Core (Pair(..), Rep(..), RowRep(..))
import Polytropos.RowMorph (rowMorph)
import Data.Primitive.SmallArray (smallArrayFromList)

main :: IO ()
main = hspec $ do
  describe "Penelope.Backend.MLIR" $ do
    it "emits MLIR func and return for Route term" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
          initFrame :: RecordF SSAVal '["x" ':= Int]
          initFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# (SSAVal "%arg0")]))
          out = compileWithBackend mlirBackendPlugin defaultHostTargetLayout RTSFreestandingLinear term initFrame
      T.isInfixOf "func.func @penelope_entry" out `shouldBe` True
      T.isInfixOf "func.return" out `shouldBe` True

    it "emits MLIR slot update instruction with linear reuse" $ do
      let plan = PlaceInPlaceLinear 0 (unsafeCoerce# ())
          (_, linesOut, SSAVal outReg) = emitMLIRSlotUpdate plan (SSAVal "%r0") 0 (SSAVal "%v0") 1
      outReg `shouldBe` "%r_1"
      take 1 linesOut `shouldBe` ["    %r_1 = organ.rep.slot_update %r0[0], %v0 {reuse = #organ.freestanding_linear} : !organ.rep"]
