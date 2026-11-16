{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators, MagicHash #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
module Main (main) where

import Test.Hspec
import qualified Data.Text as T
import GHC.Exts (unsafeCoerce#)
import Penelope.Core
import Penelope.Backend.Plugin
import Penelope.Backend.Gimple
import Penelope.Perceus (PlacePlan(..), RTSProfile(..))
import Polytropos (RecordF)
import Polytropos.Core (Pair(..), Rep(..), RowRep(..))
import Polytropos.RowMorph (rowMorph)
import Data.Primitive.SmallArray (smallArrayFromList)

main :: IO ()
main = hspec $ do
  describe "Penelope.Backend.Gimple" $ do
    it "emits GIMPLE SSA entry and return for Route term" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
          initFrame :: RecordF GimpleVal '["x" ':= Int]
          initFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# (GimpleVal "r_3(D)")]))
          out = compileWithBackend gimpleBackendPlugin defaultHostTargetLayout RTSFreestandingLinear term initFrame
      T.isInfixOf "penelope_gimple_entry" out `shouldBe` True
      T.isInfixOf "return" out `shouldBe` True

    it "emits GIMPLE slot update with freestanding linear zero RC branch" $ do
      let plan = PlaceInPlaceLinear 0 (unsafeCoerce# ())
          (_, _, _, linesOut, GimpleVal dst) =
            emitGimpleComponentRefStore RTSFreestandingLinear plan (GimpleVal "r_3(D)") 0 (GimpleVal "val_1") 1 2 1
      dst `shouldBe` "r_3(D)"
      linesOut `shouldBe`
        [ "  // #[freestanding_linear] PlaceInPlaceLinear: zero RC branch"
        , "  // # .MEM_2 = VDEF <.MEM_1>"
        , "  r_3(D)->slots[0] = val_1;"
        ]
