{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
module Main (main) where

import Test.Hspec
import qualified Data.IntMap.Strict as IM
import Penelope.Core
import Penelope.Perceus
import Penelope.Stage.OrganIR
import Polytropos.Core (Pair(..))
import Polytropos.RowMorph (rowMorph)

main :: IO ()
main = hspec $ do
  describe "Penelope.Stage.OrganIR" $ do
    it "verifies pure Route term satisfies freestanding linear profile" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
          boxedIn = IM.singleton 0 (SizeClass 1)
          res = verifyFreestandingLinear boxedIn term
      case res of
        Right note -> pnOutputPlaces note `shouldBe` IM.empty
        Left err   -> expectationFailure (show err)

    it "rejects LiftPure closure in freestanding linear profile" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = LiftPure m id (BranchCont (\_ -> Route m))
          boxedIn = IM.singleton 0 (SizeClass 1)
          res = verifyFreestandingLinear boxedIn term
      res `shouldBe` Left ErrOpaqueClosureInKernelStage
