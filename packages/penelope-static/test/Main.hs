{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module Main (main) where

import qualified Data.IntSet as IntSet
import GHC.Exts (unsafeCoerce#)
import Penelope.Core
import Penelope.Static
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (Pair (..))
import Polytropos.RowMorph (KnownRowMorph (..))
import Test.Hspec

data TestGet :: PolyEffect
data TestPut :: PolyEffect
type TestEF = '["get" ':= TestGet, "put" ':= TestPut]

data TestCatch :: PolyHEffect
type TestEH = '["catch" ':= TestCatch]

main :: IO ()
main = hspec $ do
  describe "Penelope.Static.Analysis" $ do
    it "constructs StaticPenelope and accumulates metadata via seqStatic" $ do
      let p1 :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          p1 = route
          p2 :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          p2 = route

          s1 = annotateStatic (Just ()) p1
          s2 = annotateStatic (Just ()) p2
          sComp = seqStatic s1 s2

      case spStatic sComp of
        Just () -> (1 :: Int) `shouldBe` 1
        _       -> expectationFailure "Expected Just () metadata"

    it "extracts active first-order effects and port liveness" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int, "y" ':= String]
          retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          term :: Penelope '[] TestEF '["x" ':= Int, "y" ':= String] '["x" ':= Int]
          term = OpF 0## m (unsafeCoerce# (42 :: Int)) conts

          report = analyzeLivenessAndEffects term

      IntSet.member 0 (lrActiveFirstOrder report) `shouldBe` True
      IntSet.member 0 (lrReadFrameSlots report) `shouldBe` True
      lrTotalRegions report `shouldBe` 0
      lrMaxBranchingFactor report `shouldBe` 1

    it "extracts higher-order regions and branch continuations" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          subProg :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          subProg = Route m
          reg = Region @'[] @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
          regions = insertField (Field @"body") reg emptyRecord
          term :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
          term = OpH 0## m regions (unsafeCoerce# ()) conts

          report = analyzeLivenessAndEffects term

      IntSet.member 0 (lrActiveHigherOrder report) `shouldBe` True
      lrTotalRegions report `shouldBe` 1
      lrMaxBranchingFactor report `shouldBe` 1

    it "analyzes 3-way exit branching factor" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          branchOk = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          branchRetry = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          branchAbort = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          handlers = insertField (Field @"ok") branchOk
                   $ insertField (Field @"retry") branchRetry
                   $ insertField (Field @"abort") branchAbort
                   $ emptyRecord
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = OpF 0## m (unsafeCoerce# ()) handlers

          report = analyzeLivenessAndEffects term

      lrMaxBranchingFactor report `shouldBe` 3
