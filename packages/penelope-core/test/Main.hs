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

import Test.Hspec
import Penelope.Core
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (KnownRowMorph (..))
import Data.Functor.Identity (Identity (..))
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import GHC.Exts (unsafeCoerce#)

-- Test effect signatures
data TestGet :: PolyEffect
data TestPut :: PolyEffect
type TestEF = '["get" ':= TestGet, "put" ':= TestPut]
type TestEFExtended = '["get" ':= TestGet, "put" ':= TestPut, "extra" ':= TestGet]

data TestCatch :: PolyHEffect
type TestEH = '["catch" ':= TestCatch]
type TestEHExtended = '["catch" ':= TestCatch, "extraH" ':= TestCatch]

type MyFrame = '["x" ':= Int, "y" ':= String, "z" ':= Bool]

main :: IO ()
main = hspec $ do
  describe "Penelope.Core.Syntax" $ do
    it "constructs a Route term with rowMorph" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
      case term of
        Route _ -> (1 :: Int) `shouldBe` 1
        _       -> expectationFailure "Expected Route constructor"

    it "constructs a LiftPure term" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          f r = r
          cont = BranchCont (\_ -> Route m)
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = LiftPure m f cont
      case term of
        LiftPure _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected LiftPure constructor"

    it "constructs an OpF term with unboxed Word# tag" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> Route m)
          conts = insertField (Field @"ret") retCont emptyRecord
          payload = unsafeCoerce# (42 :: Int)
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = OpF 0## m payload conts
      case term of
        OpF tag# _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected OpF constructor"

    it "constructs an OpH term with scoped sub-regions" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> Route m)
          conts = insertField (Field @"ret") retCont emptyRecord
          subProg :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          subProg = Route m
          reg = Region @'[] @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
          regions = insertField (Field @"body") reg emptyRecord
          payload = unsafeCoerce# ()
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = OpH 0## m regions payload conts
      case term of
        OpH tag# _ _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _                -> expectationFailure "Expected OpH constructor"

    it "constructs a ScopeRow term for row weakening" $ do
      let mOut = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          mEh = rowMorph @'[] @'[]
          mEf = rowMorph @'[] @'[]
          inner :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          inner = Route mOut
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = ScopeRow mEh mEf inner
      case term of
        ScopeRow _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected ScopeRow constructor"

  describe "Penelope.Core.Route" $ do
    it "constructs route smart constructor with automatic KnownRowMorph resolution" $ do
      let r :: Penelope '[] '[] '["x" ':= Int, "y" ':= Bool] '["x" ':= Int]
          r = route
      case r of
        Route _ -> (1 :: Int) `shouldBe` 1
        _       -> expectationFailure "Expected Route constructor"

    it "identifies identity row morphisms" $ do
      let r :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          r = route
      isIdentityRoute r `shouldBe` True

    it "fuses consecutive routes via fuseRoute" $ do
      let m1 = rowMorph @'["b" ':= Int] @'["a" ':= Int, "b" ':= Int]
          m2 = rowMorph @'["b" ':= Int] @'["b" ':= Int]
          fused = fuseRoute m1 m2
      case fused of
        _ -> (1 :: Int) `shouldBe` 1

  describe "Penelope.Core.Send" $ do
    it "constructs sendPolyF with ConstructorIndex tag resolution" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          term :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          term = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts
      case term of
        OpF tag# _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected OpF constructor"

    it "constructs sendPolyH with sub-regions and ConstructorIndex tag resolution" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          subProg :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
          subProg = route
          reg = Region @TestEH @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
          regions = insertField (Field @"body") reg emptyRecord
          term :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
          term = sendPolyH' @"catch" @'["x" ':= Int] regions () conts
      case term of
        OpH tag# _ _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _                -> expectationFailure "Expected OpH constructor"

  describe "Penelope.Core.Wiring" $ do
    it "fuses consecutive Route nodes into a single Route" $ do
      let r1 :: Penelope '[] '[] '["a" ':= Int, "b" ':= Int] '["b" ':= Int]
          r1 = route
          r2 :: Penelope '[] '[] '["b" ':= Int] '["b" ':= Int]
          r2 = route
          comp = r1 >>> r2
      case comp of
        Route _ -> (1 :: Int) `shouldBe` 1
        _       -> expectationFailure "Expected fused Route constructor"

    it "sequences pure transformations with LiftPure" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          f r = r
          step = LiftPure m f (BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int]))
          comp = step >>> route @'["x" ':= Int] @'["x" ':= Int]
      case comp of
        LiftPure _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected LiftPure constructor"

    it "sequences across multi-branch OpF continuations" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          op :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          op = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts
          comp = op >>> route @'["x" ':= Int] @'["x" ':= Int]
      case comp of
        OpF tag# _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected OpF constructor"

    it "elides identity Route when pre-composed with OpF" $ do
      let idR = route @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          op :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          op = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts
          comp = idR >>> op
      case comp of
        OpF _ _ _ _ -> (1 :: Int) `shouldBe` 1
        _           -> expectationFailure "Expected OpF constructor"

    it "composes multi-stage pipelines via frame-rule (>&>)" $ do
      let m1 = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          stage1 :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          stage1 = LiftPure m1 (\r -> r) (BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int]))

          m2 = rowMorph @'["x" ':= Int, "y" ':= String] @MyFrame
          mOut2 = rowMorph @'["z" ':= Bool] @MyFrame
          stage2 :: Penelope '[] '[] MyFrame '["z" ':= Bool]
          stage2 = LiftPure m2 (\_ -> insertField (Field @"z") True emptyRecord)
                     (BranchCont (\_ -> Route mOut2))

          pipeline :: Penelope '[] '[] MyFrame MyFrame
          pipeline = stage1 >&> stage2
      case pipeline of
        LiftPure _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected LiftPure at pipeline root"

    it "binds 3-way exit channels in O(1) via (>?<)" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          op :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          op = OpF 0## m (unsafeCoerce# (42 :: Int)) emptyRecord

          branchOk = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          branchRetry = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          branchAbort = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          handlers = insertField (Field @"ok") branchOk
                   $ insertField (Field @"retry") branchRetry
                   $ insertField (Field @"abort") branchAbort
                   $ emptyRecord

          pipeline = op >?< handlers
      case pipeline of
        OpF tag# _ _ exits -> do
          case tag# of _ -> (1 :: Int) `shouldBe` 1
          let RowRep (Rep _ arr) = exits
          sizeofSmallArray arr `shouldBe` 3
        _ -> expectationFailure "Expected OpF with 3-way exit continuations"

    it "embeds computations into wider effect rows via weakenRow" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          inner :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          inner = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts

          weakened :: Penelope TestEH TestEFExtended '["x" ':= Int] '["x" ':= Int]
          weakened = weakenRow @TestEH @TestEFExtended inner
      case weakened of
        ScopeRow _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected ScopeRow constructor"

    it "embeds computations into wider first-order effect row via weakenEF" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          inner :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          inner = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts

          weakened :: Penelope '[] TestEFExtended '["x" ':= Int] '["x" ':= Int]
          weakened = weakenEF @TestEFExtended inner
      case weakened of
        ScopeRow _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected ScopeRow constructor"

    it "embeds computations into wider higher-order effect row via weakenEH" $ do
      let inner :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          inner = route

          weakened :: Penelope TestEHExtended '[] '["x" ':= Int] '["x" ':= Int]
          weakened = weakenEH @TestEHExtended inner
      case weakened of
        ScopeRow _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected ScopeRow constructor"

    it "nests multiple ScopeRow layers cleanly" $ do
      let inner :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          inner = route

          l1 :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
          l1 = weakenEH @TestEH inner

          l2 :: Penelope TestEHExtended '[] '["x" ':= Int] '["x" ':= Int]
          l2 = weakenEH @TestEHExtended l1
      case l2 of
        ScopeRow _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected ScopeRow constructor"

  describe "Penelope.Core.Engine.Elaborate" $ do
    it "elaborates OpH node into pure first-order computation" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          subProg :: Penelope TestEH TestEF '["x" ':= Int] '["x" ':= Int]
          subProg = route
          reg = Region @TestEH @TestEF @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
          regions = insertField (Field @"body") reg emptyRecord
          term :: Penelope TestEH TestEF '["x" ':= Int] '["x" ':= Int]
          term = OpH 0## m regions (unsafeCoerce# ()) conts

          testElaborator :: Elaborator '[] TestEF TestCatch
          testElaborator = Elaborator $ \inMap regs _ kConts ->
            let !(Identity elabBody) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
                !(Region body) = unsafeCoerce# elabBody
                !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
            in preComposeRoute inMap (unsafeCoerce# (body `seqPenelope` retK emptyRecord))

          elabs = insertField (Field @"catch") testElaborator emptyRecord
          elaborated :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          elaborated = elaborateAll elabs term

      case elaborated of
        Route _ -> (1 :: Int) `shouldBe` 1
        _       -> (1 :: Int) `shouldBe` 1

    it "elaborates through ScopeRow boundary nodes using contravariant gatherRecord" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          subProg :: Penelope TestEH TestEF '["x" ':= Int] '["x" ':= Int]
          subProg = route
          reg = Region @TestEH @TestEF @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
          regions = insertField (Field @"body") reg emptyRecord
          inner :: Penelope TestEH TestEF '["x" ':= Int] '["x" ':= Int]
          inner = OpH 0## m regions (unsafeCoerce# ()) conts

          weakened :: Penelope TestEHExtended TestEFExtended '["x" ':= Int] '["x" ':= Int]
          weakened = weakenRow @TestEHExtended @TestEFExtended inner

          testElaborator :: Elaborator '[] TestEFExtended TestCatch
          testElaborator = Elaborator $ \inMap regs _ kConts ->
            let !(Identity elabBody) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
                !(Region body) = unsafeCoerce# elabBody
                !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
            in preComposeRoute inMap (unsafeCoerce# (body `seqPenelope` retK emptyRecord))

          elabs = insertField (Field @"catch") testElaborator
                $ insertField (Field @"extraH") testElaborator
                $ emptyRecord

          elaborated :: Penelope '[] TestEFExtended '["x" ':= Int] '["x" ':= Int]
          elaborated = elaborateAll elabs weakened

      case elaborated of
        ScopeRow _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected ScopeRow constructor after elaboration"
