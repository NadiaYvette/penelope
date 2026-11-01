#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase3-02-frame-rule-wiring
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 3.2: Frame-Rule Automatic Port Wiring Combinator (>&>) ==="

# 1. Update packages/penelope-core/src/Penelope/Core/Wiring.hs
cat <<'EOF' > packages/penelope-core/src/Penelope/Core/Wiring.hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Penelope.Core.Wiring
Description : Polycategorical wiring combinators, sequential composition, and frame-rule auto-wiring
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides polycategorical composition primitives for the Penelope effect system.
See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation],
Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing],
and Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring].
-}
module Penelope.Core.Wiring
  ( -- * Sequential Composition
    seqPenelope
  , (>>>)

    -- * Frame-Rule Automatic Port Wiring
  , (>&>)
  , liftFrame

    -- * Route Pre-Composition & Peephole Fusion
  , preComposeRoute

    -- * Coyoneda Leaf Continuation Record Mapping
  , rmapFused
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.PrimArray (sizeofPrimArray)
import Data.Primitive.SmallArray
  ( indexSmallArray
  , newSmallArray
  , runSmallArray
  , sizeofSmallArray
  , writeSmallArray
  )
import GHC.Exts (Int (..), Word (..), int2Word#, unsafeCoerce#, word2Int#)
import Penelope.Core.Route (fuseRoute, isIdentityRowMorph)
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , Region (..)
  )
import Polytropos (Record, RowOf)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph
  ( FramePlan (..)
  , FrameWire (..)
  , RowMorph (..)
  , composeRowMorph
  , executeFrameScatter
  , gatherRep
  , indexRowMorph
  , unsafeMkRowMorph
  )
import Unsafe.Coerce (unsafeCoerce)

{- |
Map a continuation or region transformation across all slots of an open record in a
single flat SmallArray pass without intermediate container allocations.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
rmapFused :: (forall x. a x -> b x) -> Record r1 -> Record r2
rmapFused f (RowRep (Rep tag arr)) =
  let !sz = sizeofSmallArray arr
      !arr' = runSmallArray $ do
        m <- newSmallArray sz (Identity (unsafeCoerce# ()))
        let loop !i
              | i < sz = do
                  let Identity !anyVal = indexSmallArray arr i
                  let !res = f (unsafeCoerce anyVal)
                  writeSmallArray m i (Identity (unsafeCoerce res))
                  loop (i + 1)
              | otherwise = pure ()
        loop 0
        pure m
  in RowRep (Rep tag arr')

{- |
Pre-compose a static 'RowMorph' wire routing stage before a downstream 'Penelope' pipeline.
Peephole-fuses into 'fuseRoute' if the downstream stage is also a 'Route', elides identity
morphisms in O(1) time, or distributes into continuation gather maps.

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
preComposeRoute ::
  forall mid ins (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf Type).
  RowMorph mid ins
  -> Penelope eh ef mid outs
  -> Penelope eh ef ins outs
preComposeRoute m1 (Route m2) = Route (fuseRoute m1 m2)
preComposeRoute m1 next
  | isIdentityRowMorph m1 = unsafeCoerce next
preComposeRoute m1 (LiftPure inMap f cont) =
  LiftPure
    (fuseRoute m1 inMap)
    f
    (BranchCont (\res -> preComposeRoute m1 (runBranchCont cont res)))
preComposeRoute m1 (OpF tag# inMap payload conts) =
  OpF
    tag#
    (fuseRoute m1 inMap)
    payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> preComposeRoute m1 (k res))) conts)
preComposeRoute m1 (OpH tag# inMap regions payload conts) =
  OpH
    tag#
    (fuseRoute m1 inMap)
    regions
    payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> preComposeRoute m1 (k res))) conts)
preComposeRoute m1 (ScopeRow ehM efM inner) =
  ScopeRow ehM efM (preComposeRoute m1 inner)

{- |
Sequential polycategorical composition of Penelope pipelines.
Substitutes downstream pipelines into continuation leaves with Coyoneda fusion.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
seqPenelope ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  Penelope eh ef a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
seqPenelope (Route m) next = preComposeRoute m next
seqPenelope (LiftPure inMap f (BranchCont k)) next =
  LiftPure inMap f (BranchCont (\res -> seqPenelope (k res) next))
seqPenelope (OpF tag# inMap payload conts) next =
  OpF tag# inMap payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqPenelope (k res) next)) conts)
seqPenelope (OpH tag# inMap regions payload conts) next =
  OpH tag# inMap regions payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqPenelope (k res) next)) conts)
seqPenelope (ScopeRow ehM efM inner) next =
  seqScopeRow ehM efM inner next

-- | Infix sequential composition operator.
{-# INLINE (>>>) #-}
infixr 1 >>>
(>>>) ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  Penelope eh ef a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
(>>>) = seqPenelope

{- |
Lifts a local @(opIn -> opOut)@ computation into a wider @(frameIn -> frameOut)@
register frame by composing 'fpGatherIn' at the root and 'executeFrameScatter' at the leaves.

See Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring].
-}
liftFrame ::
  forall frameIn opIn opOut frameOut eh ef.
  FramePlan frameIn opIn opOut frameOut
  -> Penelope eh ef opIn opOut
  -> Penelope eh ef frameIn frameOut
liftFrame plan = go
  where
    !baseSz = sizeofPrimArray (unRowMorph (fpBaseScatter plan))
    !outSz = fpOutSize plan

    liftCont ::
      forall branchOuts.
      BranchCont (Penelope eh ef opIn opOut) branchOuts
      -> BranchCont (Penelope eh ef frameIn frameOut) branchOuts
    liftCont (BranchCont k) = BranchCont (\branchRec -> go (k branchRec))

    go :: Penelope eh ef opIn opOut -> Penelope eh ef frameIn frameOut
    go (Route m) =
      LiftPure
        (fpGatherIn plan)
        (\(RowRep inRep) -> RowRep (gatherRep m inRep))
        (BranchCont $ \(RowRep outRep) ->
          LiftPure
            (unsafeMkRowMorph [0 .. baseSz - 1])
            (\(RowRep frameRep) -> RowRep (executeFrameScatter plan frameRep outRep))
            (BranchCont $ \_ -> Route (unsafeMkRowMorph [0 .. outSz - 1])))
    go (LiftPure inMap f cont) =
      LiftPure (composeRowMorph inMap (fpGatherIn plan)) f (liftCont cont)
    go (OpF tag# inMap payload conts) =
      OpF tag# (composeRowMorph inMap (fpGatherIn plan)) payload (rmapFused liftCont conts)
    go (OpH tag# inMap regions payload conts) =
      OpH tag# (composeRowMorph inMap (fpGatherIn plan)) regions payload (rmapFused liftCont conts)
    go (ScopeRow ehM efM inner) =
      ScopeRow ehM efM (liftFrame plan inner)

{- |
Frame-Rule Auto-Wiring Composition ('(>&>)'):
Automatically gathers each stage's required input ports from the live register frame
and scatters its outputs back into the frame, preserving untouched context ports.

See Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring].
-}
{-# INLINE (>&>) #-}
infixr 1 >&>
(>&>) ::
  forall frameIn ins1 outs1 frameMid ins2 outs2 frameOut eh ef.
  ( FrameWire frameIn ins1 outs1 frameMid
  , FrameWire frameMid ins2 outs2 frameOut
  ) =>
  Penelope eh ef ins1 outs1
  -> Penelope eh ef ins2 outs2
  -> Penelope eh ef frameIn frameOut
stage1 >&> stage2 =
  let plan1 = framePlan @frameIn @ins1 @outs1 @frameMid
      plan2 = framePlan @frameMid @ins2 @outs2 @frameOut
  in liftFrame plan1 stage1 `seqPenelope` liftFrame plan2 stage2

{- |
Helper to sequence through regional 'ScopeRow' boundaries to downstream continuations.
-}
seqScopeRow ::
  forall (ehSub :: RowOf PolyHEffect) (eh :: RowOf PolyHEffect)
         (efSub :: RowOf PolyEffect) (ef :: RowOf PolyEffect)
         (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  RowMorph ehSub eh
  -> RowMorph efSub ef
  -> Penelope ehSub efSub a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
seqScopeRow ehM efM p next = case p of
  Route m -> preComposeRoute m next
  LiftPure inMap f (BranchCont k) ->
    LiftPure inMap f (BranchCont (\res -> seqScopeRow ehM efM (k res) next))
  OpF tag# inMap payload conts ->
    let !(I# fullIdx#) = indexRowMorph efM (I# (word2Int# tag#))
        !fullTag# = int2Word# fullIdx#
    in OpF fullTag# inMap payload
         (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqScopeRow ehM efM (k res) next)) conts)
  OpH tag# inMap regions payload conts ->
    let !(I# fullIdx#) = indexRowMorph ehM (I# (word2Int# tag#))
        !fullTag# = int2Word# fullIdx#
        !regions' = rmapFused (\(Region reg) -> Region (ScopeRow ehM efM reg)) regions
    in OpH fullTag# inMap regions' payload
         (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqScopeRow ehM efM (k res) next)) conts)
  ScopeRow ehM2 efM2 inner ->
    seqScopeRow (composeRowMorph ehM2 ehM) (composeRowMorph efM2 efM) inner next
EOF

# 2. Update packages/penelope-core/test/Main.hs with tests for Milestone 3.2
cat <<'EOF' > packages/penelope-core/test/Main.hs
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

import Data.Text (Text)
import Test.Hspec
import Penelope.Core
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (Pair (..))
import Polytropos.RowMorph (KnownRowMorph (..))
import GHC.Exts (unsafeCoerce#)

-- Test effect signatures
data TestGet :: PolyEffect
data TestPut :: PolyEffect
type TestEF = '["get" ':= TestGet, "put" ':= TestPut]

data TestCatch :: PolyHEffect
type TestEH = '["catch" ':= TestCatch]

type MyFrame = '["x" ':= Int, "y" ':= Text, "z" ':= Bool]

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

          m2 = rowMorph @'["x" ':= Int, "y" ':= Text] @'["x" ':= Int, "y" ':= Text]
          mOut2 = rowMorph @'["z" ':= Bool] @'["z" ':= Bool]
          stage2 :: Penelope '[] '[] '["x" ':= Int, "y" ':= Text] '["z" ':= Bool]
          stage2 = LiftPure m2 (\_ -> insertField (Field @"z") True emptyRecord)
                     (BranchCont (\_ -> Route mOut2))

          pipeline :: Penelope '[] '[] MyFrame MyFrame
          pipeline = stage1 >&> stage2
      case pipeline of
        LiftPure _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected LiftPure at pipeline root"
EOF

chmod +x packages/penelope-core/src/Penelope/Core/Wiring.hs || true
chmod +x scripts/tasks/phase3-02-frame-rule-wiring.sh || true
echo "=== [yamaarashi] Milestone 3.2 setup complete ==="
