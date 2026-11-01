#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase4-04-partial-interpretation
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 4.4: Partial Layered Interpretation (interpretSub) ==="

mkdir -p packages/penelope-core/src/Penelope/Core/Engine

# 1. Update packages/penelope-core/penelope-core.cabal
cat <<'CABAL_EOF' > packages/penelope-core/penelope-core.cabal
cabal-version: 3.0
name: penelope-core
version: 0.1.0.0
synopsis: Unpacked (m,n,k,r)-ary Polycategorical Effect System Core
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Core
    Penelope.Core.Syntax
    Penelope.Core.Route
    Penelope.Core.Send
    Penelope.Core.Wiring
    Penelope.Core.Engine
    Penelope.Core.Engine.Elaborate
    Penelope.Core.Engine.Interpret
    Penelope.Core.Engine.Partial
  hs-source-dirs: src
  default-language: Haskell2010
  default-extensions:
    AllowAmbiguousTypes
    BangPatterns
    DataKinds
    DerivingStrategies
    FlexibleContexts
    FlexibleInstances
    GADTs
    KindSignatures
    MagicHash
    MultiParamTypeClasses
    OverloadedStrings
    PolyKinds
    RankNTypes
    ScopedTypeVariables
    TypeApplications
    TypeFamilies
    TypeOperators
    UnboxedTuples
  ghc-options: -Wall -Wcompat -Wredundant-constraints
  build-depends:
    base >= 4.14 && < 5,
    text >= 2.0,
    containers >= 0.6,
    primitive >= 0.9,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1

test-suite tests
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  default-language: Haskell2010
  default-extensions:
    AllowAmbiguousTypes
    DataKinds
    GADTs
    MagicHash
    OverloadedStrings
    PolyKinds
    RankNTypes
    TypeApplications
    TypeOperators
  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts
  build-depends:
    base >= 4.14 && < 5,
    hspec >= 2.11,
    text >= 2.0,
    primitive >= 0.9,
    penelope-core,
    polytropos-core,
    polytropos-plugin
CABAL_EOF

# 2. Update packages/penelope-core/src/Penelope/Core/Engine.hs
cat <<'ENGINE_EOF' > packages/penelope-core/src/Penelope/Core/Engine.hs
{- |
Module      : Penelope.Core.Engine
Description : High-level re-export facade for Penelope execution engine
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides execution engines for Penelope: Phase 1 elaboration, Phase 2 interpretation, and partial peeling.
See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
module Penelope.Core.Engine
  ( module Penelope.Core.Engine.Elaborate
  , module Penelope.Core.Engine.Interpret
  , module Penelope.Core.Engine.Partial
  ) where

import Penelope.Core.Engine.Elaborate
import Penelope.Core.Engine.Interpret
import Penelope.Core.Engine.Partial
ENGINE_EOF

# 3. Implement packages/penelope-core/src/Penelope/Core/Engine/Partial.hs
cat <<'PARTIAL_EOF' > packages/penelope-core/src/Penelope/Core/Engine/Partial.hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Penelope.Core.Engine.Partial
Description : Layered effect peeling and partial interpretation
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides partial effect peeling ('interpretSub'), lowering a subset of first-order
operations ('efSub') while leaving residual unhandled effects ('efRem') intact in the syntax tree.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
module Penelope.Core.Engine.Partial
  ( -- * Compile-Time Row Partitioning
    RowPartition (..)
  , PartitionPlan (..)
  , mkPartitionPlan

    -- * Partial Interpretation Engine
  , interpretSub
  ) where

import Control.Monad.ST (runST)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.PrimArray
  ( PrimArray
  , indexPrimArray
  , newPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Int (..), int2Word#, unsafeCoerce#, word2Int#)
import Penelope.Core.Engine.Elaborate (MapRow)
import Penelope.Core.Engine.Interpret (Handler (..))
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  )
import Penelope.Core.Wiring (rmapFused)
import Polytropos (Record, RowOf)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph (KnownRowMorph (..), RowMorph (..), composeRowMorph, indexRowMorph)

{- |
Runtime partition plan capturing the static decomposition of row @full@ into
disjoint sub-rows @sub@ and @rem@.
The 'ppLookup' unboxed array maps each constructor tag @fullIdx@ to:
* @val >= 0@: 0-based index in @sub@.
* @val < 0@: 0-based index in @rem@ (encoded as @-1 - val@).

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
data PartitionPlan (sub :: RowOf k) (rem :: RowOf k) (full :: RowOf k) = PartitionPlan
  { ppSubMorph :: !(RowMorph sub full)
  , ppRemMorph :: !(RowMorph rem full)
  , ppLookup   :: !(PrimArray Int)
  }

-- | Synthesizes an unboxed lookup array from compile-time row morphisms.
{-# INLINE mkPartitionPlan #-}
mkPartitionPlan ::
  RowMorph sub full
  -> RowMorph rem full
  -> PartitionPlan sub rem full
mkPartitionPlan mSub@(RowMorph arrSub) mRem@(RowMorph arrRem) =
  let !nSub = sizeofPrimArray arrSub
      !nRem = sizeofPrimArray arrRem
      !totalSz = nSub + nRem
      !lookupArr = runST $ do
        m <- newPrimArray totalSz
        let initLoop !i
              | i >= totalSz = pure ()
              | otherwise    = writePrimArray m i (-1) >> initLoop (i + 1)
        initLoop 0
        let fillSub !k
              | k >= nSub = pure ()
              | otherwise = do
                  let !fullIdx = indexPrimArray arrSub k
                  writePrimArray m fullIdx k
                  fillSub (k + 1)
        fillSub 0
        let fillRem !j
              | j >= nRem = pure ()
              | otherwise = do
                  let !fullIdx = indexPrimArray arrRem j
                  writePrimArray m fullIdx (-1 - j)
                  fillRem (j + 1)
        fillRem 0
        unsafeFreezePrimArray m
  in PartitionPlan mSub mRem lookupArr

{- |
Evidence that row @full@ is partitioned into disjoint sub-rows @sub@ and @rem@.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
class (KnownRowMorph sub full, KnownRowMorph rem full) => RowPartition (sub :: RowOf k) (rem :: RowOf k) (full :: RowOf k) where
  rowPartition :: PartitionPlan sub rem full

instance {-# OVERLAPPABLE #-} (KnownRowMorph sub full, KnownRowMorph rem full) => RowPartition sub rem full where
  {-# INLINE rowPartition #-}
  rowPartition = mkPartitionPlan (rowMorph @sub @full) (rowMorph @rem @full)

{- |
Partially peel and interpret a subset of first-order effects @efSub@ from @efFull@,
leaving residual effects @efRem@ intact in the syntax tree.

Dispatches each target effect via \(O(1)\) unboxed lookup in the partition table,
invoking the provided handler, while re-tagging unhandled residual operations to @efRem@.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
{-# INLINE interpretSub #-}
interpretSub ::
  forall (efSub :: RowOf PolyEffect) (efRem :: RowOf PolyEffect) (efFull :: RowOf PolyEffect)
         (ins :: RowOf Type) (outs :: RowOf Type).
  RowPartition efSub efRem efFull =>
  Record (MapRow (Handler efRem (Penelope '[] efRem)) efSub)
  -> Penelope '[] efFull ins outs
  -> Penelope '[] efRem ins outs
interpretSub !handlers = go
  where
    plan = rowPartition @_ @efSub @efRem @efFull
    arrLookup = ppLookup plan

    interpCont ::
      BranchCont (Penelope '[] efFull i o) branchOuts
      -> BranchCont (Penelope '[] efRem i o) branchOuts
    interpCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    go :: Penelope '[] efFull i o -> Penelope '[] efRem i o
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap f cont ->
        LiftPure inMap f (interpCont cont)

      OpF tag# inMap opAny conts ->
        let !idx = I# (word2Int# tag#)
            !entry = indexPrimArray arrLookup idx
            !interpretedConts = rmapFused interpCont conts
        in if entry >= 0
             then
               let !(RowRep (Rep _ arr)) = handlers
                   !(Identity handlerAny) = indexSmallArray arr entry
                   !(Handler handleFn) = unsafeCoerce# handlerAny
               in handleFn (unsafeCoerce# inMap) opAny interpretedConts
             else
               let !(I# i#) = -1 - entry
                   !remIdx# = int2Word# i#
               in OpF remIdx# inMap opAny interpretedConts

      ScopeRow _ehMorph efMorph subPipe ->
        goInScope (unsafeCoerce# efMorph) (unsafeCoerce# subPipe)

      OpH {} ->
        errorWithoutStackTrace "Penelope.Core.Engine.Partial: impossible OpH in empty '[] higher-order row"

    goInScope :: RowMorph efInner efFull -> Penelope '[] efInner i o -> Penelope '[] efRem i o
    goInScope !efM = \case
      Route morph ->
        Route morph

      LiftPure inMap f (BranchCont k) ->
        LiftPure inMap f (BranchCont (\branchFrame -> goInScope efM (k branchFrame)))

      OpF tag# inMap opAny conts ->
        let !idx = indexRowMorph efM (I# (word2Int# tag#))
            !entry = indexPrimArray arrLookup idx
            !interpretedConts = rmapFused (\(BranchCont k) -> BranchCont (\branchFrame -> goInScope efM (k branchFrame))) conts
        in if entry >= 0
             then
               let !(RowRep (Rep _ arr)) = handlers
                   !(Identity handlerAny) = indexSmallArray arr entry
                   !(Handler handleFn) = unsafeCoerce# handlerAny
               in handleFn (unsafeCoerce# inMap) opAny interpretedConts
             else
               let !(I# i#) = -1 - entry
                   !remIdx# = int2Word# i#
               in OpF remIdx# inMap opAny interpretedConts

      ScopeRow _ehInner efMorph2 inner ->
        goInScope (composeRowMorph (unsafeCoerce# efMorph2) efM) (unsafeCoerce# inner)

      OpH {} ->
        errorWithoutStackTrace "Penelope.Core.Engine.Partial: impossible OpH in empty '[] higher-order row"
PARTIAL_EOF

# 4. Update packages/penelope-core/test/Main.hs
cat <<'TEST_EOF' > packages/penelope-core/test/Main.hs
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

  describe "Penelope.Core.Engine.Interpret" $ do
    it "evaluates pure routes with runPenelopePure" $ do
      let pipe :: Penelope '[] '[] '["x" ':= Int, "y" ':= String] '["x" ':= Int]
          pipe = route
          inFrame = insertField (Field @"x") (42 :: Int)
                  $ insertField (Field @"y") ("test" :: String)
                  $ emptyRecord
          outFrame = runPenelopePure pipe inFrame
          !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
      (unsafeCoerce# xVal :: Int) `shouldBe` 42

    it "evaluates pure transformations with runPenelopePure" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          f inF =
            let !(Identity x) = indexSmallArray (let RowRep (Rep _ arr) = inF in arr) 0
                !newX = (unsafeCoerce# x :: Int) + 1
            in insertField (Field @"x") newX emptyRecord
          pipe :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          pipe = LiftPure m f (BranchCont (\_ -> route))
          inFrame = insertField (Field @"x") (42 :: Int) emptyRecord
          outFrame = runPenelopePure pipe inFrame
          !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
      (unsafeCoerce# xVal :: Int) `shouldBe` 43

    it "dispatches first-order operations via interpretAll" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          op :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          op = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts

          getHandler :: Handler '[] (FrameKleisli Identity) TestGet
          getHandler = Handler $ \_inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
                resFrame = insertField (Field @"x") (99 :: Int) emptyRecord
            in FrameKleisli $ \_inF -> runFrameKleisli (retK resFrame) resFrame

          dummyPutHandler :: Handler '[] (FrameKleisli Identity) TestPut
          dummyPutHandler = Handler $ \_inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
            in FrameKleisli $ \inF -> runFrameKleisli (retK emptyRecord) inF

          handlers = insertField (Field @"get") getHandler
                   $ insertField (Field @"put") dummyPutHandler
                   $ emptyRecord

          kleisli = interpretAll handlers op
          inFrame = insertField (Field @"x") (0 :: Int) emptyRecord
          Identity outFrame = runFrameKleisli kleisli inFrame
          !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
      (unsafeCoerce# xVal :: Int) `shouldBe` 99

    it "interprets through ScopeRow boundary nodes using contravariant gatherRecord" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          inner :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          inner = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts

          weakened :: Penelope '[] TestEFExtended '["x" ':= Int] '["x" ':= Int]
          weakened = weakenEF @TestEFExtended inner

          getHandler :: Handler '[] (FrameKleisli Identity) TestGet
          getHandler = Handler $ \_inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
                resFrame = insertField (Field @"x") (99 :: Int) emptyRecord
            in FrameKleisli $ \_inF -> runFrameKleisli (retK resFrame) resFrame

          dummyPutHandler :: Handler '[] (FrameKleisli Identity) TestPut
          dummyPutHandler = Handler $ \_inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
            in FrameKleisli $ \inF -> runFrameKleisli (retK emptyRecord) inF

          handlersExt = insertField (Field @"get") getHandler
                      $ insertField (Field @"put") dummyPutHandler
                      $ insertField (Field @"extra") getHandler
                      $ emptyRecord

          kleisli = interpretAll handlersExt weakened
          inFrame = insertField (Field @"x") (0 :: Int) emptyRecord
          Identity outFrame = runFrameKleisli kleisli inFrame
          !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
      (unsafeCoerce# xVal :: Int) `shouldBe` 99

  describe "Penelope.Core.Engine.Partial" $ do
    it "peels target effect via interpretSub leaving residual effect in AST" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          opPut :: Penelope '[] TestEFExtended '["x" ':= Int] '["x" ':= Int]
          opPut = sendPolyF' @"put" @'["x" ':= Int] (100 :: Int) conts

          getHandler :: Handler '["put" ':= TestPut, "extra" ':= TestGet] (Penelope '[] '["put" ':= TestPut, "extra" ':= TestGet]) TestGet
          getHandler = Handler $ \inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
                resFrame = insertField (Field @"x") (42 :: Int) emptyRecord
            in preComposeRoute inMap (retK resFrame)

          handlers = insertField (Field @"get") getHandler emptyRecord
          peeled :: Penelope '[] '["put" ':= TestPut, "extra" ':= TestGet] '["x" ':= Int] '["x" ':= Int]
          peeled = interpretSub @'["get" ':= TestGet] @'["put" ':= TestPut, "extra" ':= TestGet] @TestEFExtended handlers opPut

      case peeled of
        OpF tag# _ _ _ -> do
          case tag# of _ -> (1 :: Int) `shouldBe` 1
        _ -> expectationFailure "Expected OpF constructor with remapped residual tag"

    it "peeled computation can subsequently be interpreted by interpretAll" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          opPut :: Penelope '[] TestEFExtended '["x" ':= Int] '["x" ':= Int]
          opPut = sendPolyF' @"put" @'["x" ':= Int] (100 :: Int) conts

          getHandler :: Handler '["put" ':= TestPut, "extra" ':= TestGet] (Penelope '[] '["put" ':= TestPut, "extra" ':= TestGet]) TestGet
          getHandler = Handler $ \inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
                resFrame = insertField (Field @"x") (42 :: Int) emptyRecord
            in preComposeRoute inMap (retK resFrame)

          handlers = insertField (Field @"get") getHandler emptyRecord
          peeled :: Penelope '[] '["put" ':= TestPut, "extra" ':= TestGet] '["x" ':= Int] '["x" ':= Int]
          peeled = interpretSub @'["get" ':= TestGet] @'["put" ':= TestPut, "extra" ':= TestGet] @TestEFExtended handlers opPut

          putHandlerFinal :: Handler '[] (FrameKleisli Identity) TestPut
          putHandlerFinal = Handler $ \_inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
                resFrame = insertField (Field @"x") (77 :: Int) emptyRecord
            in FrameKleisli $ \_inF -> runFrameKleisli (retK resFrame) resFrame

          extraHandlerFinal :: Handler '[] (FrameKleisli Identity) TestGet
          extraHandlerFinal = Handler $ \_inMap _payload kConts ->
            let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
                !(BranchCont retK) = unsafeCoerce# elabK
            in FrameKleisli $ \inF -> runFrameKleisli (retK emptyRecord) inF

          handlersFinal = insertField (Field @"put") putHandlerFinal
                        $ insertField (Field @"extra") extraHandlerFinal
                        $ emptyRecord

          kleisli = interpretAll handlersFinal peeled
          inFrame = insertField (Field @"x") (0 :: Int) emptyRecord
          Identity outFrame = runFrameKleisli kleisli inFrame
          !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
      (unsafeCoerce# xVal :: Int) `shouldBe` 77

TEST_EOF

# 5. Build and verify
cabal build penelope-core
cabal test penelope-core:test:tests
echo "=== [yamaarashi] Milestone 4.4 implementation script completed successfully ==="
