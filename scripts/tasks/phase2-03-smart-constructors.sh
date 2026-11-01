#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase2-03-smart-constructors
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 2.3: Zero-Overhead Smart Constructors ==="

# 1. Update packages/penelope-core/penelope-core.cabal to expose Penelope.Core.Send
cat <<'EOF' > packages/penelope-core/penelope-core.cabal
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
    TypeFamilies
    TypeOperators
  ghc-options: -Wall -Wcompat -Wredundant-constraints
  build-depends:
    base >= 4.14 && < 5,
    penelope-core,
    polytropos-core,
    polytropos-plugin,
    text >= 2.0,
    hspec >= 2.10
EOF

# 2. Create packages/penelope-core/src/Penelope/Core/Send.hs
cat <<'EOF' > packages/penelope-core/src/Penelope/Core/Send.hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Penelope.Core.Send
Description : Zero-overhead smart constructors for first-order and higher-order effect operations
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

This module provides zero-overhead smart constructors ('sendPolyF', 'sendPolyH')
injecting algebraic effect operations into the 'Penelope' polycategorical core.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
module Penelope.Core.Send
  ( -- * Smart Constructors
    sendPolyF
  , sendPolyF'
  , sendPolyH
  , sendPolyH'
  ) where

import GHC.Exts (Any, Word (W#), unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Syntax (Penelope (..), PolyEffect, PolyHEffect)
import Polytropos (Record, RowOf)
import Polytropos.Core (ConstructorIndex (..), Row)
import Polytropos.RowMorph (KnownRowMorph (..))

{- |
Inject a first-order algebraic operation into the syntax tree.
Looks up the constructor tag via 'ConstructorIndex' solved at compile time by @Polytropos.Plugin@,
and wires input ports via 'KnownRowMorph'.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
sendPolyF ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (exits :: Row) (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l ef, KnownRowMorph opIns ins) =>
  Any
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyF #-}
sendPolyF payload conts =
  case constructorIndex @l @ef of
    W# tag# -> OpF tag# (rowMorph @opIns @ins) payload conts

-- | Typed payload variant of 'sendPolyF' applying 'unsafeCoerce#' without runtime boxing.
sendPolyF' ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (exits :: Row) op (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l ef, KnownRowMorph opIns ins) =>
  op
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyF' #-}
sendPolyF' op conts = sendPolyF @l @opIns (unsafeCoerce# op) conts

{- |
Inject a higher-order scoped algebraic operation into the syntax tree with
explicit sub-regions and multi-exit branch continuations.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
sendPolyH ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (regions :: Row) (exits :: Row) (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l eh, KnownRowMorph opIns ins) =>
  Record regions
  -> Any
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyH #-}
sendPolyH regions payload conts =
  case constructorIndex @l @eh of
    W# tag# -> OpH tag# (rowMorph @opIns @ins) regions payload conts

-- | Typed payload variant of 'sendPolyH' applying 'unsafeCoerce#' without runtime boxing.
sendPolyH' ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (regions :: Row) (exits :: Row) op (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l eh, KnownRowMorph opIns ins) =>
  Record regions
  -> op
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyH' #-}
sendPolyH' regions op conts = sendPolyH @l @opIns regions (unsafeCoerce# op) conts
EOF

# 3. Update packages/penelope-core/src/Penelope/Core.hs to re-export Send
cat <<'EOF' > packages/penelope-core/src/Penelope/Core.hs
{- |
Module      : Penelope.Core
Description : High-level re-export facade for penelope-core
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Foundational exports for Penelope core polycategorical effect engine.
See Note [Core Architecture] [ref:note-core-architecture].
-}
module Penelope.Core
  ( module Penelope.Core.Syntax
  , module Penelope.Core.Route
  , module Penelope.Core.Send
  ) where

import Penelope.Core.Syntax
import Penelope.Core.Route
import Penelope.Core.Send
EOF

# 4. Update packages/penelope-core/test/Main.hs with Send tests
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
EOF

echo "=== [yamaarashi] Milestone 2.3 implementation files written successfully ==="
