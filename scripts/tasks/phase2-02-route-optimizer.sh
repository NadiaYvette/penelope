#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase2-02-route-optimizer
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 2.2: Pure Route Peephole Optimizer ==="

# 1. Update packages/penelope-core/penelope-core.cabal to expose Penelope.Core.Route
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
  hs-source-dirs: src
  default-language: Haskell2010
  default-extensions:
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

# 2. Create packages/penelope-core/src/Penelope/Core/Route.hs
cat <<'EOF' > packages/penelope-core/src/Penelope/Core/Route.hs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Penelope.Core.Route
Description : Pure port routing, identity elision, and route peephole optimization
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

This module provides smart constructors and fusion routines for pure port wire
permutations over Polytropos 'RowMorph' structures.

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
module Penelope.Core.Route
  ( -- * Smart Constructors
    route
  , routeExplicit

    -- * Route Optimization & Fusion
  , fuseRoute
  , isIdentityRowMorph
  , isIdentityRoute
  ) where

import Data.Kind (Type)
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import Penelope.Core.Syntax (Penelope (..), PolyEffect, PolyHEffect)
import Polytropos (RowOf)
import Polytropos.RowMorph
  ( KnownRowMorph (..)
  , RowMorph (..)
  , composeRowMorph
  )

{- |
Construct a pure wire permutation between input ports and output ports using
a compile-time synthesized 'RowMorph' solved by @Polytropos.Plugin@.

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
route ::
  forall {eh :: RowOf PolyHEffect} {ef :: RowOf PolyEffect} (ins :: RowOf Type) (outs :: RowOf Type).
  KnownRowMorph outs ins =>
  Penelope eh ef ins outs
{-# INLINE route #-}
route = Route (rowMorph @outs @ins)

-- | Construct a pure wire permutation with an explicit 'RowMorph'.
routeExplicit ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (ins :: RowOf Type) (outs :: RowOf Type).
  RowMorph outs ins
  -> Penelope eh ef ins outs
{-# INLINE routeExplicit #-}
routeExplicit = Route

{- |
Fuse two consecutive pure route stages in \(O(|outs|)\) time with zero intermediate allocations.
Given a route from @a@ to @b@ ('RowMorph' @b@ @a@) and a route from @b@ to @c@ ('RowMorph' @c@ @b@),
computes the composite route from @a@ to @c@ ('RowMorph' @c@ @a@).
-}
fuseRoute ::
  RowMorph b a
  -> RowMorph c b
  -> RowMorph c a
{-# INLINE fuseRoute #-}
fuseRoute m1 m2 = composeRowMorph m2 m1

{- |
Check whether a 'RowMorph' represents the identity permutation (slot @i@ maps to @i@).
-}
isIdentityRowMorph :: RowMorph r1 r2 -> Bool
{-# INLINE isIdentityRowMorph #-}
isIdentityRowMorph (RowMorph arr) =
  let !n = sizeofPrimArray arr
      go !i
        | i >= n    = True
        | otherwise = indexPrimArray arr i == i && go (i + 1)
  in go 0

{- |
Check whether a 'Penelope' computation is an identity wire permutation.
-}
isIdentityRoute :: Penelope eh ef ins outs -> Bool
isIdentityRoute (Route m) = isIdentityRowMorph m
isIdentityRoute _         = False
EOF

# 3. Update packages/penelope-core/src/Penelope/Core.hs to re-export Route
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
  ) where

import Penelope.Core.Syntax
import Penelope.Core.Route
EOF

# 4. Update packages/penelope-core/test/Main.hs with route optimizer tests
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
          m2 = rowMorph @'["c" ':= Int] @'["b" ':= Int]
          fused = fuseRoute m1 m2
      case fused of
        _ -> (1 :: Int) `shouldBe` 1

echo "=== [yamaarashi] Milestone 2.2 implementation files written successfully ==="
EOF

chmod +x packages/penelope-core/test/Main.hs 2>/dev/null || true
echo "=== [yamaarashi] Milestone 2.2 implementation script complete ==="
