#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase2-01-syntax-gadt
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 2.1: Type-Level Signatures & Core Syntax GADT ==="

# 1. Update packages/penelope-core/penelope-core.cabal
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

# 2. Create packages/penelope-core/src/Penelope/Core/Syntax.hs
mkdir -p packages/penelope-core/src/Penelope/Core
cat <<'EOF' > packages/penelope-core/src/Penelope/Core/Syntax.hs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Penelope.Core.Syntax
Description : Foundational (m,n,k,r)-ary polycategorical syntax GADT and type-level signatures
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

This module implements the foundational syntax GADT for Penelope's
$(m,n,k,r)$-ary polycategorical effect engine.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
module Penelope.Core.Syntax
  ( -- * Type-Level Signatures
    PortSig (..)
  , RegionIns
  , RegionOuts
  , PolyEffect
  , PolyHEffect

    -- * Continuations and Scoped Regions
  , BranchCont (..)
  , Region (..)

    -- * Core Syntax GADT
  , Penelope (..)
  ) where

import Data.Kind (Type)
import GHC.Exts (Any, Word#)
import Polytropos (Record, RowOf)
import Polytropos.RowMorph (RowMorph)

{- |
Polycategorical port signature encoding input ports and output ports.
Used as the sort for higher-order scoped sub-regions.
-}
data PortSig = PortSig (RowOf Type) (RowOf Type)

-- | Extract the input port row from a 'PortSig'.
type family RegionIns (sig :: PortSig) :: RowOf Type where
  RegionIns ('PortSig ins _) = ins

-- | Extract the output port row from a 'PortSig'.
type family RegionOuts (sig :: PortSig) :: RowOf Type where
  RegionOuts ('PortSig _ outs) = outs

{- |
Signature of a first-order polycategorical effect operation.
Takes an input argument row and a row of disjunctive exit rows.
-}
type PolyEffect = RowOf Type -> RowOf (RowOf Type) -> Type

{- |
Signature of a higher-order scoped polycategorical effect operation.
Takes:
1. Region representation runner capability
2. Row of scoped region port signatures
3. Input argument row
4. Row of disjunctive exit rows
-}
type PolyHEffect =
  (PortSig -> Type)
  -> RowOf PortSig
  -> RowOf Type
  -> RowOf (RowOf Type)
  -> Type

{- |
Multi-exit branch continuation mapping a resolved output record into a downstream computation.
-}
newtype BranchCont m (outs :: RowOf Type) = BranchCont
  { runBranchCont :: Record outs -> m
  }

{- |
Scoped sub-region representation wrapping an inner polycategorical computation.
-}
newtype Region (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (sig :: PortSig) = Region
  { runRegion :: Penelope eh ef (RegionIns sig) (RegionOuts sig)
  }

{- |
The core (m,n,k,r)-ary polycategorical syntax GADT.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation]
for the rationale behind unboxing 'Word#' variant tags and 'Any' payloads directly
into GADT constructors.
-}
data Penelope (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (ins :: RowOf Type) (outs :: RowOf Type) where
  -- | Static wire permutation or subset extraction.
  Route ::
    !(RowMorph outs ins)
    -> Penelope eh ef ins outs

  -- | Opaque pure computational transformation with explicit return continuation.
  LiftPure ::
    !(RowMorph opIns ins)
    -> (Record opIns -> Record opOuts)
    -> !(BranchCont (Penelope eh ef ins outs) opOuts)
    -> Penelope eh ef ins outs

  -- | Unpacked first-order algebraic operation with unboxed constructor tag,
  -- input gather wiring, dynamic payload, and multi-exit branch continuations.
  OpF ::
    Word#
    -> !(RowMorph opIns ins)
    -> Any
    -> !(Record exits)
    -> Penelope eh ef ins outs

  -- | Unpacked higher-order scoped operation with unboxed constructor tag,
  -- input gather wiring, record of scoped sub-regions, dynamic payload,
  -- and multi-exit branch continuations.
  OpH ::
    Word#
    -> !(RowMorph opIns ins)
    -> !(Record regions)
    -> Any
    -> !(Record exits)
    -> Penelope eh ef ins outs

  -- | Scoped regional boundary for zero-traversal effect row weakening.
  -- See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
  ScopeRow ::
    !(RowMorph ehSub ehFull)
    -> !(RowMorph efSub efFull)
    -> !(Penelope ehSub efSub ins outs)
    -> Penelope ehFull efFull ins outs
EOF

# 3. Update packages/penelope-core/src/Penelope/Core.hs
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
  ) where

import Penelope.Core.Syntax
EOF

# 4. Update packages/penelope-core/test/Main.hs
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
EOF

echo "=== [yamaarashi] Milestone 2.1 implementation files written successfully ==="
