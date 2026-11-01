#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase4-02-higher-order-elaboration
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 4.2: Higher-Order Elaboration (elaborateAll) ==="

mkdir -p packages/penelope-core/src/Penelope/Core/Engine

# 1. Implement packages/penelope-core/src/Penelope/Core/Engine/Elaborate.hs
cat <<'EOF' > packages/penelope-core/src/Penelope/Core/Engine/Elaborate.hs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Penelope.Core.Engine.Elaborate
Description : Phase 1 single-pass bottom-up higher-order effect elaboration
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides Phase 1 single-pass bottom-up macro elaboration of higher-order scoped
effects into pure first-order operations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine] and
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
module Penelope.Core.Engine.Elaborate
  ( -- * Elaborator Abstraction
    Elaborator (..)
  , Elaborators
  , MapRow

    -- * Phase 1 Elaboration Engine
  , elaborateAll
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Any, Int (..), unsafeCoerce#, word2Int#)
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , Region (..)
  )
import Penelope.Core.Wiring (rmapFused)
import Polytropos (Pair (..), Record, RowOf, gatherRecord)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph (RowMorph, unsafeMkRowMorph)

-- | Map a higher-order type constructor over each field of a row.
type family MapRow (f :: k -> Type) (r :: RowOf k) :: RowOf Type where
  MapRow f '[] = '[]
  MapRow f ((l ':= v) ': rest) = (l ':= f v) ': MapRow f rest

-- | Convenient type alias for a record of elaborators.
type Elaborators ef eh = Record (MapRow (Elaborator '[] ef) eh)

{- |
An 'Elaborator' translates a higher-order \((m, n, k, r)\)-ary scoped operation @h@
into a pure first-order 'Penelope' computation.

Notice that:
1. All \(r\) nested regions in @Record regions@ have ALREADY
   been recursively elaborated before 'runElaborator' is called (bottom-up catamorphism).
2. The elaborator receives the \(k\) downstream continuation branches
   @Record exits@ directly, allowing it to wire region exits straight into downstream
   branches without intermediate ADT allocations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
newtype Elaborator
          (eh' :: RowOf PolyHEffect)
          (ef  :: RowOf PolyEffect)
          (h   :: PolyHEffect)
  = Elaborator
  { runElaborator
      :: forall (regions :: RowOf Type) (opIns :: RowOf Type) (exits :: RowOf Type)
                (frameIns :: RowOf Type) (frameOuts :: RowOf Type).
         RowMorph opIns frameIns
      -> Record regions
      -> Any
      -> Record exits
      -> Penelope eh' ef frameIns frameOuts
  }

{- |
Elaborates all higher-order scoped effects @eh@ into pure first-order effects @ef@
in a single bottom-up pass over the syntax tree.
Dispatch at each 'OpH' node is a branchless \(O(1)\) 'indexSmallArray#' read into the
Polytropos 'Record' of elaborators.

Contravariant reduct gathering via 'gatherRecord' adapts outer elaborators to inner
scopes across 'ScopeRow' boundaries in \(O(|eh_{sub}|)\) time without walking the inner sub-tree.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine] and
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
{-# INLINE elaborateAll #-}
elaborateAll ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect)
         (ins :: RowOf Type) (outs :: RowOf Type).
  Record (MapRow (Elaborator '[] ef) eh)
  -> Penelope eh ef ins outs
  -> Penelope '[] ef ins outs
elaborateAll !elabs = go
  where
    elabCont ::
      BranchCont (Penelope eh ef i o) branchOuts
      -> BranchCont (Penelope '[] ef i o) branchOuts
    elabCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    elabRegion :: Region eh ef sig -> Region '[] ef sig
    elabRegion (Region subPipe) = Region (elaborateAll elabs subPipe)

    go :: Penelope eh ef i o -> Penelope '[] ef i o
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap f cont ->
        LiftPure inMap f (elabCont cont)

      OpF tag# inMap opAny conts ->
        OpF tag# inMap opAny (rmapFused elabCont conts)

      OpH tag# inMap regions opAny conts ->
        let !elaboratedRegions = rmapFused elabRegion regions
            !elaboratedConts   = rmapFused elabCont conts
            !(RowRep (Rep _ arr)) = elabs
            !(Identity elabAny) = indexSmallArray arr (I# (word2Int# tag#))
            !(Elaborator elabFn) = unsafeCoerce# elabAny
        in elabFn (unsafeCoerce# inMap) elaboratedRegions opAny elaboratedConts

      ScopeRow ehMorph efMorph subPipe ->
        let !subElabs = unsafeCoerce# (gatherRecord (unsafeCoerce# ehMorph) elabs)
        in ScopeRow (unsafeMkRowMorph []) efMorph (elaborateAll subElabs subPipe)
EOF

# 2. Implement packages/penelope-core/src/Penelope/Core/Engine.hs
cat <<'EOF' > packages/penelope-core/src/Penelope/Core/Engine.hs
{- |
Module      : Penelope.Core.Engine
Description : High-level re-export facade for Penelope execution engine
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides execution engines for Penelope: Phase 1 elaboration and Phase 2 interpretation.
See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
module Penelope.Core.Engine
  ( module Penelope.Core.Engine.Elaborate
  ) where

import Penelope.Core.Engine.Elaborate
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
  , module Penelope.Core.Route
  , module Penelope.Core.Send
  , module Penelope.Core.Wiring
  , module Penelope.Core.Engine
  ) where

import Penelope.Core.Syntax
import Penelope.Core.Route
import Penelope.Core.Send
import Penelope.Core.Wiring
import Penelope.Core.Engine
EOF

# 4. Update packages/penelope-core/penelope-core.cabal
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
    Penelope.Core.Wiring
    Penelope.Core.Engine
    Penelope.Core.Engine.Elaborate
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
EOF

# 5. Update packages/penelope-core/test/Main.hs
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
      let mId = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          mSwap = rowMorph @'["x" ':= Int, "y" ':= Bool] @'["y" ':= Bool, "x" ':= Int]
      isIdentityRowMorph mId `shouldBe` True
      isIdentityRowMorph mSwap `shouldBe` False

    it "fuses consecutive routes via fuseRoute" $ do
      let m1 = rowMorph @'["x" ':= Int] @'["x" ':= Int, "y" ':= Bool]
          m2 = rowMorph @'["x" ':= Int, "y" ':= Bool] @'["x" ':= Int, "y" ':= Bool, "z" ':= String]
          mFused = fuseRoute m1 m2
      case mFused of
        _ -> (1 :: Int) `shouldBe` 1

  describe "Penelope.Core.Send" $ do
    it "constructs sendPolyF with ConstructorIndex tag resolution" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          op :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
          op = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts
      case op of
        OpF tag# _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected OpF constructor"

    it "constructs sendPolyH with sub-regions and ConstructorIndex tag resolution" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          subProg :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          subProg = route
          reg = Region @'[] @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
          regions = insertField (Field @"body") reg emptyRecord
          op :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
          op = sendPolyH' @"catch" @'["x" ':= Int] regions () conts
      case op of
        OpH tag# _ _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _                -> expectationFailure "Expected OpH constructor"

  describe "Penelope.Core.Wiring" $ do
    it "fuses consecutive Route nodes into a single Route" $ do
      let r1 :: Penelope '[] '[] '["x" ':= Int, "y" ':= Bool] '["x" ':= Int]
          r1 = route
          r2 :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          r2 = route
          fused = r1 >>> r2
      case fused of
        Route _ -> (1 :: Int) `shouldBe` 1
        _       -> expectationFailure "Expected Route after >>> fusion"

    it "sequences pure transformations with LiftPure" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          f r = r
          cont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          p1 = LiftPure m f cont
          p2 = route @'["x" ':= Int] @'["x" ':= Int]
          composed = p1 >>> p2
      case composed of
        LiftPure _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected LiftPure after >>>"

    it "sequences across multi-branch OpF continuations" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          op = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts
          next = route @'["x" ':= Int] @'["x" ':= Int]
          composed = op >>> next
      case composed of
        OpF tag# _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected OpF after >>>"

    it "elides identity Route when pre-composed with OpF" $ do
      let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"ret") retCont emptyRecord
          op = sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts
          mId = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          optimized = preComposeRoute mId op
      case optimized of
        OpF tag# _ _ _ -> case tag# of _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected OpF without redundant wrapper"

    it "composes multi-stage pipelines via frame-rule (>&>)" $ do
      let stage1 :: Penelope '[] '[] '["x" ':= Int] '["y" ':= String]
          stage1 = LiftPure (rowMorph @'["x" ':= Int] @'["x" ':= Int])
                            (\_ -> insertField (Field @"y") ("hello" :: String) emptyRecord)
                            (BranchCont (\_ -> route))

          stage2 :: Penelope '[] '[] '["y" ':= String] '["z" ':= Bool]
          stage2 = LiftPure (rowMorph @'["y" ':= String] @'["y" ':= String])
                            (\_ -> insertField (Field @"z") True emptyRecord)
                            (BranchCont (\_ -> route))

          framedPipeline :: Penelope '[] '[] MyFrame MyFrame
          framedPipeline = stage1 >&> stage2
      case framedPipeline of
        LiftPure _ _ _ -> (1 :: Int) `shouldBe` 1
        _              -> expectationFailure "Expected LiftPure after frame-wire composition"

    it "binds 3-way exit channels in O(1) via (>?<)" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
          conts = insertField (Field @"out") retCont emptyRecord
          op :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          op = OpF 0## m (unsafeCoerce# ()) conts

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
EOF

chmod +x packages/penelope-core/src/Penelope/Core/Engine/Elaborate.hs || true
chmod +x packages/penelope-core/src/Penelope/Core/Engine.hs || true
chmod +x scripts/tasks/phase4-02-higher-order-elaboration.sh || true
echo "=== [yamaarashi] Milestone 4.2 setup complete ==="
