#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase6-04-benchmarks
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 6.4: Allocation & Throughput Benchmarks ==="

# 1. Update packages/penelope-effects/penelope-effects.cabal
cat <<'CABAL_EOF' > packages/penelope-effects/penelope-effects.cabal
cabal-version: 3.0
name: penelope-effects
version: 0.1.0.0
synopsis: Domain package for penelope-effects
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Effects
    Penelope.Effects.Unary
    Penelope.Effects.Binary
    Penelope.Effects.Control
    Penelope.Effects.Coroutine
    Penelope.Effects.UnliftIO
    Penelope.Effects.Scoped
    Penelope.Effects.Concurrent
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
    profunctors >= 5.6,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    penelope-core >= 0.1

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
    ScopedTypeVariables
    TypeApplications
    TypeFamilies
    TypeOperators
  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts
  build-depends:
    base >= 4.14 && < 5,
    penelope-effects,
    penelope-core >= 0.1,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    primitive >= 0.9,
    profunctors >= 5.6,
    text >= 2.0,
    hspec >= 2.10

benchmark bench
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: bench
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
  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-T -O2
  build-depends:
    base >= 4.14 && < 5,
    text >= 2.0,
    containers >= 0.6,
    primitive >= 0.9,
    profunctors >= 5.6,
    deepseq >= 1.4,
    tasty-bench >= 0.3,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    penelope-core >= 0.1,
    penelope-effects >= 0.1
CABAL_EOF

# 2. Implement benchmark suite in packages/penelope-effects/bench/Main.hs
mkdir -p packages/penelope-effects/bench
cat <<'BENCH_EOF' > packages/penelope-effects/bench/Main.hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

{- |
Module      : Main
Description : Milestone 6.4 Allocation & Throughput Benchmarks
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Comprehensive Criterion/Gauge compatible benchmarks via 'tasty-bench':
1. Deep First-Order Effect Stacks (D = 30):
   Demonstrates \(O(1)\) branchless vector indexing vs \(O(D)\) freer sum peeling.
2. Deep Higher-Order Scoped Elaboration (H = 15):
   Demonstrates single-pass bottom-up elaboration vs multi-pass linear tree traversal.
3. Wide Register Frames (m, n = 20 ports):
   Demonstrates \(O(1)\) static 'RowMorph' gather-scatter vs \(O(N)\) record/lens traversal.
4. Surface Facades Throughput & Allocation ('Eff' & 'Rope'):
   Measures monadic and arrow composition overhead vs pure baselines.
5. Scoped Semantics & Delimited Continuations:
   Measures 'local', 'catchError', and multi-shot coroutine resumption overhead.

See Note [Core Architecture] [ref:note-core-architecture],
Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation],
Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing],
Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine], and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Main (main) where

import qualified Control.Arrow as Arrow
import qualified Control.Category as Cat
import Data.Functor.Identity (Identity (..))
import Data.Primitive.SmallArray (indexSmallArray, smallArrayFromList)
import GHC.Exts (Any, Word#, unsafeCoerce#)
import Penelope.Core
  ( BranchCont (..)
  , Elaborator (..)
  , FrameKleisli (..)
  , Handler (..)
  , MapRow
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , PortSig (..)
  , Region (..)
  , elaborateAll
  , interpretAll
  , preComposeRoute
  , runFrameKleisli
  , seqPenelope
  )
import Penelope.Effects
  ( AskEff (..)
  , Eff
  , NonDetEff (..)
  , Rope
  , ThrowEff (..)
  , UnaryEffect
  , askReader
  , branch
  , catchError
  , local
  , nonDetHandler
  , runEff
  , runErrorPure
  , runReaderPure
  , runRopePure
  , runEffM
  )
import Polytropos (Field (..), Pair (..), Record, emptyRecord, insertField)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph (RowMorph, gatherRep, unsafeMkRowMorph)
import Prelude hiding (id, (.))
import qualified Prelude as P
import Test.Tasty.Bench
  ( bench
  , bgroup
  , defaultMain
  , nf
  , whnf
  )

{-------------------------------------------------------------------------------
  Suite 1: Deep First-Order Effect Stacks (D = 30)
  See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation]
  and Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine]
-------------------------------------------------------------------------------}

-- 30 distinct first-order effect kinds
data Op01 :: PolyEffect
data Op02 :: PolyEffect
data Op03 :: PolyEffect
data Op04 :: PolyEffect
data Op05 :: PolyEffect
data Op06 :: PolyEffect
data Op07 :: PolyEffect
data Op08 :: PolyEffect
data Op09 :: PolyEffect
data Op10 :: PolyEffect
data Op11 :: PolyEffect
data Op12 :: PolyEffect
data Op13 :: PolyEffect
data Op14 :: PolyEffect
data Op15 :: PolyEffect
data Op16 :: PolyEffect
data Op17 :: PolyEffect
data Op18 :: PolyEffect
data Op19 :: PolyEffect
data Op20 :: PolyEffect
data Op21 :: PolyEffect
data Op22 :: PolyEffect
data Op23 :: PolyEffect
data Op24 :: PolyEffect
data Op25 :: PolyEffect
data Op26 :: PolyEffect
data Op27 :: PolyEffect
data Op28 :: PolyEffect
data Op29 :: PolyEffect
data Op30 :: PolyEffect

type OpRow30 =
  '[ "op01" ':= Op01, "op02" ':= Op02, "op03" ':= Op03, "op04" ':= Op04, "op05" ':= Op05
   , "op06" ':= Op06, "op07" ':= Op07, "op08" ':= Op08, "op09" ':= Op09, "op10" ':= Op10
   , "op11" ':= Op11, "op12" ':= Op12, "op13" ':= Op13, "op14" ':= Op14, "op15" ':= Op15
   , "op16" ':= Op16, "op17" ':= Op17, "op18" ':= Op18, "op19" ':= Op19, "op20" ':= Op20
   , "op21" ':= Op21, "op22" ':= Op22, "op23" ':= Op23, "op24" ':= Op24, "op25" ':= Op25
   , "op26" ':= Op26, "op27" ':= Op27, "op28" ':= Op28, "op29" ':= Op29, "op30" ':= Op30
   ]

-- | Build a Penelope chain of N operations targeting a specific constructor tag.
mkChainPenelope :: Int -> Word# -> Penelope '[] OpRow30 '["x" ':= Int] '["x" ':= Int]
mkChainPenelope 0 _ = Route (unsafeMkRowMorph [0])
mkChainPenelope n tag# =
  let retCont = BranchCont (\_outF -> mkChainPenelope (n - 1) tag#)
      conts = RowRep (Rep 0 (smallArrayFromList [Identity (unsafeCoerce# retCont)]))
  in OpF tag# (unsafeMkRowMorph [0]) (unsafeCoerce# (1 :: Int)) conts

-- | Generic identity handler executing the continuation.
genericHandler :: Handler '[] (FrameKleisli Identity) e
genericHandler = Handler $ \_inMap _payload kConts ->
  let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
  in FrameKleisli $ \inF -> runFrameKleisli (retK inF) inF

-- | Construct the 30-element handler record.
handlers30 :: Record (MapRow (Handler '[] (FrameKleisli Identity)) OpRow30)
handlers30 =
  let hAny = Identity (unsafeCoerce# (genericHandler @Op01))
      arr = smallArrayFromList (replicate 30 hAny)
  in RowRep (Rep 0 arr)

-- | Run a Penelope chain of N operations at the given effect tag.
runPenelopeChain :: Word# -> Int -> Int
runPenelopeChain tag# n =
  let prog = mkChainPenelope n tag#
      inFrame = insertField (Field @"x") (42 :: Int) emptyRecord
      kleisli = interpretAll handlers30 prog
      outFrame = runIdentity (runFrameKleisli kleisli inFrame)
      !(Identity valAny) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
  in (unsafeCoerce# valAny :: Int)

-- | Freer open sum baseline modeling linear union peeling.
data FreerSum = FreerLeaf !Int | FreerBranch !FreerSum

decompFreer :: FreerSum -> Either FreerSum Int
decompFreer = \case
  FreerLeaf x   -> Right x
  FreerBranch u -> Left u

evalFreerAtDepth :: Int -> Int -> Int
evalFreerAtDepth depth val =
  let !tree = buildFreer depth val
  in peelFreer depth tree
  where
    buildFreer :: Int -> Int -> FreerSum
    buildFreer 0 x = FreerLeaf x
    buildFreer k x = FreerBranch (buildFreer (k - 1) x)

    peelFreer :: Int -> FreerSum -> Int
    peelFreer 0 (FreerLeaf x) = x
    peelFreer k (FreerBranch u) =
      case decompFreer (FreerBranch u) of
        Right x -> x
        Left rest -> peelFreer (k - 1) rest
    peelFreer _ _ = 0

evalFreerChain :: Int -> Int -> Int
evalFreerChain _depth 0 = 0
evalFreerChain depth n =
  let !res = evalFreerAtDepth depth n
  in res + evalFreerChain depth (n - 1)

{-------------------------------------------------------------------------------
  Suite 2: Deep Higher-Order Scoped Elaboration (H = 15)
  See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine]
-------------------------------------------------------------------------------}

data Scope01 :: PolyHEffect
data Scope02 :: PolyHEffect
data Scope03 :: PolyHEffect
data Scope04 :: PolyHEffect
data Scope05 :: PolyHEffect
data Scope06 :: PolyHEffect
data Scope07 :: PolyHEffect
data Scope08 :: PolyHEffect
data Scope09 :: PolyHEffect
data Scope10 :: PolyHEffect
data Scope11 :: PolyHEffect
data Scope12 :: PolyHEffect
data Scope13 :: PolyHEffect
data Scope14 :: PolyHEffect
data Scope15 :: PolyHEffect

type ScopeRow15 =
  '[ "scope01" ':= Scope01, "scope02" ':= Scope02, "scope03" ':= Scope03
   , "scope04" ':= Scope04, "scope05" ':= Scope05, "scope06" ':= Scope06
   , "scope07" ':= Scope07, "scope08" ':= Scope08, "scope09" ':= Scope09
   , "scope10" ':= Scope10, "scope11" ':= Scope11, "scope12" ':= Scope12
   , "scope13" ':= Scope13, "scope14" ':= Scope14, "scope15" ':= Scope15
   ]

-- | Build a nested tree with H scoped regions.
mkNestedScopes :: Int -> Penelope ScopeRow15 '[] '["x" ':= Int] '["x" ':= Int]
mkNestedScopes 0 = Route (unsafeMkRowMorph [0])
mkNestedScopes h =
  let subRegion = Region @ScopeRow15 @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) (mkNestedScopes (h - 1))
      regions = RowRep (Rep 0 (smallArrayFromList [Identity (unsafeCoerce# subRegion)]))
      retCont = BranchCont (\_outF -> Route (unsafeMkRowMorph [0]))
      conts = RowRep (Rep 0 (smallArrayFromList [Identity (unsafeCoerce# retCont)]))
      tag# = case h of
        1  -> 0##
        2  -> 1##
        3  -> 2##
        4  -> 3##
        5  -> 4##
        6  -> 5##
        7  -> 6##
        8  -> 7##
        9  -> 8##
        10 -> 9##
        11 -> 10##
        12 -> 11##
        13 -> 12##
        14 -> 13##
        _  -> 14##
  in OpH tag# (unsafeMkRowMorph [0]) regions (unsafeCoerce# (0 :: Int)) conts

genericElaborator :: Elaborator '[] '[] e
genericElaborator = Elaborator $ \inMap regions _payload conts ->
  let !(Identity regAny) = indexSmallArray (let RowRep (Rep _ arr) = regions in arr) 0
      !(Region innerPipe) = unsafeCoerce# regAny
      !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = conts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
  in preComposeRoute inMap (unsafeCoerce# (innerPipe `seqPenelope` retK (unsafeCoerce# emptyRecord)))

elabs15 :: Record (MapRow (Elaborator '[] '[]) ScopeRow15)
elabs15 =
  let eAny = Identity (unsafeCoerce# (genericElaborator @Scope01))
      arr = smallArrayFromList (replicate 15 eAny)
  in RowRep (Rep 0 arr)

runSinglePassElaboration :: Int -> Int
runSinglePassElaboration h =
  case elaborateAll elabs15 (mkNestedScopes h) of
    !_ -> h

-- Baseline multi-pass tree traversal:
data ScopedTree = ScopedLeaf !Int | ScopedNode !Int ScopedTree

buildScopedTree :: Int -> ScopedTree
buildScopedTree 0 = ScopedLeaf 42
buildScopedTree k = ScopedNode k (buildScopedTree (k - 1))

multiPassElab :: Int -> ScopedTree -> ScopedTree
multiPassElab 0 tree = tree
multiPassElab passes tree = multiPassElab (passes - 1) (walkTree tree)
  where
    walkTree (ScopedLeaf x) = ScopedLeaf (x + 1)
    walkTree (ScopedNode tag child) = ScopedNode tag (walkTree child)

runMultiPassElaboration :: Int -> Int
runMultiPassElaboration h =
  let tree = buildScopedTree h
      res = multiPassElab h tree
  in case res of
       ScopedLeaf x -> x
       ScopedNode _ _ -> 0

{-------------------------------------------------------------------------------
  Suite 3: Wide Register Frames (m, n = 20 Ports)
  See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing]
-------------------------------------------------------------------------------}

mkRep20 :: Rep Identity Any
mkRep20 =
  let arr = smallArrayFromList [Identity (unsafeCoerce# i) | i <- [0..19 :: Int]]
  in Rep 0 arr

morphId20 :: RowMorph Any Any
morphId20 = unsafeMkRowMorph [0..19]

morphRev20 :: RowMorph Any Any
morphRev20 = unsafeMkRowMorph [19, 18 .. 0]

morphProj10 :: RowMorph Any Any
morphProj10 = unsafeMkRowMorph [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]

benchGatherRep :: RowMorph Any Any -> Int -> Int
benchGatherRep morph iterations = go iterations
  where
    go 0 = 0
    go k =
      let !(Rep _ arr) = gatherRep morph mkRep20
          !(Identity v) = indexSmallArray arr 0
      in (unsafeCoerce# v :: Int) + go (k - 1)

benchListTraversal :: Int -> Int
benchListTraversal iterations = go iterations
  where
    initialList :: [Int]
    initialList = [0..19]

    go 0 = 0
    go k =
      let rev = reverse initialList
          res = case rev of
            (x : _) -> x
            []      -> 0
      in res + go (k - 1)

{-------------------------------------------------------------------------------
  Suite 4: Surface Facades (Unary Eff & Binary Rope)
  See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades]
-------------------------------------------------------------------------------}

benchEffPureChain :: Int -> Int
benchEffPureChain n = runEff (go n 0)
  where
    go :: Int -> Int -> Eff '[] '[] Int
    go 0 !acc = pure acc
    go k !acc = pure (acc + 1) >>= go (k - 1)

benchIdentityPureChain :: Int -> Int
benchIdentityPureChain n = runIdentity (go n 0)
  where
    go :: Int -> Int -> Identity Int
    go 0 !acc = pure acc
    go k !acc = pure (acc + 1) >>= go (k - 1)

benchRopeChain :: Int -> Int -> Int
benchRopeChain n input = runRopePure (buildRope n) input
  where
    buildRope :: Int -> Rope '[] '[] Int Int
    buildRope 0 = Cat.id
    buildRope k = Arrow.arr (+ 1) Cat.>>> buildRope (k - 1)

benchFunctionChain :: Int -> Int -> Int
benchFunctionChain n input = buildFn n input
  where
    buildFn :: Int -> (Int -> Int)
    buildFn 0 = P.id
    buildFn k = (+ 1) P.. buildFn (k - 1)

{-------------------------------------------------------------------------------
  Suite 5: Scoped Semantics & Delimited Continuations
-------------------------------------------------------------------------------}

benchLocalScope :: Int -> Int
benchLocalScope n = runReaderPure 100 (go n)
  where
    go :: Int -> Eff '[] '["reader" ':= UnaryEffect (AskEff Int)] Int
    go 0 = askReader
    go k = local @Int (+ 1) (go (k - 1))

benchCatchScope :: Int -> Either String Int
benchCatchScope n = runErrorPure (go n)
  where
    go :: Int -> Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] Int
    go 0 = pure 42
    go k = catchError (go (k - 1)) (\_err -> pure (-1))

type NonDetEF = '["nondet" ':= UnaryEffect NonDetEff]

benchNonDetBranch :: Int -> [Int]
benchNonDetBranch n =
  let handlers = insertField (Field @"nondet") nonDetHandler emptyRecord
  in runEffM handlers (go n)
  where
    go :: Int -> Eff '[] NonDetEF Int
    go 0 = pure 0
    go k = do
      b <- branch @"nondet"
      rest <- go (k - 1)
      pure ((if b then 1 else 2) + rest)

{-------------------------------------------------------------------------------
  Main Benchmark Execution Harness
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain
  [ bgroup "1. First-Order Effect Depth (D = 30)"
      [ bgroup "Penelope O(1) Vector Dispatch"
          [ bench "Depth 1 (Shallow / Op01) [1000 binds]" $
              whnf (runPenelopeChain 0##) 1000
          , bench "Depth 15 (Mid / Op15) [1000 binds]" $
              whnf (runPenelopeChain 14##) 1000
          , bench "Depth 30 (Deep / Op30) [1000 binds]" $
              whnf (runPenelopeChain 29##) 1000
          ]
      , bgroup "Freer O(D) Linear Peeling Baseline"
          [ bench "Depth 1 (Shallow) [1000 binds]" $
              whnf (evalFreerChain 1) 1000
          , bench "Depth 15 (Mid) [1000 binds]" $
              whnf (evalFreerChain 15) 1000
          , bench "Depth 30 (Deep) [1000 binds]" $
              whnf (evalFreerChain 30) 1000
          ]
      ]
  , bgroup "2. Higher-Order Scoped Elaboration (H = 15 Scopes)"
      [ bench "Penelope Single-Pass Elaboration (H = 15)" $
          whnf runSinglePassElaboration 15
      , bench "Baseline Multi-Pass Tree Traversal (H = 15)" $
          whnf runMultiPassElaboration 15
      ]
  , bgroup "3. Wide Register Frames (m, n = 20 Ports)"
      [ bench "Penelope RowMorph Identity [1000 ops]" $
          whnf (benchGatherRep morphId20) 1000
      , bench "Penelope RowMorph Reversal [1000 ops]" $
          whnf (benchGatherRep morphRev20) 1000
      , bench "Penelope RowMorph Projection (10 ports) [1000 ops]" $
          whnf (benchGatherRep morphProj10) 1000
      , bench "Baseline List/Record Reversal [1000 ops]" $
          whnf benchListTraversal 1000
      ]
  , bgroup "4. Surface Facades (Unary Eff & Binary Rope)"
      [ bench "Penelope Eff Monadic Chain [1000 binds]" $
          whnf benchEffPureChain 1000
      , bench "Baseline Identity Monadic Chain [1000 binds]" $
          whnf benchIdentityPureChain 1000
      , bench "Penelope Rope Arrow Pipeline [1000 stages]" $
          whnf (benchRopeChain 1000) 42
      , bench "Baseline Function Composition [1000 stages]" $
          whnf (benchFunctionChain 1000) 42
      ]
  , bgroup "5. Scoped Semantics & Delimited Continuations"
      [ bench "Nested Local Environment Modification [Depth 10]" $
          whnf benchLocalScope 10
      , bench "CatchError Interception Boundary [Depth 10]" $
          nf benchCatchScope 10
      , bench "Delimited Coroutine Multi-Shot Branching [2^8 branches]" $
          nf benchNonDetBranch 8
      ]
  ]
BENCH_EOF

echo "=== [yamaarashi] Milestone 6.4 Implementation complete! ==="
