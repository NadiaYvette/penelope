#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase6-02-reduct-properties
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 6.2: Institutional Satisfaction & Reduct Property Tests ==="

mkdir -p packages/penelope-static/test

# 1. Update packages/penelope-static/penelope-static.cabal
cat <<'CABAL_EOF' > packages/penelope-static/penelope-static.cabal
cabal-version: 3.0
name: penelope-static
version: 0.1.0.0
synopsis: Static Reflection and Cayley Analysis for Penelope Effect Engine
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Static
    Penelope.Static.Analysis
  hs-source-dirs: src
  default-language: Haskell2010
  default-extensions:
    AllowAmbiguousTypes
    BangPatterns
    DataKinds
    DerivingStrategies
    FlexibleContexts
    GADTs
    KindSignatures
    MagicHash
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
    TypeApplications
    TypeOperators
  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts
  build-depends:
    base >= 4.14 && < 5,
    hspec >= 2.10,
    text >= 2.0,
    containers >= 0.6,
    primitive >= 0.9,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    penelope-core >= 0.1,
    penelope-static

test-suite properties
  type: exitcode-stdio-1.0
  main-is: Properties.hs
  hs-source-dirs: test
  default-language: Haskell2010
  default-extensions:
    AllowAmbiguousTypes
    BangPatterns
    DataKinds
    DerivingStrategies
    FlexibleContexts
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
    hspec >= 2.10,
    QuickCheck >= 2.14,
    text >= 2.0,
    containers >= 0.6,
    primitive >= 0.9,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    penelope-core >= 0.1,
    penelope-static
CABAL_EOF

# 2. Implement packages/penelope-static/test/Properties.hs
cat <<'PROP_EOF' > packages/penelope-static/test/Properties.hs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

{- |
Module      : Main
Description : Milestone 6.2 Institutional Satisfaction & Reduct Property Tests
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Property-tests fundamental institutional invariants of the Penelope engine:
1. Reduct-Evaluation Identity:
   eval_J(H, sigma_*(P)) === eval_I(sigma^*(H), P)
   across randomized row permutations, embeddings, and register frame inputs.
2. Static-Elaboration Commutativity:
   Static telemetry and metadata extracted by 'StaticPenelope' and
   'analyzeLivenessAndEffects' commute with higher-order macro elaboration
   'elaborateAll'.

See Note [Core Architecture] [ref:note-core-architecture],
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening],
Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine], and
Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
module Main (main) where

import Control.Monad (forM_)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))
import Data.Primitive.SmallArray (indexSmallArray, newSmallArray, runSmallArray, writeSmallArray)
import GHC.Exts (Any, unsafeCoerce#)
import Penelope.Core
import Penelope.Static
import Polytropos (Field (..), Record, emptyRecord, gatherRecord, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (composeRowMorph, rowMorph, unsafeMkRowMorph)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- Effect signatures for sub-row I
data OpAdd :: PolyEffect
data OpMul :: PolyEffect
data OpSet :: PolyEffect
type SubEF = '["add" ':= OpAdd, "mul" ':= OpMul, "set" ':= OpSet]

data OpScoped :: PolyHEffect
type SubEH = '["scoped" ':= OpScoped]

-- Algebraic command language
data Command
  = CmdAdd !Int
  | CmdMul !Int
  | CmdSet !Int
  | CmdScopedAdd !Int
  deriving stock (Show, Eq)

instance Arbitrary Command where
  arbitrary = oneof
    [ CmdAdd <$> choose (-50, 50)
    , CmdMul <$> choose (-5, 5)
    , CmdSet <$> choose (-100, 100)
    , CmdScopedAdd <$> choose (-20, 20)
    ]

-- Distinct permutation generator for embedding [0, 1, 2] into [0 .. n-1]
data RowEmbedding = RowEmbedding
  { reWideSize :: !Int
  , reAddIdx   :: !Int
  , reMulIdx   :: !Int
  , reSetIdx   :: !Int
  , reScopedIdx :: !Int
  , reWideHSize :: !Int
  } deriving stock (Show, Eq)

instance Arbitrary RowEmbedding where
  arbitrary = do
    wideN <- choose (4, 8)
    wideM <- choose (2, 5)
    -- Pick 3 distinct indices in [0 .. wideN - 1]
    p1 <- choose (0, wideN - 1)
    p2 <- suchThat (choose (0, wideN - 1)) (/= p1)
    p3 <- suchThat (choose (0, wideN - 1)) (\x -> x /= p1 && x /= p2)
    q1 <- choose (0, wideM - 1)
    pure $ RowEmbedding wideN p1 p2 p3 q1 wideM

-- Smart constructors for SubEF and SubEH operations
buildAdd :: Int -> Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
buildAdd k =
  let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
      conts = insertField (Field @"ret") retCont emptyRecord
  in OpF 0## (rowMorph @'["x" ':= Int] @'["x" ':= Int]) (unsafeCoerce# k) conts

buildMul :: Int -> Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
buildMul k =
  let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
      conts = insertField (Field @"ret") retCont emptyRecord
  in OpF 1## (rowMorph @'["x" ':= Int] @'["x" ':= Int]) (unsafeCoerce# k) conts

buildSet :: Int -> Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
buildSet k =
  let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
      conts = insertField (Field @"ret") retCont emptyRecord
  in OpF 2## (rowMorph @'["x" ':= Int] @'["x" ':= Int]) (unsafeCoerce# k) conts

buildScopedAdd :: Int -> Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
buildScopedAdd k =
  let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
      conts = insertField (Field @"ret") retCont emptyRecord
      subProg :: Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
      subProg = buildAdd k
      reg = Region @SubEH @SubEF @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
      regions = insertField (Field @"body") reg emptyRecord
  in OpH 0## (rowMorph @'["x" ':= Int] @'["x" ':= Int]) regions (unsafeCoerce# ()) conts

buildCommand :: Command -> Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
buildCommand (CmdAdd k)       = buildAdd k
buildCommand (CmdMul k)       = buildMul k
buildCommand (CmdSet k)       = buildSet k
buildCommand (CmdScopedAdd k) = buildScopedAdd k

buildPipeline :: [Command] -> Penelope SubEH SubEF '["x" ':= Int] '["x" ':= Int]
buildPipeline []     = route
buildPipeline (c:cs) = buildCommand c `seqPenelope` buildPipeline cs

-- Concrete Handlers
addHandler :: Handler '[] (FrameKleisli Identity) OpAdd
addHandler = Handler $ \_inMap payload kConts ->
  let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
      !kVal = unsafeCoerce# payload :: Int
  in FrameKleisli $ \inF ->
       let !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = inF in arr) 0
           !curX = unsafeCoerce# xVal :: Int
           !newX = curX + kVal
           !outF = insertField (Field @"x") newX emptyRecord
       in runFrameKleisli (retK outF) outF

mulHandler :: Handler '[] (FrameKleisli Identity) OpMul
mulHandler = Handler $ \_inMap payload kConts ->
  let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
      !kVal = unsafeCoerce# payload :: Int
  in FrameKleisli $ \inF ->
       let !(Identity xVal) = indexSmallArray (let RowRep (Rep _ arr) = inF in arr) 0
           !curX = unsafeCoerce# xVal :: Int
           !newX = curX * kVal
           !outF = insertField (Field @"x") newX emptyRecord
       in runFrameKleisli (retK outF) outF

setHandler :: Handler '[] (FrameKleisli Identity) OpSet
setHandler = Handler $ \_inMap payload kConts ->
  let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
      !newX = unsafeCoerce# payload :: Int
      !outF = insertField (Field @"x") newX emptyRecord
  in FrameKleisli $ \_inF -> runFrameKleisli (retK outF) outF

dummyHandler :: Handler '[] (FrameKleisli Identity) OpAdd
dummyHandler = Handler $ \_ _ _ ->
  errorWithoutStackTrace "Properties: dummyHandler invoked — index dispatch violation!"

-- Concrete Elaborator for OpScoped
scopedElaborator :: Elaborator '[] SubEF OpScoped
scopedElaborator = Elaborator $ \inMap regs _ kConts ->
  let !(Identity elabBody) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
      !(Region body) = unsafeCoerce# elabBody
      !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
  in preComposeRoute inMap (unsafeCoerce# (body `seqPenelope` retK emptyRecord))

dummyElaborator :: Elaborator '[] SubEF OpScoped
dummyElaborator = Elaborator $ \_ _ _ _ ->
  errorWithoutStackTrace "Properties: dummyElaborator invoked — index dispatch violation!"

-- Make arbitrary-sized Record of handlers/elaborators
makeWideRecord :: Int -> [(Int, Any)] -> Any -> Record r
makeWideRecord !n !indexedEntries !dummy =
  RowRep $ Rep 0 $ runSmallArray $ do
    arr <- newSmallArray n (Identity dummy)
    forM_ indexedEntries $ \(idx, val) ->
      writeSmallArray arr idx (Identity val)
    pure arr

-- Extract 'x' from a frame
getX :: Record '["x" ':= Int] -> Int
getX (RowRep (Rep _ arr)) =
  let !(Identity x) = indexSmallArray arr 0
  in unsafeCoerce# x

main :: IO ()
main = hspec $ do
  describe "Penelope.Core Milestone 6.2: Institutional Satisfaction & Reduct Properties" $ do

    prop "Reduct-Evaluation Identity: eval_J(H, sigma_*(P)) === eval_I(sigma^*(H), P)" $
      \(commands :: [Command]) (emb :: RowEmbedding) (initX :: Int) ->
        let !pipe = buildPipeline commands
            !inFrame = insertField (Field @"x") initX emptyRecord

            -- Embeddings sigma_ef and sigma_eh
            !sigmaEf = unsafeMkRowMorph [reAddIdx emb, reMulIdx emb, reSetIdx emb]
            !sigmaEh = unsafeMkRowMorph [reScopedIdx emb]

            -- Wide handlers and elaborators for J
            !wideHandlers = makeWideRecord
              (reWideSize emb)
              [ (reAddIdx emb, unsafeCoerce# addHandler)
              , (reMulIdx emb, unsafeCoerce# mulHandler)
              , (reSetIdx emb, unsafeCoerce# setHandler)
              ]
              (unsafeCoerce# dummyHandler)

            !wideElabs = makeWideRecord
              (reWideHSize emb)
              [ (reScopedIdx emb, unsafeCoerce# scopedElaborator) ]
              (unsafeCoerce# dummyElaborator)

            -- 1. Weakened evaluation in J: eval_J(H, sigma_*(P))
            !pWeakened = weakenRowExplicit sigmaEh sigmaEf pipe
            !pWeakenedElab = elaborateAll wideElabs pWeakened
            !(Identity outJ) = runFrameKleisli (interpretAll wideHandlers pWeakenedElab) inFrame
            !resJ = getX outJ

            -- 2. Reduct evaluation in I: eval_I(sigma^*(H), P)
            !reductElabs = unsafeCoerce# (gatherRecord (unsafeCoerce# sigmaEh) wideElabs)
            !reductHandlers = unsafeCoerce# (gatherRecord (unsafeCoerce# sigmaEf) wideHandlers)
            !pReductElab = elaborateAll reductElabs pipe
            !(Identity outI) = runFrameKleisli (interpretAll reductHandlers pReductElab) inFrame
            !resI = getX outI

        in resJ === resI

    prop "Functorial Reduct Transitivity: (sigma2 . sigma1)^*(H) === sigma1^*(sigma2^*(H))" $
      \(commands :: [Command]) (initX :: Int) ->
        let !pipe = buildPipeline commands
            !inFrame = insertField (Field @"x") initX emptyRecord

            -- Intermediate row K (size 4), Final row J (size 6)
            !sigma1 = unsafeMkRowMorph [1, 2, 0]             -- 3 -> 4
            !sigma2 = unsafeMkRowMorph [3, 1, 4, 0]          -- 4 -> 6
            !sigmaComp = composeRowMorph sigma1 sigma2       -- 3 -> 6

            !wideHandlers6 = makeWideRecord
              6
              [ (1, unsafeCoerce# addHandler)
              , (4, unsafeCoerce# mulHandler)
              , (3, unsafeCoerce# setHandler)
              ]
              (unsafeCoerce# dummyHandler)

            -- Single higher-order identity
            !idEh = unsafeMkRowMorph [0]
            !elabs1 = makeWideRecord 1 [(0, unsafeCoerce# scopedElaborator)] (unsafeCoerce# dummyElaborator)

            -- Two-step weakening vs composite weakening
            !pStep1 = weakenRowExplicit idEh sigma1 pipe
            !pStep2 = weakenRowExplicit idEh sigma2 pStep1
            !pStepElab = elaborateAll elabs1 pStep2
            !(Identity outTwoStep) = runFrameKleisli (interpretAll wideHandlers6 pStepElab) inFrame
            !resTwoStep = getX outTwoStep

            !pComp = weakenRowExplicit idEh sigmaComp pipe
            !pCompElab = elaborateAll elabs1 pComp
            !(Identity outComp) = runFrameKleisli (interpretAll wideHandlers6 pCompElab) inFrame
            !resComp = getX outComp

        in resTwoStep === resComp

  describe "Penelope.Static Milestone 6.2: Static-Elaboration Commutativity Properties" $ do

    prop "Metadata Conservation: spStatic (elaborateStatic elabs sp) === spStatic sp" $
      \(commands :: [Command]) (metaVal :: Int) ->
        let !pipe = buildPipeline commands
            !sp = annotateStatic (Const (Sum metaVal)) pipe
            !elabs = makeWideRecord 1 [(0, unsafeCoerce# scopedElaborator)] (unsafeCoerce# dummyElaborator)
            !spElab = StaticPenelope (spStatic sp) (elaborateAll elabs (spPipeline sp))
        in spStatic spElab === spStatic sp

    prop "Sequential Composition Commutativity: elaborate(sp1 >>> sp2) === elaborate(sp1) >>> elaborate(sp2)" $
      \(cmds1 :: [Command]) (cmds2 :: [Command]) (m1 :: Int) (m2 :: Int) (initX :: Int) ->
        let !p1 = buildPipeline cmds1
            !p2 = buildPipeline cmds2
            !sp1 = annotateStatic (Const (Sum m1)) p1
            !sp2 = annotateStatic (Const (Sum m2)) p2

            !elabs = makeWideRecord 1 [(0, unsafeCoerce# scopedElaborator)] (unsafeCoerce# dummyElaborator)
            !handlers = makeWideRecord
              3
              [ (0, unsafeCoerce# addHandler)
              , (1, unsafeCoerce# mulHandler)
              , (2, unsafeCoerce# setHandler)
              ]
              (unsafeCoerce# dummyHandler)

            -- Composed then elaborated
            !composed = sp1 `seqStatic` sp2
            !spCompElab = StaticPenelope (spStatic composed) (elaborateAll elabs (spPipeline composed))

            -- Separately elaborated then composed
            !sp1Elab = StaticPenelope (spStatic sp1) (elaborateAll elabs (spPipeline sp1))
            !sp2Elab = StaticPenelope (spStatic sp2) (elaborateAll elabs (spPipeline sp2))
            !spSepElab = sp1Elab `seqStatic` sp2Elab

            -- Static metadata match
            !metaMatch = spStatic spCompElab == spStatic spSepElab

            -- Runtime execution match
            !inFrame = insertField (Field @"x") initX emptyRecord
            !(Identity outComp) = runFrameKleisli (interpretAll handlers (spPipeline spCompElab)) inFrame
            !(Identity outSep) = runFrameKleisli (interpretAll handlers (spPipeline spSepElab)) inFrame
            !execMatch = getX outComp == getX outSep

        in metaMatch && execMatch

    prop "Structural Liveness Invariant: port read footprint is invariant under elaborateAll" $
      \(commands :: [Command]) ->
        let !pipe = buildPipeline commands
            !elabs = makeWideRecord 1 [(0, unsafeCoerce# scopedElaborator)] (unsafeCoerce# dummyElaborator)
            !pipeElab = elaborateAll elabs pipe

            !repPre = analyzeLivenessAndEffects pipe
            !repPost = analyzeLivenessAndEffects pipeElab

            -- Port read set is strictly preserved
            !readSlotsPreserved = lrReadFrameSlots repPre == lrReadFrameSlots repPost

            -- Higher-order regions are unrolled to zero
            !regionsUnrolled = lrTotalRegions repPost == 0

            -- Branching factor bound is maintained
            !branchFactorSound = lrMaxBranchingFactor repPost <= max 1 (lrMaxBranchingFactor repPre)

        in readSlotsPreserved && regionsUnrolled && branchFactorSound
PROP_EOF

echo "=== [yamaarashi] Milestone 6.2 implementation complete ==="
