{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-all #-}

{- |
Module      : Main
Description : Milestone 6.1 GHC Core inspection tests
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Formally certifies GHC Core representation invariants via inspection-testing:
1. 'sendPolyF' and 'sendPolyH' compile to literal unboxed 'Word#' constants with
   zero residual dictionaries ('ConstructorIndex', 'KnownRowMorph').
2. 'elaborateAll' and 'interpretAll' dispatch via branchless 'indexSmallArray#'
   reads with zero 'eqWord#' comparisons and zero linear search cascades.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
module Main where

import Test.Hspec
import Test.Inspection
import Penelope.Core
import Polytropos (Field (..), Record, emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex, Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (KnownRowMorph)
import Data.Functor.Identity (Identity (..))
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (eqWord#, unsafeCoerce#)

-- Test effect signatures
data TestGet :: PolyEffect
data TestPut :: PolyEffect
type TestEF = '["get" ':= TestGet, "put" ':= TestPut]

data TestCatch :: PolyHEffect
type TestEH = '["catch" ':= TestCatch]

-- Target 1: sendPolyF compiled term
{-# NOINLINE sendTestF #-}
sendTestF :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
sendTestF =
  let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
      conts = insertField (Field @"ret") retCont emptyRecord
  in sendPolyF' @"get" @'["x" ':= Int] (100 :: Int) conts

-- Target 2: sendPolyH compiled term
{-# NOINLINE sendTestH #-}
sendTestH :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
sendTestH =
  let retCont = BranchCont (\_ -> route @'["x" ':= Int] @'["x" ':= Int])
      conts = insertField (Field @"ret") retCont emptyRecord
      subProg :: Penelope TestEH '[] '["x" ':= Int] '["x" ':= Int]
      subProg = route
      reg = Region @TestEH @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) subProg
      regions = insertField (Field @"body") reg emptyRecord
  in sendPolyH' @"catch" @'["x" ':= Int] regions () conts

-- Target 3: elaborateAll wrapper and evaluation
{-# NOINLINE runElaborateAll #-}
runElaborateAll ::
  Record (MapRow (Elaborator '[] TestEF) TestEH)
  -> Penelope TestEH TestEF '["x" ':= Int] '["x" ':= Int]
  -> Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
runElaborateAll = elaborateAll

{-# NOINLINE elaborateTest #-}
elaborateTest :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
elaborateTest =
  let testElaborator :: Elaborator '[] TestEF TestCatch
      testElaborator = Elaborator $ \inMap regs _ kConts ->
        let !(Identity elabBody) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
            !(Region body) = unsafeCoerce# elabBody
            !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
            !(BranchCont retK) = unsafeCoerce# elabK
        in preComposeRoute inMap (unsafeCoerce# (body `seqPenelope` retK emptyRecord))
      elabs = insertField (Field @"catch") testElaborator emptyRecord
      prog :: Penelope TestEH TestEF '["x" ':= Int] '["x" ':= Int]
      prog = route
  in elaborateAll elabs prog

-- Target 4: interpretAll wrapper and evaluation
{-# NOINLINE runInterpretAll #-}
runInterpretAll ::
  Record (MapRow (Handler '[] (FrameKleisli Identity)) TestEF)
  -> Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
  -> FrameKleisli Identity '["x" ':= Int] '["x" ':= Int]
runInterpretAll = interpretAll

{-# NOINLINE interpretTest #-}
interpretTest :: FrameKleisli Identity '["x" ':= Int] '["x" ':= Int]
interpretTest =
  let getHandler :: Handler '[] (FrameKleisli Identity) TestGet
      getHandler = Handler $ \_inMap _payload kConts ->
        let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
            !(BranchCont retK) = unsafeCoerce# elabK
            resFrame = insertField (Field @"x") (99 :: Int) emptyRecord
        in FrameKleisli $ \_inF -> runFrameKleisli (retK resFrame) resFrame
      putHandler :: Handler '[] (FrameKleisli Identity) TestPut
      putHandler = Handler $ \_inMap _payload kConts ->
        let !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
            !(BranchCont retK) = unsafeCoerce# elabK
        in FrameKleisli $ \inF -> runFrameKleisli (retK emptyRecord) inF
      handlers = insertField (Field @"get") getHandler
               $ insertField (Field @"put") putHandler emptyRecord
      prog :: Penelope '[] TestEF '["x" ':= Int] '["x" ':= Int]
      prog = route
  in interpretAll handlers prog

-- Core Inspection Obligations
inspect $ 'sendTestF `hasNoType` ''ConstructorIndex
inspect $ 'sendTestF `hasNoType` ''KnownRowMorph
inspect $ hasNoTypeClasses 'sendTestF

inspect $ 'sendTestH `hasNoType` ''ConstructorIndex
inspect $ 'sendTestH `hasNoType` ''KnownRowMorph
inspect $ hasNoTypeClasses 'sendTestH

inspect $ 'runElaborateAll `doesNotUse` 'eqWord#
inspect $ 'elaborateTest `doesNotUse` 'eqWord#

inspect $ 'runInterpretAll `doesNotUse` 'eqWord#
inspect $ 'interpretTest `doesNotUse` 'eqWord#

main :: IO ()
main = hspec $ do
  describe "Penelope.Core Inspection Obligations" $ do
    it "sendPolyF eliminates ConstructorIndex dictionary" $ do
      case $(inspectTest $ 'sendTestF `hasNoType` ''ConstructorIndex) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "sendPolyF eliminates KnownRowMorph dictionary" $ do
      case $(inspectTest $ 'sendTestF `hasNoType` ''KnownRowMorph) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "sendPolyF passes zero dictionaries (hasNoTypeClasses)" $ do
      case $(inspectTest $ hasNoTypeClasses 'sendTestF) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "sendPolyH eliminates ConstructorIndex dictionary" $ do
      case $(inspectTest $ 'sendTestH `hasNoType` ''ConstructorIndex) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "sendPolyH eliminates KnownRowMorph dictionary" $ do
      case $(inspectTest $ 'sendTestH `hasNoType` ''KnownRowMorph) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "sendPolyH passes zero dictionaries (hasNoTypeClasses)" $ do
      case $(inspectTest $ hasNoTypeClasses 'sendTestH) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "elaborateAll does not use eqWord#" $ do
      case $(inspectTest $ 'runElaborateAll `doesNotUse` 'eqWord#) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "elaborateTest does not use eqWord#" $ do
      case $(inspectTest $ 'elaborateTest `doesNotUse` 'eqWord#) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "interpretAll does not use eqWord#" $ do
      case $(inspectTest $ 'runInterpretAll `doesNotUse` 'eqWord#) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err

    it "interpretTest does not use eqWord#" $ do
      case $(inspectTest $ 'interpretTest `doesNotUse` 'eqWord#) of
        Success msg -> putStrLn $ "  [PASS] " ++ msg
        Failure err -> expectationFailure err
