{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module Main (main) where

import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
import Control.Category (Category (..))
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (unsafeCoerce#)
import Penelope.Core.Engine.Elaborate (elaborateAll)
import Penelope.Core.Engine.Interpret (interpretAll, runFrameKleisli, runPenelopePure)
import Penelope.Core.Syntax (BranchCont (..), Penelope (..), PortSig (..), Region (..))
import Penelope.Effects
import Polytropos (Field (..), Record, Variant, emptyRecord, injectVariant, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (unsafeMkRowMorph)
import Prelude hiding (id, (.))
import Test.Hspec

-- Unary Effect GADTs for testing
data AskEffTest :: Type -> Type where
  AskVal :: AskEffTest Int

data MsgEff :: Type -> Type where
  GetMsg :: MsgEff String
  SetMsg :: String -> MsgEff ()

type AskEF = '["ask" ':= UnaryEffect AskEffTest]
type MsgEF = '["msg" ':= UnaryEffect MsgEff]

-- Variant type for router tests
type EventSum = '["click" ':= '["x" ':= Int], "key" ':= '["char" ':= Char]]

-- Multi-effect rows for Heftia interaction tests
type CoroReaderEF = '["yield" ':= UnaryEffect (YieldEff Int), "reader" ':= UnaryEffect (AskEff Int)]
type CoroCatchEF = '["yield" ':= UnaryEffect (YieldEff String), "error" ':= UnaryEffect (ThrowEff String)]
type NonDetReaderEF = '["nondet" ':= UnaryEffect NonDetEff, "reader" ':= UnaryEffect (AskEff Int)]

main :: IO ()
main = hspec $ do
  describe "Penelope.Effects (Domain Entity)" $ do
    it "constructs default entity with correct properties" $ do
      let e = defaultEntity
      entityName e `shouldBe` "penelope"
      entityModule e `shouldBe` "Penelope.Effects"
    it "renders entity accurately" $ do
      let e = defaultEntity
      renderEntity e `shouldBe` "penelope:Penelope.Effects"

  describe "Penelope.Effects.Unary (Eff Monad Laws)" $ do
    it "satisfies left identity: pure a >>= f == f a" $ do
      let f x = pure (x * 2 :: Int)
          val = 21 :: Int
          lhs = runEff (pure val >>= f)
          rhs = runEff (f val)
      lhs `shouldBe` 42
      rhs `shouldBe` 42
      lhs `shouldBe` rhs

    it "satisfies right identity: m >>= pure == m" $ do
      let m = pure (42 :: Int)
          lhs = runEff (m >>= pure)
          rhs = runEff m
      lhs `shouldBe` 42
      rhs `shouldBe` 42
      lhs `shouldBe` rhs

    it "satisfies associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $ do
      let m = pure (10 :: Int)
          f x = pure (x + 5 :: Int)
          g y = pure (y * 2 :: Int)
          lhs = runEff ((m >>= f) >>= g)
          rhs = runEff (m >>= (\x -> f x >>= g))
      lhs `shouldBe` 30
      rhs `shouldBe` 30
      lhs `shouldBe` rhs

  describe "Penelope.Effects.Unary (Monadic Ergonomics & Do-Notation)" $ do
    it "evaluates multi-step monadic do-notation blocks cleanly" $ do
      let prog :: Eff '[] '[] Int
          prog = do
            a <- pure (10 :: Int)
            b <- pure (20 :: Int)
            c <- pure (12 :: Int)
            pure (a + b + c)
      runEff prog `shouldBe` 42

    it "supports functor fmap and applicative ap" $ do
      let p1 = pure (20 :: Int)
          p2 = fmap (* 2) p1
          p3 = (+) <$> p1 <*> p2
      runEff p2 `shouldBe` 40
      runEff p3 `shouldBe` 60

  describe "Penelope.Effects.Unary (Effectful Runners: runEffM)" $ do
    it "interprets unary effect GADTs via mkEffHandler into Identity" $ do
      let prog :: Eff '[] AskEF Int
          prog = do
            v <- send @"ask" AskVal
            pure (v * 2)

          askHandler :: EffHandler Identity AskEffTest
          askHandler = mkEffHandler $ \case
            AskVal -> Identity (21 :: Int)

          handlers = insertField (Field @"ask") askHandler emptyRecord
          Identity result = runEffM handlers prog
      result `shouldBe` 42

    it "interprets stateful multi-operation GADTs via mkEffHandler into IO" $ do
      ref <- newIORef ("initial" :: String)
      let prog :: Eff '[] MsgEF String
          prog = do
            m <- send @"msg" GetMsg
            send @"msg" (SetMsg "updated")
            pure ("msg:" <> m)

          msgHandler :: EffHandler IO MsgEff
          msgHandler = mkEffHandler $ \case
            GetMsg -> readIORef ref
            SetMsg s -> writeIORef ref s

          handlers = insertField (Field @"msg") msgHandler emptyRecord

      res <- runEffM handlers prog
      res `shouldBe` "msg:initial"
      final <- readIORef ref
      final `shouldBe` "updated"

  describe "Penelope.Effects.Binary (Rope Category and Arrow Laws)" $ do
    it "satisfies Category identity and composition laws" $ do
      let f :: Rope '[] '[] Int Int
          f = arr (+ 1)
          g :: Rope '[] '[] Int Int
          g = arr (* 2)
          h :: Rope '[] '[] Int Int
          h = arr (+ 3)
      runRopePure (id >>> f) 10 `shouldBe` 11
      runRopePure (f >>> id) 10 `shouldBe` 11
      runRopePure ((f >>> g) >>> h) 10 `shouldBe` 25
      runRopePure (f >>> (g >>> h)) 10 `shouldBe` 25

    it "satisfies Arrow first, second, (***), and (&&&) combinators" $ do
      let f :: Rope '[] '[] Int Int
          f = arr (+ 1)
          g :: Rope '[] '[] String String
          g = arr reverse
      runRopePure (first f) (10 :: Int, "hello" :: String) `shouldBe` (11, "hello")
      runRopePure (second g) (10 :: Int, "hello" :: String) `shouldBe` (10, "olleh")
      runRopePure (f *** g) (10 :: Int, "world" :: String) `shouldBe` (11, "dlrow")
      runRopePure (f &&& arr (* 2)) (10 :: Int) `shouldBe` (11, 20)

    it "satisfies ArrowChoice left and right branch routing" $ do
      let f :: Rope '[] '[] Int Int
          f = arr (+ 1)
      runRopePure (left f) (Left 10 :: Either Int String) `shouldBe` Left 11
      runRopePure (left f) (Right "pass" :: Either Int String) `shouldBe` Right "pass"
      runRopePure (right f) (Right 20 :: Either String Int) `shouldBe` Right 21
      runRopePure (right f) (Left "pass" :: Either String Int) `shouldBe` Left "pass"

  describe "Penelope.Effects.Binary (Static Jump-Table deriveRouter)" $ do
    it "dispatches multi-constructor Variant inputs in O(1) via deriveRouter" $ do
      let hClick :: Rope '[] '[] (Record '["x" ':= Int]) String
          hClick = arr $ \r ->
            let !(Identity x) = indexSmallArray (let RowRep (Rep _ arrRep) = r in arrRep) 0
            in "clicked:" <> show (unsafeCoerce# x :: Int)

          hKey :: Rope '[] '[] (Record '["char" ':= Char]) String
          hKey = arr $ \r ->
            let !(Identity c) = indexSmallArray (let RowRep (Rep _ arrRep) = r in arrRep) 0
            in "key:" <> [unsafeCoerce# c :: Char]

          handlers = insertField (Field @"click") hClick
                   $ insertField (Field @"key") hKey
                   $ emptyRecord

          router :: Rope '[] '[] (Variant EventSum) String
          router = deriveRouter handlers

          v1 = injectVariant (Field @"click") (insertField (Field @"x") (42 :: Int) emptyRecord)
          v2 = injectVariant (Field @"key") (insertField (Field @"char") 'q' emptyRecord)

      runRopePure router v1 `shouldBe` "clicked:42"
      runRopePure router v2 `shouldBe` "key:q"

  describe "Penelope.Effects.Control (While, Switch, Saga)" $ do
    it "executes whileEff loop with state termination" $ do
      let loop :: Eff '[] '[] Int
          loop = whileEff (\s -> pure (s < 5)) (\s -> pure (s + 1)) (0 :: Int)
      runEff loop `shouldBe` 5

    it "elaborates and executes multi-region While loop to state termination" $ do
      let condPipe :: Penelope '[] '[] '["x" ':= Int] '["cond" ':= Bool]
          condPipe = LiftPure (unsafeMkRowMorph [0])
            (\r ->
              let !(Identity x) = indexSmallArray (let RowRep (Rep _ sArr) = r in sArr) 0
                  !b = (unsafeCoerce# x :: Int) < 3
              in insertField (Field @"cond") b emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

      let bodyPipe :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          bodyPipe = LiftPure (unsafeMkRowMorph [0])
            (\r ->
              let !(Identity x) = indexSmallArray (let RowRep (Rep _ sArr) = r in sArr) 0
                  !nextX = (unsafeCoerce# x :: Int) + 1
              in insertField (Field @"x") nextX emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

      let regs = insertField (Field @"cond") (Region @'[] @'[] @('PortSig '["x" ':= Int] '["cond" ':= Bool]) condPipe)
               $ insertField (Field @"body") (Region @'[] @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) bodyPipe)
               $ emptyRecord

      let retCont = BranchCont (\_ -> Route (unsafeMkRowMorph [0]))
      let exits = insertField (Field @"exit") retCont emptyRecord

      let whileTerm :: Penelope '["while" ':= While] '[] '["x" ':= Int] '["x" ':= Int]
          whileTerm = OpH 0## (unsafeMkRowMorph [0]) regs (unsafeCoerce# ()) exits

      let elabs = insertField (Field @"while") whileElaborator emptyRecord
          elaborated = elaborateAll elabs whileTerm

      let inFrame = insertField (Field @"x") (0 :: Int) emptyRecord
          outFrame = runPenelopePure elaborated inFrame
          !(Identity finalX) = indexSmallArray (let RowRep (Rep _ sArr) = outFrame in sArr) 0

      (unsafeCoerce# finalX :: Int) `shouldBe` 3

    it "executes transactional Saga committing on success and compensating on failure" $ do
      let prog1 :: Eff '[] '[] (Either String Int)
          prog1 = runSagaEff (pure ("action" :: String)) (\_ -> pure ()) (\_ -> pure (Right 42))

          prog2 :: Eff '[] '[] (Either String Int)
          prog2 = runSagaEff (pure ("action" :: String)) (\_ -> pure ()) (\_ -> pure (Left "err"))

      runEff prog1 `shouldBe` Right 42
      runEff prog2 `shouldBe` Left "err"

  describe "Penelope.Effects.Coroutine (Delimited Continuations & Generator)" $ do
    it "yields elements from generator to consumer cleanly" $ do
      let gen :: Eff '[] '["yield" ':= UnaryEffect (YieldEff Int)] String
          gen = do
            yield @"yield" (10 :: Int)
            yield @"yield" (20 :: Int)
            yield @"yield" (30 :: Int)
            pure "finished"
          (yielded, res) = runGenerator gen
      yielded `shouldBe` [10, 20, 30]
      res `shouldBe` "finished"

    it "runs non-deterministic branching exploration via runNonDet" $ do
      let prog :: Eff '[] '["nondet" ':= UnaryEffect NonDetEff] Int
          prog = do
            b1 <- branch @"nondet"
            b2 <- branch @"nondet"
            case (b1, b2) of
              (True, True)   -> pure 10
              (True, False)  -> pure 20
              (False, True)  -> pure 30
              (False, False) -> emptyNonDet @"nondet"
      runNonDet prog `shouldBe` [10, 20, 30]

  describe "Penelope.Effects.UnliftIO (Scoped Resource Safety & Provider)" $ do
    it "guarantees Bracket resource cleanup on success exit" $ do
      ref <- newIORef ([] :: [String])
      let acquire = do
            writeIORef ref ["acquired"]
            pure ("handle" :: String)
          release _ =
            modifyIORef' ref ("released" :)
          action _ = do
            modifyIORef' ref ("action" :)
            pure (42 :: Int)

      res <- bracketIO acquire release action
      res `shouldBe` 42
      logRes <- readIORef ref
      logRes `shouldBe` ["released", "action", "acquired"]

    it "guarantees Bracket resource cleanup on error exit" $ do
      ref <- newIORef ([] :: [String])
      let acquire = do
            writeIORef ref ["acquired"]
            pure ("handle" :: String)
          release _ =
            modifyIORef' ref ("released" :)
          actionErr _ = do
            modifyIORef' ref ("action-failed" :)
            error "simulated failure" :: IO Int

      (bracketIO acquire release actionErr >> pure ()) `shouldThrow` anyErrorCall
      logRes <- readIORef ref
      logRes `shouldBe` ["released", "action-failed", "acquired"]

    it "supports pure bracketEitherEff error exit" $ do
      let prog = bracketEitherEff (pure ("res" :: String)) (\_ -> pure ()) (\_ -> pure (Left "boom" :: Either String Int))
      runEff prog `shouldBe` Left "boom"

    it "injects dependencies via Provider purely" $ do
      let prog :: Eff '[] '["provider" ':= UnaryEffect (ProviderEff String)] String
          prog = do
            env <- askProvider @"provider"
            pure ("env:" <> env)
      runProviderPure "production" prog `shouldBe` "env:production"

  describe "Penelope.Effects.Scoped (Reader / Local)" $ do
    it "reads lexical environment purely via askReader and runReaderPure" $ do
      let prog :: Eff '[] '["reader" ':= UnaryEffect (AskEff Int)] Int
          prog = do
            v <- askReader
            pure (v * 2)
      runReaderPure (21 :: Int) prog `shouldBe` 42

    it "modifies environment in nested local blocks and restores state cleanly" $ do
      let prog :: Eff '[] '["reader" ':= UnaryEffect (AskEff Int)] (Int, Int, Int)
          prog = do
            v0 <- askReader
            v1 <- local @Int (+ 10) $ do
              inner1 <- askReader
              inner2 <- local @Int (* 2) askReader
              pure (inner1 + inner2)
            v2 <- askReader
            pure (v0, v1, v2)
      -- With initial 5:
      -- v0 = 5
      -- inner1 = 15, inner2 = (5 + 10) * 2 = 30 -> v1 = 45
      -- v2 = 5
      runReaderPure (5 :: Int) prog `shouldBe` (5, 45, 5)

    it "unrolls higher-order Local effect via localElaborator and elaborateAll" $ do
      let bodyPipe :: Penelope '[] '["reader" ':= UnaryEffect (AskEff Int)] '[] '["val" ':= Int]
          bodyPipe = unEff $ do
            x <- askReader
            pure (x + 100)

      let regs = insertField (Field @"body") (Region @'[] @'["reader" ':= UnaryEffect (AskEff Int)] @('PortSig '[] '["val" ':= Int]) bodyPipe) emptyRecord
      let retCont = BranchCont (\_outF -> Route (unsafeMkRowMorph [0]))
      let exits = insertField (Field @"ret") retCont emptyRecord

      let localOpH :: Penelope '["local" ':= Local Int] '["reader" ':= UnaryEffect (AskEff Int)] '[] '["val" ':= Int]
          localOpH = OpH 0## (unsafeMkRowMorph []) regs (unsafeCoerce# ((* 2) :: Int -> Int)) exits

      let elabs = insertField (Field @"local") (localElaborator @"reader") emptyRecord
      let elaborated = elaborateAll elabs localOpH

      let handlers = insertField (Field @"reader") (readerHandler (10 :: Int)) emptyRecord
      let Identity outFrame = runFrameKleisli (interpretAll handlers elaborated) emptyRecord
      let !(Identity resVal) = indexSmallArray (let RowRep (Rep _ arrRep) = outFrame in arrRep) 0
      -- Initial 10 * 2 = 20, plus 100 = 120
      (unsafeCoerce# resVal :: Int) `shouldBe` 120

  describe "Penelope.Effects.Scoped (Error / Catch)" $ do
    it "evaluates successful computations to Right with runErrorPure" $ do
      let prog :: Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] Int
          prog = pure 42
      runErrorPure prog `shouldBe` Right 42

    it "aborts on throw with Left error message" $ do
      let prog :: Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] Int
          prog = do
            _ <- pure (10 :: Int)
            throw ("aborted-computation" :: String)
      runErrorPure prog `shouldBe` Left "aborted-computation"

    it "catches and recovers from errors via catchError" $ do
      let prog :: Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] String
          prog = do
            catchError
              (do
                _ <- pure (1 :: Int)
                throw ("recoverable-error" :: String)
              )
              (\e -> pure ("caught:" <> e))
      runErrorPure prog `shouldBe` Right "caught:recoverable-error"

    it "unrolls higher-order Catch effect via catchElaborator and elaborateAll" $ do
      let bodyPipe :: Penelope '[] '["error" ':= UnaryEffect (ThrowEff String)] '[] '["val" ':= String]
          bodyPipe = unEff $ do
            _ <- throw ("scoped-error-thrown" :: String)
            pure "unreachable"

      let handlerPipe :: Penelope '[] '["error" ':= UnaryEffect (ThrowEff String)] '[] '["val" ':= String]
          handlerPipe = unEff (pure "intercepted-by-elaborator")

      let regs = insertField (Field @"body") (Region @'[] @'["error" ':= UnaryEffect (ThrowEff String)] @('PortSig '[] '["val" ':= String]) bodyPipe)
               $ insertField (Field @"handler") (Region @'[] @'["error" ':= UnaryEffect (ThrowEff String)] @('PortSig '[] '["val" ':= String]) handlerPipe)
               $ emptyRecord
      let retCont = BranchCont (\_outF -> Route (unsafeMkRowMorph [0]))
      let exits = insertField (Field @"ret") retCont emptyRecord

      let catchOpH :: Penelope '["catch" ':= Catch String] '["error" ':= UnaryEffect (ThrowEff String)] '[] '["val" ':= String]
          catchOpH = OpH 0## (unsafeMkRowMorph []) regs (unsafeCoerce# ()) exits

      let elabs = insertField (Field @"catch") (catchElaborator @"error") emptyRecord
      let elaborated = elaborateAll elabs catchOpH

      let handlers = insertField (Field @"error") errorHandler emptyRecord
      let res = runFrameKleisli (interpretAll handlers elaborated) emptyRecord
      case res of
        Right outFrame -> do
          let !(Identity resVal) = indexSmallArray (let RowRep (Rep _ arrRep) = outFrame in arrRep) 0
          (unsafeCoerce# resVal :: String) `shouldBe` "intercepted-by-elaborator"
        Left e ->
          expectationFailure ("Unexpected unhandled error: " <> show e)

  describe "Penelope.Effects.Concurrent (Async & Task Isolation)" $ do
    it "forks background IO task via forkEff" $ do
      mvar <- C.newEmptyMVar
      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] ()
          prog = forkEff @"concurrent" (C.putMVar mvar ("worker-done" :: String))
      runConcurrentIO prog
      res <- C.takeMVar mvar
      res `shouldBe` "worker-done"

    it "runs asynchronous tasks concurrently and awaits results via waitEff" $ do
      let task1 = do
            C.threadDelay 2000
            pure (10 :: Int)
          task2 = do
            C.threadDelay 1000
            pure (20 :: Int)

      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] (Int, Int)
          prog = do
            h1 <- async task1
            h2 <- async task2
            r1 <- wait h1
            r2 <- wait h2
            pure (r1, r2)

      res <- runConcurrentIO prog
      res `shouldBe` (10, 20)

    it "propagates exceptions from failed asynchronous tasks safely" $ do
      let failingTask = do
            E.throwIO (userError "async-failure") :: IO Int

      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] Int
          prog = do
            h <- async failingTask
            wait h

      runConcurrentIO prog `shouldThrow` anyException

  describe "Heftia Scoped Semantics & Multi-Shot Interaction" $ do
    it "preserves local reader environment across coroutine yield suspensions" $ do
      ref <- newIORef ([] :: [Int])
      let prog :: Eff '[] CoroReaderEF ()
          prog = do
            yield @"yield" (1 :: Int)
            local @Int (+ 100) $ do
              yield @"yield" (2 :: Int)
              x <- ask @"reader"
              yield @"yield" x
            yield @"yield" (3 :: Int)
            y <- ask @"reader"
            yield @"yield" y

      let yHandler :: EffHandler IO (YieldEff Int)
          yHandler = mkEffHandler $ \(Yield val) ->
            modifyIORef' ref (val :)

          rHandler :: EffHandler IO (AskEff Int)
          rHandler = readerHandler (10 :: Int)

          handlers = insertField (Field @"yield") yHandler
                   $ insertField (Field @"reader") rHandler
                   $ emptyRecord

      _ <- runEffM handlers prog
      yielded <- readIORef ref
      reverse yielded `shouldBe` [1, 2, 110, 3, 10]

    it "catches errors thrown during coroutine execution and resumes yielding downstream" $ do
      ref <- newIORef ([] :: [String])
      let prog :: Eff '[] CoroCatchEF ()
          prog = do
            yield @"yield" ("start" :: String)
            catchError
              (do
                yield @"yield" ("in-try" :: String)
                throwError @"error" ("simulated-failure" :: String)
                yield @"yield" ("unreachable" :: String)
              )
              (\e -> do
                yield @"yield" ("recovered:" <> e :: String)
              )
            yield @"yield" ("after-catch" :: String)

      let yHandler :: EffHandler IO (YieldEff String)
          yHandler = mkEffHandler $ \(Yield val) ->
            modifyIORef' ref (val :)

          eHandler :: EffHandler IO (ThrowEff String)
          eHandler = mkEffHandler $ \(Throw e) ->
            fail ("Unhandled throw caught by carrier: " <> e)

          handlers = insertField (Field @"yield") yHandler
                   $ insertField (Field @"error") eHandler
                   $ emptyRecord

      _ <- runEffM handlers prog
      yielded <- readIORef ref
      reverse yielded `shouldBe` ["start", "in-try", "recovered:simulated-failure", "after-catch"]

    it "maintains complete environment isolation across multi-shot non-deterministic branches" $ do
      let prog :: Eff '[] NonDetReaderEF Int
          prog = do
            b <- branch @"nondet"
            if b
              then local @Int (+ 10) (ask @"reader")
              else local @Int (+ 20) (ask @"reader")

      let ndHandler :: EffHandler [] NonDetEff
          ndHandler = nonDetHandler

          rHandler :: EffHandler [] (AskEff Int)
          rHandler = readerHandler (5 :: Int)

          handlers = insertField (Field @"nondet") ndHandler
                   $ insertField (Field @"reader") rHandler
                   $ emptyRecord

      let results = runEffM handlers prog
      results `shouldBe` [15, 25]

    it "guarantees balanced resource acquisition and cleanup across multiple concurrent branches" $ do
      ref <- newIORef ([] :: [String])
      let acquire name = do
            modifyIORef' ref ((name <> "-acq") :)
            pure name
          release name = do
            modifyIORef' ref ((name <> "-rel") :)

      let task1 = bracketIO (acquire "worker1") release (\n -> pure (n <> "-done"))
          task2 = bracketIO (acquire "worker2") release (\n -> pure (n <> "-done"))

      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] (String, String)
          prog = do
            h1 <- async task1
            h2 <- async task2
            r1 <- wait h1
            r2 <- wait h2
            pure (r1, r2)

      res <- runConcurrentIO prog
      res `shouldBe` ("worker1-done", "worker2-done")

      logEntries <- readIORef ref
      logEntries `shouldContain` ["worker1-acq"]
      logEntries `shouldContain` ["worker1-rel"]
      logEntries `shouldContain` ["worker2-acq"]
      logEntries `shouldContain` ["worker2-rel"]
