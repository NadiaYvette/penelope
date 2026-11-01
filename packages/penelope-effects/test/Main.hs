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
import Data.Functor.Identity (Identity (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (unsafeCoerce#)
import Penelope.Effects
import Polytropos (Field (..), Record, Variant, emptyRecord, injectVariant, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..))
import Prelude hiding (id, (.))
import Test.Hspec

-- Unary Effect GADTs for testing
data AskEff :: Type -> Type where
  AskVal :: AskEff Int

data MsgEff :: Type -> Type where
  GetMsg :: MsgEff String
  SetMsg :: String -> MsgEff ()

type AskEF = '["ask" ':= UnaryEffect AskEff]
type MsgEF = '["msg" ':= UnaryEffect MsgEff]

-- Variant type for router tests
type EventSum = '["click" ':= '["x" ':= Int], "key" ':= '["char" ':= Char]]

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

          askHandler :: EffHandler Identity AskEff
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
