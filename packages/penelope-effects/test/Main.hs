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

import Data.Functor.Identity (Identity (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Penelope.Effects
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (Pair (..))
import Test.Hspec

-- Unary Effect GADTs for testing
data AskEff :: Type -> Type where
  AskVal :: AskEff Int

data MsgEff :: Type -> Type where
  GetMsg :: MsgEff String
  SetMsg :: String -> MsgEff ()

type AskEF = '["ask" ':= UnaryEffect AskEff]
type MsgEF = '["msg" ':= UnaryEffect MsgEff]

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
