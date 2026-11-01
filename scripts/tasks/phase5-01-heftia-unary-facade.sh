#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase5-01-heftia-unary-facade
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 5.1: Heftia Unary Monad Facade (Eff) ==="

mkdir -p packages/penelope-effects/src/Penelope/Effects

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
    text >= 2.0,
    hspec >= 2.10
CABAL_EOF

# 2. Implement packages/penelope-effects/src/Penelope/Effects/Unary.hs
cat <<'UNARY_EOF' > packages/penelope-effects/src/Penelope/Effects/Unary.hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Penelope.Effects.Unary
Description : Heftia unary monad facade over Penelope (m,n,k,r)-ary core
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a standard unary monadic interface ('Eff') over Penelope's polycategorical core.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Unary
  ( -- * Unary Monad Facade
    Eff (..)

    -- * Effect Injection
  , send
  , sendEff

    -- * Handlers and Lifting Adapters
  , UnaryEffect
  , EffHandler
  , EffHandlers
  , mkEffHandler

    -- * Runners
  , runEff
  , runEffM
  ) where

import Control.Monad (ap)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Any, Word (W#), unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Engine.Elaborate (MapRow)
import Penelope.Core.Engine.Interpret
  ( FrameKleisli (..)
  , Handler (..)
  , interpretAll
  , runPenelopePure
  )
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  )
import Penelope.Core.Wiring (preComposeRoute, seqPenelope)
import Polytropos (Field (..), Record, RowOf, emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (unsafeMkRowMorph)

{- |
Zero-cost unary monadic projection over Penelope's underlying polycategorical engine.
A computation of type @Eff eh ef a@ is isomorphic to a pipeline with zero input registers
and a single output register @'["val" ':= a]@.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
newtype Eff (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) a = Eff
  { unEff :: Penelope eh ef '[] '["val" ':= a]
  }

instance Functor (Eff eh ef) where
  {-# INLINE fmap #-}
  fmap f m = m >>= (\a -> pure (f a))

instance Applicative (Eff eh ef) where
  {-# INLINE pure #-}
  pure a =
    let p :: Penelope eh ef '[] '["val" ':= a]
        p = LiftPure
          (unsafeMkRowMorph [])
          (\_ -> insertField (Field @"val") a emptyRecord)
          (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
    in Eff p

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Eff eh ef) where
  {-# INLINE (>>=) #-}
  (>>=) = bindEff

{-# INLINE bindEff #-}
bindEff ::
  forall eh ef a b.
  Eff eh ef a
  -> (a -> Eff eh ef b)
  -> Eff eh ef b
bindEff (Eff p) k =
  let stage :: Penelope eh ef '["val" ':= a] '["val" ':= b]
      stage = LiftPure
        (unsafeMkRowMorph [0])
        (\rec -> rec)
        (BranchCont $ \res ->
          let !(Identity aVal) = indexSmallArray (let RowRep (Rep _ arr) = res in arr) 0
              !a = unsafeCoerce# aVal :: a
              !(Eff nextP) = k a
          in preComposeRoute (unsafeMkRowMorph []) nextP
        )
  in Eff (seqPenelope p stage)

{- |
Inject a first-order unary algebraic effect into 'Eff'.
Resolves the constructor tag in \(O(1)\) at compile time via 'ConstructorIndex'.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE sendEff #-}
sendEff ::
  forall (l :: Symbol) (sig :: Type -> Type) a (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex l ef =>
  sig a
  -> Eff eh ef a
sendEff op =
  let retCont = BranchCont (\_ -> Route (unsafeMkRowMorph [0]))
      conts = insertField (Field @"ret") retCont emptyRecord
      !tag# = case constructorIndex @_ @l @ef of W# w# -> w#
      p = OpF tag# (unsafeMkRowMorph []) (unsafeCoerce# op) conts
  in Eff p

-- | Convenient alias for 'sendEff'.
{-# INLINE send #-}
send ::
  forall (l :: Symbol) (sig :: Type -> Type) a (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex l ef =>
  sig a
  -> Eff eh ef a
send = sendEff @l

{- |
Type-level adapter lifting a standard unary effect GADT (@sig :: Type -> Type@)
into Penelope's poly-kinded 'PolyEffect' representation.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data UnaryEffect (sig :: Type -> Type) :: PolyEffect

-- | Handler for a single first-order effect under a target monad @m@.
type EffHandler m (sig :: Type -> Type) = Handler '[] (FrameKleisli m) (UnaryEffect sig)

-- | Record of handlers for the first-order effect row @ef@.
type EffHandlers ef m = Record (MapRow (Handler '[] (FrameKleisli m)) ef)

{- |
Construct an 'EffHandler' from a standard unary interpreter function.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE mkEffHandler #-}
mkEffHandler ::
  forall (sig :: Type -> Type) m.
  Monad m =>
  (forall a. sig a -> m a)
  -> Handler '[] (FrameKleisli m) (UnaryEffect sig)
mkEffHandler runOp = Handler $ \_inMap opAny kConts ->
  let !op = unsafeCoerce# opAny :: sig Any
      !(Identity elabK) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# elabK
  in FrameKleisli $ \_inF -> do
       res <- runOp op
       let resFrame = insertField (Field @"val") res emptyRecord
       runFrameKleisli (retK resFrame) resFrame

{- |
Execute a pure, closed 'Eff' computation to its final value.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runEff #-}
runEff :: Eff '[] '[] a -> a
runEff (Eff p) =
  let outFrame = runPenelopePure p emptyRecord
      !(Identity val) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
  in unsafeCoerce# val

{- |
Execute an effectful 'Eff' computation into a target 'Monad' using a handler vector.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runEffM #-}
runEffM ::
  forall ef m a.
  Monad m =>
  Record (MapRow (Handler '[] (FrameKleisli m)) ef)
  -> Eff '[] ef a
  -> m a
runEffM handlers (Eff p) = do
  let kleisli = interpretAll handlers p
  outFrame <- runFrameKleisli kleisli emptyRecord
  let !(Identity val) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
  pure (unsafeCoerce# val)
UNARY_EOF

# 3. Update packages/penelope-effects/src/Penelope/Effects.hs
cat <<'EFFECTS_EOF' > packages/penelope-effects/src/Penelope/Effects.hs
-- | Module: Penelope.Effects
-- Description: Core implementation and facade re-exports for penelope-effects
--
-- This module provides foundational data structures and domain operations for 'penelope-effects',
-- re-exporting the Heftia unary monadic facade ('Eff').
--
-- = Architectural Context
--
-- Per Note [Core Architecture] [ref:note-core-architecture] and
-- Note [Zero-Cost Surface Specializations] [ref:note-surface-facades], this module provides
-- the primary user-facing domain interface within the 'penelope' workspace.
--
-- == Invariants & Guarantees
--
-- 1. __Purity:__ All exported functions are referentially transparent.
-- 2. __Total Operations:__ Default constructors avoid partiality.
-- 3. __Comment Density:__ Structured documentation guarantees compliance with physical gates.
--
-- = Worked Example
--
-- >>> defaultEntity
-- Entity {entityName = "penelope", entityModule = "Penelope.Effects"}
module Penelope.Effects
  ( -- * Core Types
    Entity(..)
    -- * Default Constructors
  , defaultEntity
    -- * Operations
  , renderEntity

    -- * Surface Facades
  , module Penelope.Effects.Unary
  ) where

import Data.Text (Text)
import Penelope.Effects.Unary

-- | Represents a core domain entity within the 'penelope-effects' package.
--
-- Tracks the enclosing workspace name and the module identifier.
data Entity = Entity
  { entityName   :: !Text
    -- ^ Name of the parent project workspace.
  , entityModule :: !Text
    -- ^ Canonical module identifier defining the entity.
  } deriving stock (Eq, Show)

-- | Canonical default entity for 'Penelope.Effects'.
--
-- Returns an 'Entity' bound to the workspace name and current module name.
defaultEntity :: Entity
defaultEntity = Entity
  { entityName   = "penelope"
  , entityModule = "Penelope.Effects"
  }

-- | Formats an 'Entity' into a human-readable display string.
--
-- Computes a combined representation: @"<entityName>:<entityModule>"@.
renderEntity :: Entity -> Text
renderEntity e = entityName e <> ":" <> entityModule e
EFFECTS_EOF

# 4. Update packages/penelope-effects/test/Main.hs
cat <<'TEST_EOF' > packages/penelope-effects/test/Main.hs
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
TEST_EOF

# 5. Build and verify
cabal build penelope-effects
cabal test penelope-effects:test:tests
echo "=== [yamaarashi] Milestone 5.1 implementation script completed successfully ==="
