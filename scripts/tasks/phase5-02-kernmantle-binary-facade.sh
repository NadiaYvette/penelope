#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase5-02-kernmantle-binary-facade
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 5.2: Kernmantle Binary Arrow Facade (Rope) ==="

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
    Penelope.Effects.Binary
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
CABAL_EOF

# 2. Write packages/penelope-effects/src/Penelope/Effects/Binary.hs
cat <<'BINARY_EOF' > packages/penelope-effects/src/Penelope/Effects/Binary.hs
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
Module      : Penelope.Effects.Binary
Description : Kernmantle binary arrow facade over Penelope (m,n,k,r)-ary core
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a binary arrow interface ('Rope') and static jump-table routing ('deriveRouter')
over Penelope's polycategorical core.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Binary
  ( -- * Binary Arrow Facade
    Rope (..)

    -- * Jump-Table Router
  , deriveRouter

    -- * Runners
  , runRopePure
  , runRopeM
  ) where

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..), (>>>))
import Data.Functor.Identity (Identity (..))
import Data.Primitive.SmallArray (indexSmallArray)
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import GHC.Exts (Any, unsafeCoerce#)
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
import Penelope.Core.Wiring (seqPenelope)
import Polytropos (Field (..), Record, RowOf, Variant, emptyRecord, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..), SumRow)
import Polytropos.RowMorph (unsafeMkRowMorph)
import Prelude hiding (id, (.))

{- |
Zero-cost binary arrow projection over Penelope's underlying polycategorical engine.
A computation of type @Rope eh ef a b@ is isomorphic to a pipeline with a single input register
@'["in" ':= a]@ and a single output register @'["out" ':= b]@.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
newtype Rope (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) a b = Rope
  { unRope :: Penelope eh ef '["in" ':= a] '["out" ':= b]
  }

instance Category (Rope eh ef) where
  {-# INLINE id #-}
  id = Rope (Route (unsafeMkRowMorph [0]))

  {-# INLINE (.) #-}
  Rope g . Rope f =
    let rename = Route (unsafeMkRowMorph [0])
    in Rope (seqPenelope f (seqPenelope rename g))

instance Arrow (Rope eh ef) where
  {-# INLINE arr #-}
  arr f = Rope $
    LiftPure (unsafeMkRowMorph [0])
      (\rec ->
        let !(Identity aVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
            !b = f (unsafeCoerce# aVal)
        in insertField (Field @"out") b emptyRecord
      )
      (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

  {-# INLINE first #-}
  first = arrowFirst

  {-# INLINE second #-}
  second = arrowSecond

instance ArrowChoice (Rope eh ef) where
  {-# INLINE left #-}
  left = arrowLeft

  {-# INLINE right #-}
  right = arrowRight

instance Profunctor (Rope eh ef) where
  {-# INLINE dimap #-}
  dimap f g r = arr f >>> r >>> arr g

  {-# INLINE lmap #-}
  lmap f r = arr f >>> r

  {-# INLINE rmap #-}
  rmap g r = r >>> arr g

instance Strong (Rope eh ef) where
  {-# INLINE first' #-}
  first' = first

  {-# INLINE second' #-}
  second' = second

instance Choice (Rope eh ef) where
  {-# INLINE left' #-}
  left' = left

  {-# INLINE right' #-}
  right' = right

{-# INLINE arrowFirst #-}
arrowFirst ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (b, d) (c, d)
arrowFirst (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity pairVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(b, d) = unsafeCoerce# pairVal :: (b, d)
          !bIn = insertField (Field @"in") b emptyRecord
          stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= (c, d)]
          stageOut = LiftPure (unsafeMkRowMorph [0])
            (\cRec ->
              let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                  !c = unsafeCoerce# cVal :: c
              in insertField (Field @"out") (c, d) emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
      in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
           (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
    )

{-# INLINE arrowSecond #-}
arrowSecond ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (d, b) (d, c)
arrowSecond (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity pairVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(d, b) = unsafeCoerce# pairVal :: (d, b)
          !bIn = insertField (Field @"in") b emptyRecord
          stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= (d, c)]
          stageOut = LiftPure (unsafeMkRowMorph [0])
            (\cRec ->
              let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                  !c = unsafeCoerce# cVal :: c
              in insertField (Field @"out") (d, c) emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
      in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
           (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
    )

{-# INLINE arrowLeft #-}
arrowLeft ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (Either b d) (Either c d)
arrowLeft (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity eitherVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
      in case (unsafeCoerce# eitherVal :: Either b d) of
        Left b ->
          let !bIn = insertField (Field @"in") b emptyRecord
              stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= Either c d]
              stageOut = LiftPure (unsafeMkRowMorph [0])
                (\cRec ->
                  let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                      !c = unsafeCoerce# cVal :: c
                  in insertField (Field @"out") (Left c :: Either c d) emptyRecord
                )
                (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
          in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
               (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
        Right d ->
          LiftPure (unsafeMkRowMorph [])
            (\_ -> insertField (Field @"out") (Right d :: Either c d) emptyRecord)
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
    )

{-# INLINE arrowRight #-}
arrowRight ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (Either d b) (Either d c)
arrowRight (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity eitherVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
      in case (unsafeCoerce# eitherVal :: Either d b) of
        Right b ->
          let !bIn = insertField (Field @"in") b emptyRecord
              stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= Either d c]
              stageOut = LiftPure (unsafeMkRowMorph [0])
                (\cRec ->
                  let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                      !c = unsafeCoerce# cVal :: c
                  in insertField (Field @"out") (Right c :: Either d c) emptyRecord
                )
                (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
          in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
               (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
        Left d ->
          LiftPure (unsafeMkRowMorph [])
            (\_ -> insertField (Field @"out") (Left d :: Either d c) emptyRecord)
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
    )

{- |
Static jump-table router: compiles a 'Record' of branch handlers into an \(O(1)\)
jump table over 'Polytropos.Variant', eliminating linear branching cascades and nested
coproduct shuffles (@Either (Either a b) c@).

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE deriveRouter #-}
deriveRouter ::
  forall (s :: SumRow) (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) b handlers.
  Record handlers
  -> Rope eh ef (Variant s) b
deriveRouter handlers = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity varVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(RowRep (Rep tag payloadArr)) = unsafeCoerce# varVal
          !(RowRep (Rep _ handlerArr)) = handlers
          !(Identity handlerAny) = indexSmallArray handlerArr (fromIntegral tag)
          !(Rope (pipe :: Penelope eh ef '["in" ':= Any] '["out" ':= b])) = unsafeCoerce# handlerAny
          !payloadRecord = RowRep (Rep 0 payloadArr)
          !payloadIn = insertField (Field @"in") (unsafeCoerce# payloadRecord) emptyRecord
      in LiftPure (unsafeMkRowMorph []) (\_ -> payloadIn)
           (BranchCont (\_ -> unsafeCoerce# pipe))
    )

{- |
Execute a pure, closed 'Rope' arrow to its final output value.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runRopePure #-}
runRopePure :: Rope '[] '[] a b -> a -> b
runRopePure (Rope p) a =
  let inFrame = insertField (Field @"in") a emptyRecord
      outFrame = runPenelopePure p inFrame
      !(Identity bVal) = indexSmallArray (let RowRep (Rep _ sArr) = outFrame in sArr) 0
  in unsafeCoerce# bVal

{- |
Execute an effectful 'Rope' arrow into a target 'Monad' using a handler vector.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runRopeM #-}
runRopeM ::
  forall ef m a b.
  Monad m =>
  Record (MapRow (Handler '[] (FrameKleisli m)) ef)
  -> Rope '[] ef a b
  -> a
  -> m b
runRopeM handlers (Rope p) a = do
  let inFrame = insertField (Field @"in") a emptyRecord
      kleisli = interpretAll handlers p
  outFrame <- runFrameKleisli kleisli inFrame
  let !(Identity bVal) = indexSmallArray (let RowRep (Rep _ sArr) = outFrame in sArr) 0
  pure (unsafeCoerce# bVal)
BINARY_EOF

# 3. Write packages/penelope-effects/src/Penelope/Effects.hs
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
  , module Penelope.Effects.Binary
  ) where

import Data.Text (Text)
import Penelope.Effects.Unary
import Penelope.Effects.Binary

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

# 4. Write packages/penelope-effects/test/Main.hs
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
TEST_EOF

# 5. Build and verify
cabal build penelope-effects
cabal test penelope-effects:test:tests
echo "=== [yamaarashi] Milestone 5.2 implementation script completed successfully ==="
