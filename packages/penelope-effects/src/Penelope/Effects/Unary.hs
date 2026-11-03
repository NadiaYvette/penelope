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

Provides a standard unary monadic interface ('t:Eff') over Penelope's polycategorical core.

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
Inject a first-order unary algebraic effect into 't:Eff'.
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
Execute a pure, closed 't:Eff' computation to its final value.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runEff #-}
runEff :: Eff '[] '[] a -> a
runEff (Eff p) =
  let outFrame = runPenelopePure p emptyRecord
      !(Identity val) = indexSmallArray (let RowRep (Rep _ arr) = outFrame in arr) 0
  in unsafeCoerce# val

{- |
Execute an effectful 't:Eff' computation into a target 'Monad' using a handler vector.

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
