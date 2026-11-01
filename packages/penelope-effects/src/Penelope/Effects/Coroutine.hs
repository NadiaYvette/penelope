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
Module      : Penelope.Effects.Coroutine
Description : Delimited continuations, frame suspension, and non-determinism
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides delimited continuations with frame suspension and resumption:
1. 'Coroutine' effect with 'Yield' and 'Await' primitives
2. Generator execution via 'runGenerator' and 'runGeneratorIO'
3. 'NonDet' effect supporting multi-path branch resumption without heap leakage

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Coroutine
  ( -- * Coroutine & Suspension
    Coroutine
  , Suspension (..)
  , YieldEff (..)
  , AwaitEff (..)
  , yield
  , await

    -- * Generator Runners
  , runGenerator
  , runGeneratorIO

    -- * Non-Determinism
  , NonDet
  , NonDetEff (..)
  , branch
  , emptyNonDet
  , nonDetHandler
  , runNonDet
  ) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Penelope.Core.Syntax (PolyEffect)
import Penelope.Effects.Unary
  ( Eff (..)
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Pair (..))

{- |
Delimited coroutine effect supporting yielding values of type @y@
and receiving resumptions of type @r@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Coroutine (y :: Type) (r :: Type) :: PolyEffect

{- |
Status of a suspended coroutine pipeline.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Suspension y r a
  = Done a
  | Suspended y (r -> Suspension y r a)

{- |
Unary effect GADT representing a yield operation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data YieldEff (y :: Type) (a :: Type) where
  Yield :: y -> YieldEff y ()

{- |
Unary effect GADT representing an await operation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data AwaitEff (y :: Type) (a :: Type) where
  Await :: AwaitEff y y

{- |
Yield a value of type @y@ from within an 'Eff' computation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE yield #-}
yield ::
  forall (l :: Symbol) y eh ef.
  ConstructorIndex l ef =>
  y
  -> Eff eh ef ()
yield y = send @l (Yield y)

{- |
Await a value of type @y@ from an enclosing producer.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE await #-}
await ::
  forall (l :: Symbol) y eh ef.
  ConstructorIndex l ef =>
  Eff eh ef y
await = send @l Await

-- | Simple internal state monad for pure generator evaluation.
newtype GenState s a = GenState { runGenState :: s -> (a, s) }

instance Functor (GenState s) where
  {-# INLINE fmap #-}
  fmap f (GenState g) = GenState $ \s ->
    let (a, s') = g s in (f a, s')

instance Applicative (GenState s) where
  {-# INLINE pure #-}
  pure a = GenState $ \s -> (a, s)
  {-# INLINE (<*>) #-}
  GenState gf <*> GenState ga = GenState $ \s ->
    let (f, s1) = gf s
        (a, s2) = ga s1
    in (f a, s2)

instance Monad (GenState s) where
  {-# INLINE (>>=) #-}
  GenState g >>= k = GenState $ \s ->
    let (a, s1) = g s
    in runGenState (k a) s1

{- |
Execute a generator 'Eff' computation, returning the list of yielded items
along with the final return value in a pure pair @([y], a)@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runGenerator #-}
runGenerator ::
  forall y a.
  Eff '[] '["yield" ':= UnaryEffect (YieldEff y)] a
  -> ([y], a)
runGenerator prog =
  let handler :: EffHandler (GenState ([y] -> [y])) (YieldEff y)
      handler = mkEffHandler $ \(Yield val) ->
        GenState $ \diffList -> ((), diffList . (val :))

      handlers = insertField (Field @"yield") handler emptyRecord
      GenState stAction = runEffM handlers prog
      (finalVal, diffRes) = stAction id
  in (diffRes [], finalVal)

{- |
Execute a generator 'Eff' computation under 'IO' using an 'IORef'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runGeneratorIO #-}
runGeneratorIO ::
  forall y a.
  Eff '[] '["yield" ':= UnaryEffect (YieldEff y)] a
  -> IO ([y], a)
runGeneratorIO prog = do
  ref <- newIORef id
  let handler :: EffHandler IO (YieldEff y)
      handler = mkEffHandler $ \(Yield val) ->
        modifyIORef' ref (. (val :))

      handlers = insertField (Field @"yield") handler emptyRecord
  res <- runEffM handlers prog
  diffRes <- readIORef ref
  pure (diffRes [], res)

{- |
Polycategorical non-determinism effect.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data NonDet :: PolyEffect

{- |
Unary effect GADT representing non-deterministic choice points.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data NonDetEff (a :: Type) where
  Branch :: NonDetEff Bool
  Empty  :: NonDetEff a

{- |
Emit a binary choice point in 'Eff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE branch #-}
branch ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  Eff eh ef Bool
branch = send @l Branch

{- |
Abort the current non-deterministic branch.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE emptyNonDet #-}
emptyNonDet ::
  forall (l :: Symbol) a eh ef.
  ConstructorIndex l ef =>
  Eff eh ef a
emptyNonDet = send @l Empty

{- |
Handler for 'NonDetEff' into the list monad '[]'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE nonDetHandler #-}
nonDetHandler :: EffHandler [] NonDetEff
nonDetHandler = mkEffHandler $ \case
  Branch -> [True, False]
  Empty  -> []

{- |
Execute a non-deterministic 'Eff' computation, returning all valid execution paths.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runNonDet #-}
runNonDet ::
  Eff '[] '["nondet" ':= UnaryEffect NonDetEff] a
  -> [a]
runNonDet prog =
  let handlers = insertField (Field @"nondet") nonDetHandler emptyRecord
  in runEffM handlers prog
