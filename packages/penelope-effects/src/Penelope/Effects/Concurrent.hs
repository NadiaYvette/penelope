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
Module      : Penelope.Effects.Concurrent
Description : Asynchronous concurrency, task isolation, and join handles
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides concurrent task execution, background worker forking, and asynchronous
result joining with full exception safety and task isolation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Concurrent
  ( -- * Concurrent Effect & Handles
    ConcurrentEff (..)
  , AsyncHandle (..)

    -- * Surface Combinators
  , forkEff
  , asyncEff
  , waitEff
  , fork
  , async
  , wait

    -- * Handlers & Runners
  , concurrentHandler
  , runConcurrentIO
  ) where

import qualified Control.Concurrent as C
import qualified Control.Exception as E
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
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
Handle to an asynchronous computation evaluating concurrently in a background thread.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
newtype AsyncHandle a = AsyncHandle
  { unAsyncHandle :: C.MVar (Either E.SomeException a)
  }

{- |
Unary effect GADT representing concurrency primitives.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data ConcurrentEff (a :: Type) where
  ForkAction  :: IO () -> ConcurrentEff ()
  AsyncAction :: IO a -> ConcurrentEff (AsyncHandle a)
  WaitAction  :: AsyncHandle a -> ConcurrentEff a

{- |
Fork a background IO action from within 'Eff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE forkEff #-}
forkEff ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  IO ()
  -> Eff eh ef ()
forkEff action = send @l (ForkAction action)

{- |
Start an asynchronous task in a lightweight GHC thread.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE asyncEff #-}
asyncEff ::
  forall (l :: Symbol) a eh ef.
  ConstructorIndex l ef =>
  IO a
  -> Eff eh ef (AsyncHandle a)
asyncEff action = send @l (AsyncAction action)

{- |
Wait for an asynchronous task to complete, returning its result
or rethrowing any uncaught exception.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE waitEff #-}
waitEff ::
  forall (l :: Symbol) a eh ef.
  ConstructorIndex l ef =>
  AsyncHandle a
  -> Eff eh ef a
waitEff handle = send @l (WaitAction handle)

{- |
Convenient alias for 'forkEff' defaulting to label @"concurrent"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE fork #-}
fork ::
  forall eh ef.
  ConstructorIndex "concurrent" ef =>
  IO ()
  -> Eff eh ef ()
fork = forkEff @"concurrent"

{- |
Convenient alias for 'asyncEff' defaulting to label @"concurrent"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE async #-}
async ::
  forall a eh ef.
  ConstructorIndex "concurrent" ef =>
  IO a
  -> Eff eh ef (AsyncHandle a)
async = asyncEff @"concurrent"

{- |
Convenient alias for 'waitEff' defaulting to label @"concurrent"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE wait #-}
wait ::
  forall a eh ef.
  ConstructorIndex "concurrent" ef =>
  AsyncHandle a
  -> Eff eh ef a
wait = waitEff @"concurrent"

{- |
Construct an 'EffHandler' interpreting 'ConcurrentEff' under 'IO'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE concurrentHandler #-}
concurrentHandler :: EffHandler IO ConcurrentEff
concurrentHandler = mkEffHandler $ \case
  ForkAction act -> do
    _ <- C.forkIO act
    pure ()
  AsyncAction act -> do
    mvar <- C.newEmptyMVar
    _ <- C.forkIO $ do
      res <- E.try act
      C.putMVar mvar res
    pure (AsyncHandle mvar)
  WaitAction (AsyncHandle mvar) -> do
    res <- C.readMVar mvar
    case res of
      Left ex -> E.throwIO ex
      Right val -> pure val

{- |
Execute a concurrent 'Eff' computation under 'IO'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runConcurrentIO #-}
runConcurrentIO ::
  Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] a
  -> IO a
runConcurrentIO prog =
  let handlers = insertField (Field @"concurrent") concurrentHandler emptyRecord
  in runEffM handlers prog
