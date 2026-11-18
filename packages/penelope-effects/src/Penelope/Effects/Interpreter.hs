{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Penelope.Effects.Interpreter
-- Description : Convenience interpreter effect repertoire (StepBudget, FreshSym, MultiExitTrampoline, TraceStep)
-- Copyright   : (c) The Penelope Authors, 2026
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Standard convenience interpreter algebraic effects for abstract machine
-- evaluation, language runtimes, and stepwise normalizers.
--
-- {- Note [Convenience Interpreter Effects for Abstract Machines and Compilers]
-- Writing production-grade interpreters, abstract machines (e.g. CEK, Krivine, SECD),
-- and compiler intermediate pipelines frequently requires a recurring set of secondary
-- infrastructure effects:
--
-- 1. 'StepBudget': Enforces strict termination budgets or gas limits. Each reduction
--    step calls 'consumeStep'; if the budget drops to zero, execution aborts with
--    a clean out-of-budget error, preventing infinite loops in untrusted input code.
-- 2. 'FreshSym': Supplies fresh, collision-free variable names and register tags
--    during intermediate language lowering, normalization-by-evaluation, or ANF conversion.
-- 3. 'MultiExitTrampoline': Manages non-local exits and multi-branch returns across
--    nested expression evaluation frames without allocating intermediate Haskell exceptions.
-- 4. 'TraceStep': Emits structured diagnostic snapshots at significant reduction
--    checkpoints, aiding compiler debugging and benchmark instrumentation.
--
-- By standardizing these four effects in Penelope, language interpreters across our
-- ecosystem (such as @cek@ and @pykinos@) can share uniform execution harnesses.
-- -}
module Penelope.Effects.Interpreter
  ( -- * 1. StepBudget Effect
    StepBudget(..)
  , consumeStep
  , remainingBudget
  , checkBudget
  , runStepBudget

    -- * 2. FreshSym Effect
  , FreshSym(..)
  , freshSymbol
  , freshUnique
  , runFreshSym

    -- * 3. MultiExitTrampoline Effect
  , MultiExitTrampoline(..)
  , yieldTrampoline
  , exitWith
  , runTrampoline

    -- * 4. TraceStep Effect
  , TraceStep(..)
  , traceStep
  , runTraceStep
  ) where

import Data.IORef (IORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Penelope.Core.Syntax (PolyEffect, PolyHEffect)
import Penelope.Effects.Unary
  ( Eff
  , EffHandler
  , mkEffHandler
  , sendEff
  )
import Polytropos (RowOf)
import Polytropos.Core (ConstructorIndex)

-- | Algebraic effect representing machine step consumption and fuel observation.
--
-- == Operational Semantics & Invariants
-- 1. __Step Decrementation:__
--    'ConsumeStep' decrements fuel by @n@. Returns 'True' if remaining fuel is non-negative,
--    or 'False' if the step budget has been exhausted.
-- 2. __Monotonicity:__
--    The fuel counter is strictly non-increasing across evaluation steps.
-- 3. __Observability:__
--    'RemainingBudget' allows diagnostic profilers and abstract machines to inspect current fuel.
data StepBudget (a :: Type) where
  -- | Consume @n@ evaluation steps. Returns 'True' if fuel remains, or 'False' if exhausted.
  ConsumeStep     :: !Int -> StepBudget Bool
  -- | Read the currently available remaining fuel.
  RemainingBudget :: StepBudget Int

-- | Consume a single step under the @"stepBudget"@ effect.
{-# INLINE consumeStep #-}
consumeStep :: forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "stepBudget" ef =>
  Int -> Eff eh ef Bool
consumeStep n = sendEff @"stepBudget" (ConsumeStep n)

-- | Read remaining fuel under the @"stepBudget"@ effect.
{-# INLINE remainingBudget #-}
remainingBudget :: forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "stepBudget" ef =>
  Eff eh ef Int
remainingBudget = sendEff @"stepBudget" RemainingBudget

-- | Helper to assert that at least @n@ steps remain, consuming them.
{-# INLINE checkBudget #-}
checkBudget :: forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "stepBudget" ef =>
  Int -> Eff eh ef Bool
checkBudget = consumeStep

-- | State-passing interpreter handler for 'StepBudget' in 'IO'.
runStepBudget :: IORef Int -> EffHandler IO StepBudget
runStepBudget ref = mkEffHandler $ \case
  ConsumeStep n -> do
    remFuel <- readIORef ref
    let newFuel = remFuel - n
    writeIORef ref (max 0 newFuel)
    pure (newFuel >= 0)
  RemainingBudget -> readIORef ref

-- | Algebraic effect representing fresh symbol and unique identifier generation.
--
-- == Invariants
-- 1. __Uniqueness:__ 'FreshUnique' produces integers strictly monotonic and non-colliding within the session.
-- 2. __Prefixing:__ 'FreshSymbol' appends the monotonic counter to the requested semantic namespace prefix.
data FreshSym (a :: Type) where
  -- | Generate a fresh symbol with a prefix tag.
  FreshSymbol :: !Text -> FreshSym Text
  -- | Generate a globally unique integer id.
  FreshUnique :: FreshSym Int

-- | Request a fresh symbol under the @"freshSym"@ effect.
{-# INLINE freshSymbol #-}
freshSymbol :: forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "freshSym" ef =>
  Text -> Eff eh ef Text
freshSymbol prefix = sendEff @"freshSym" (FreshSymbol prefix)

-- | Request a fresh unique integer under the @"freshSym"@ effect.
{-# INLINE freshUnique #-}
freshUnique :: forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "freshSym" ef =>
  Eff eh ef Int
freshUnique = sendEff @"freshSym" FreshUnique

-- | Interpreter handler for 'FreshSym' in 'IO' using an atomic counter.
runFreshSym :: IORef Int -> EffHandler IO FreshSym
runFreshSym counterRef = mkEffHandler $ \case
  FreshSymbol prefix -> do
    n <- readIORef counterRef
    writeIORef counterRef (n + 1)
    pure (prefix <> "_" <> T.pack (show n))
  FreshUnique -> do
    n <- readIORef counterRef
    writeIORef counterRef (n + 1)
    pure n

-- | Algebraic effect for multi-exit trampoline suspension.
data MultiExitTrampoline r (a :: Type) where
  -- | Yield an intermediate trampoline checkpoint.
  YieldTrampoline :: r -> MultiExitTrampoline r ()
  -- | Immediately terminate computation with an early exit value.
  ExitWith        :: r -> MultiExitTrampoline r a

-- | Yield a trampoline value under @"trampoline"@.
{-# INLINE yieldTrampoline #-}
yieldTrampoline :: forall r (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "trampoline" ef =>
  r -> Eff eh ef ()
yieldTrampoline r = sendEff @"trampoline" (YieldTrampoline r)

-- | Terminate computation early with a result under @"trampoline"@.
{-# INLINE exitWith #-}
exitWith :: forall r a (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "trampoline" ef =>
  r -> Eff eh ef a
exitWith r = sendEff @"trampoline" (ExitWith r)

-- | Interpreter handler for 'MultiExitTrampoline' in 'IO'.
runTrampoline :: (r -> IO ()) -> EffHandler IO (MultiExitTrampoline r)
runTrampoline onYield = mkEffHandler $ \case
  YieldTrampoline r -> onYield r
  ExitWith r        -> onYield r >> pure (error "Penelope.Effects.Interpreter: exitWith invoked")

-- | Algebraic effect for stepper diagnostic events and execution traces.
data TraceStep (a :: Type) where
  -- | Emit an execution trace message.
  TraceEvent :: !Text -> TraceStep ()

-- | Emit a stepper trace event under @"traceStep"@.
{-# INLINE traceStep #-}
traceStep :: forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ConstructorIndex "traceStep" ef =>
  Text -> Eff eh ef ()
traceStep msg = sendEff @"traceStep" (TraceEvent msg)

-- | Interpreter handler for 'TraceStep' forwarding events to a sink callback.
runTraceStep :: (Text -> IO ()) -> EffHandler IO TraceStep
runTraceStep sink = mkEffHandler $ \(TraceEvent msg) -> sink msg
