{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Module: Penelope.Uring.Effect
-- Description: Multi-exit UringOp polycategorical effect and static batch compilation
--
-- Exposes Penelope polycategorical effect representations for @io_uring@ operations,
-- providing four discrete exit branches:
-- 1. @"ok"@: Successful I/O completion delivering positive byte count or status.
-- 2. @"eof"@: Reached end-of-file condition (zero bytes read).
-- 3. @"canceled"@: Request terminated early by kernel cancellation (@-ECANCELED@).
-- 4. @"err"@: Unrecoverable kernel error code (@-errno@).
--
-- = Polycategorical Multi-Exit Representation
--
-- Unlike standard monad transformer stacks that encode control branching through
-- nested 'Either' datatypes (e.g. 'Either (Either Eof Cancel) (Either Err Ok)'),
-- Penelope's \((m,n,k,r)\)-ary polycategorical engine models discrete disjunctive
-- completion ports natively.
--
-- When an operation is submitted to the @io_uring@ submission ring, its completion
-- status is dispatched directly across one of the four outcome ports. This eliminates
-- heap-allocated sum-type packaging while allowing the compiler to perform dead-branch
-- elimination and jump-table dispatch based on the static structure of the pipeline.
--
-- = Static Batch Compilation
--
-- Multiple concurrent 'UringOp' operations generated across independent polycategorical
-- wires or 'ApplicativeDo' blocks can be extracted into a coalesced 'SqeBatch' via
-- 'compileToBatch'. The resulting batch is dispatched in a single kernel transition,
-- avoiding per-operation system call invocation overheads.
module Penelope.Uring.Effect
  ( -- * Multi-Exit Outcome
    UringOutcome(..)
    -- * Polycategorical Effect Interface
  , UringOp(..)
  , compileToBatch
  , interpretUringBatch
  ) where

import Data.Int (Int32)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Penelope.Uring.Plan
import Polytropos.Core (Pair((:=)))

-- | Pure algebraic outcome from an @io_uring@ execution step.
data UringOutcome a
  = UringOk !a
    -- ^ Successful operation outcome holding the result value.
  | UringEof
    -- ^ Premature end of file signal.
  | UringCanceled !FlightToken
    -- ^ Cancellation confirmation with the associated flight token.
  | UringErr !Int32
    -- ^ Negative operating system error number.
  deriving stock (Eq, Show)

-- | First-order polycategorical effect syntax descriptor for @io_uring@ submissions.
-- Defines multi-port inputs and outputs matching Penelope's (m,n,k,r)-ary core.
data UringOp (ins :: [Pair Symbol Type]) (outs :: [Pair Symbol Type]) where
  -- | Submits a single pre-formed SQE entry across multi-exit branches.
  SubmitSingle
    :: !SqeEntry
    -> UringOp '["in" ':= ()] '["ok" ':= Int32, "eof" ':= (), "canceled" ':= FlightToken, "err" ':= Int32]

  -- | Submits a full fused batch in a single kernel enter transition.
  SubmitBatch
    :: !SqeBatch
    -> UringOp '["in" ':= ()] '["ok" ':= [CqeResult], "err" ':= Int32]

-- | Compiles a collection of sequential or parallel SQEs into a single fused 'SqeBatch'.
-- Employs 'planParallelBatch' to submit all independent operations in one system call.
compileToBatch :: [SqeEntry] -> SqeBatch
compileToBatch = planParallelBatch

-- | Pure interpreter mapping 'CqeResult' entries to algebraic 'UringOutcome' values.
interpretUringBatch :: [CqeResult] -> [UringOutcome Int32]
interpretUringBatch = map interpretOne
  where
    interpretOne cqe
      | cqeRes cqe > 0 = UringOk (cqeRes cqe)
      | cqeRes cqe == 0 = UringEof
      | cqeRes cqe == -125 = UringCanceled (cqeToken cqe)
      | otherwise = UringErr (cqeRes cqe)
