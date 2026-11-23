{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Module: Penelope.Uring
-- Description: Umbrella module for Penelope io_uring runtime and batch fusion
--
-- Re-exports the complete @io_uring@ subsystem:
--
-- * "Penelope.Uring.Plan": Submission batch planning, link chains, and 64-bit flight tokens.
-- * "Penelope.Uring.FlightTable": Generational token allocation and buffer lifetime tracking.
-- * "Penelope.Uring.Sim": Deterministic in-memory simulation of kernel batch semantics.
-- * "Penelope.Uring.Effect": Multi-exit polycategorical effect definitions.
--
-- = High-Performance Asynchronous I/O Subsystem
--
-- The Penelope @io_uring@ module family delivers high-throughput, low-latency
-- asynchronous I/O capabilities designed specifically for modern Linux kernels.
-- Standard Haskell I/O models rely on operating system notification primitives
-- such as @epoll@, waking up user-space threads to execute synchronous @read(2)@
-- or @write(2)@ calls. Under intensive multi-gigabit networking or write-ahead log
-- transactions, this architecture incurs substantial kernel boundary transitions,
-- CPU cache invalidations, and system call overheads.
--
-- = Key Architectural Capabilities
--
-- 1. Batch Submission Fusion:
--    Independent I/O operations are coalesced into a single 'SqeBatch' using
--    'planParallelBatch'. Rather than making $N$ kernel calls for $N$ operations,
--    the entire batch is dispatched in a single @io_uring_enter(2)@ invocation.
--
-- 2. Kernel-Linked Dependency Chains:
--    Dependent sequences (for example, writing an encrypted block followed by
--    an immediate @fsync@) are composed via 'planLinkedChain'. The Linux kernel
--    automatically manages ordering and error propagation, cascading failures
--    as @-ECANCELED@ without requiring round-trips to user-space.
--
-- 3. Dead Completion Elision:
--    Intermediate operations whose results are unobserved at runtime can be
--    configured with 'FlagCqeSkipSuccess', completely eliminating completion
--    queue ring events on success and reducing queue saturation.
--
-- 4. ABA-Safe Token Allocation:
--    Using 'packFlightToken' and 'unpackFlightToken', flight slots pair a 32-bit
--    slot index with a 32-bit generational sequence, guaranteeing that delayed
--    completion events cannot collide with recycled buffer slots.
--
-- 5. Deterministic Simulation:
--    The provided 'runUringSim' engine executes batch plans completely in memory,
--    ensuring bit-for-bit testability and cross-platform verification even
--    in constrained CI containers where native ring initialization is restricted.
--
-- = Typical Module Wiring
--
-- Applications using Penelope's high-performance runtime import 'Penelope.Uring'
-- as an umbrella interface. Code performing transaction commits can fuse
-- write and sync operations into a single chained batch, verifying observable
-- state with 'runUringSimPure' or dispatching through kernel FFI rings.
--
-- By avoiding heap closures and unboxed token passing, the subsystem delivers
-- predictability suitable for latency-critical financial and telemetry systems.
module Penelope.Uring
  ( -- * Submission Planning and Tokens
    module Penelope.Uring.Plan
    -- * Generational Flight Table
  , module Penelope.Uring.FlightTable
    -- * In-Memory Simulator
  , module Penelope.Uring.Sim
    -- * Multi-Exit Effect Facade
  , module Penelope.Uring.Effect
  ) where

import Penelope.Uring.Effect
import Penelope.Uring.FlightTable
import Penelope.Uring.Plan hiding (runUringSim)
import Penelope.Uring.Sim
