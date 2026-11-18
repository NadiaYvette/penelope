{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Module      : Penelope.Core.Engine
Description : High-level re-export facade for Penelope execution engine
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides execution engines for Penelope: Phase 1 elaboration, Phase 2 interpretation, and partial peeling.
See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].

{- Note [Two-Phase Engine Architecture and Effect Vector Classification]
The Penelope execution pipeline orchestrates two distinct phases of computation:

1. **Phase 1: Higher-Order Elaboration ('Penelope.Core.Engine.Elaborate'):**
   Traverses higher-order operations ('OpH'), macro-expanding scoped regions
   ('Region') into flat first-order algebraic operations ('OpF') and reified row
   permutations ('Route').
2. **Phase 2: Vector-Dispatched Interpretation ('Penelope.Core.Engine.Interpret'):**
   Executes the fully elaborated first-order term by dispatching operation opcodes
   into contiguous unboxed handler vectors ('Handlers').
3. **Partial Peeling ('Penelope.Core.Engine.Partial'):**
   Interprets a subset of effects while preserving residual operations in the term.

To bridge between high-level Haskell execution and 2LTT backend code generation
(GCC -fgimple, MLIR), 'EffectVectorDescriptor' classifies each effect's 'ResumptionKind':
- 'TailResumptive': Effects (e.g. State, Reader, Writer, Fuel) whose continuations are
  called strictly in tail position, compiling to direct zero-cost function calls.
- 'DelimitedOneShot': Delimited control effects (e.g. Coroutine, Abort, Async) requiring
  first-class stack capture or resumption trampolines.
-}
-}
module Penelope.Core.Engine
  ( module Penelope.Core.Engine.Elaborate
  , module Penelope.Core.Engine.Interpret
  , module Penelope.Core.Engine.Partial

    -- * 2LTT Effect Vector Descriptors
  , ResumptionKind (..)
  , EffectVectorDescriptor (..)
  , buildEffectVectorDescriptor
  , defaultClassifyEffect
  ) where

import Data.List (sort)
import GHC.Generics (Generic)
import Penelope.Core.Engine.Elaborate
import Penelope.Core.Engine.Interpret
import Penelope.Core.Engine.Partial

-- | Resumption discipline for algebraic effects in 2LTT compilation.
data ResumptionKind
  = TailResumptive
  -- ^ State/Reader/Writer/Fuel effects: compiles to a direct zero-cost C/LLVM call.
  | DelimitedOneShot
  -- ^ Coroutine/Abort/Async effects: compiles to stack-switching/yield.
  deriving stock (Eq, Ord, Show, Generic)

-- | Static Penelope effect vector descriptor for vector-dispatched lowering.
data EffectVectorDescriptor = EffectVectorDescriptor
  { evdSlotMap  :: ![(String, (Int, ResumptionKind))]
  -- ^ Maps Penelope effect label @E@ to its flat vector index and resumption discipline
  , evdOpenTail :: !(Maybe String)
  } deriving stock (Eq, Show, Generic)

-- | Compiles an effect list into a flat vector-dispatch table using the given classifier.
buildEffectVectorDescriptor
  :: (String -> ResumptionKind)
  -> [String]
  -> Maybe String
  -> EffectVectorDescriptor
buildEffectVectorDescriptor classifyEff effs mTail =
  let sortedEffs = sort effs
      slots = [ (e, (idx, classifyEff e)) | (e, idx) <- zip sortedEffs [0..] ]
  in EffectVectorDescriptor
      { evdSlotMap = slots
      , evdOpenTail = mTail
      }

-- | Default effect classifier mapping common Penelope effects to their resumption discipline.
--
-- == Classification Invariants
-- 1. __Tail-Resumptive Effects:__
--    Algebraic effects such as 'State', 'Reader', 'Writer', and 'Fuel' that always resume
--    their continuation exactly once in tail position without capturing multi-shot frames
--    are classified as 'TailResumptive'. This permits in-place stack execution.
-- 2. __Delimited One-Shot:__
--    Effects that potentially abort or escape early are classified as 'DelimitedOneShot'.
-- 3. __Multi-Shot Non-Determinism:__
--    Effects such as 'NonDet' that fork or resume branches multiple times are classified
--    as 'MultiShot', requiring full frame cloning.
defaultClassifyEffect :: String -> ResumptionKind
defaultClassifyEffect eff = case eff of
  "State"  -> TailResumptive
  "Reader" -> TailResumptive
  "Writer" -> TailResumptive
  "Fuel"   -> TailResumptive
  _        -> DelimitedOneShot
