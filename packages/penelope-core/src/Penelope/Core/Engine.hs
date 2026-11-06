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
defaultClassifyEffect :: String -> ResumptionKind
defaultClassifyEffect eff = case eff of
  "State"  -> TailResumptive
  "Reader" -> TailResumptive
  "Writer" -> TailResumptive
  "Fuel"   -> TailResumptive
  _        -> DelimitedOneShot
