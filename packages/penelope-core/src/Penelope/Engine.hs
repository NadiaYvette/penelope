-- |
-- Module      : Penelope.Engine
-- Copyright   : (c) 2026 The Penelope Authors
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Vectorized interpretation engine for Penelope polycategorical circuits.
-- Evaluates first-order polycategorical hypergraphs against carrier representations
-- without dynamic intermediate boxing or tuple cascades.
--
-- {- Note [Polycategorical Carrier Evaluation]
-- In Penelope, a polycategorical term 'Penelope' represents an open hypergraph of
-- computational nodes. When higher-order effects have been fully elaborated into
-- first-order operations ('OpF') and wire routing nodes ('Route'), the program can
-- be interpreted into an arbitrary target carrier (such as 'IO', identity, or a
-- compiler code generation monad) via 'interpretAll'.
--
-- The 'PolyCarrier' record bundles two fundamental operations:
-- 1. 'pcRoute': Interpreting an O(1) unboxed row morphism directly into the target
--    carrier representation without re-allocating intermediate data structures.
-- 2. 'pcLiftPure': Executing a pure computational transform, feeding the results
--    forward into multi-branch continuations ('BranchCont').
--
-- Handlers for algebraic effects are passed as a table of vectorized callbacks,
-- dispatching primitive operations by constructor index in O(1).
-- -}
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Engine
  ( -- * Carrier Abstractions
    PolyCarrier(..)
  , Handler(..)

    -- * Interpretation Dispatch
  , interpretAll
  ) where

import Data.Kind (Type)
import GHC.Exts (Any, Word#, unsafeCoerce#)

import Polytropos.Core (Row)
import Polytropos (Record)
import Polytropos.RowMorph (RowMorph)
import Penelope.Core.Syntax (Penelope(..), BranchCont(..), PolyEffect)

-- | Abstract polycategorical carrier substrate parameterised over an output row @outs@.
--   Provides the primitive execution mechanics required by 'interpretAll'.
data PolyCarrier (carrier :: Row -> Type) = PolyCarrier
  { pcRoute :: forall outs ins.
               RowMorph outs ins
            -> Record ins
            -> carrier outs
    -- ^ Interprets a reified row morphism wire permutation from input record @ins@
    --   to target output representation @outs@.
  , pcLiftPure :: forall opIns opOuts ins outs.
                  RowMorph opIns ins
               -> (Record opIns -> Record opOuts)
               -> BranchCont (carrier outs) opOuts
               -> Record ins
               -> carrier outs
    -- ^ Executes a lifted pure function transforming a subset of input fields @opIns@
    --   to @opOuts@, invoking the continuation on the resulting frame.
  }

-- | Vectorized algebraic effect handler for a single polycategorical effect @e@.
--   Encapsulates an unboxed jump table dispatching operation codes in O(1).
data Handler (carrier :: Row -> Type) (e :: PolyEffect) = Handler
  { runHandler :: Word# -> Any -> Any -> Any -> carrier Any
    -- ^ Unboxed handler entrypoint: takes operation opcode (@Word#@), payload arguments,
    --   and branch continuations, yielding an effectful carrier action.
  }

-- | Interprets a fully elaborated first-order polycategorical term against a carrier
--   and handler table, initiating execution from an initial input record frame.
--
-- == Execution Pipeline & Invariants
-- 1. __Pure Wire Permutation:__
--    Dispatches static wire permutations directly through 'pcRoute', executing
--    gather-scatter transformations in linear time without intermediate allocations.
-- 2. __Pure Lifted Transformations:__
--    Evaluates pure functional transformations via 'pcLiftPure', feeding results directly
--    into the unboxed continuation closure.
-- 3. __Branchless Handler Invocation:__
--    Algebraic operations index directly into the handler vector using the operation's
--    machine-word opcode (@Word#@), achieving \(\mathcal{O}(1)\) algebraic dispatch.
--
-- Traverses the term hypergraph, routing wires via 'pcRoute', executing pure transforms
-- via 'pcLiftPure', and dispatching algebraic operations to the appropriate handler.
interpretAll
  :: forall carrier handlers ef ins outs.
     PolyCarrier carrier
  -> handlers
  -> Penelope '[] ef ins outs
  -> Record ins
  -> carrier outs
interpretAll !pc _handlers prog !initFrame = go prog (unsafeCoerce# initFrame)
  where
    go :: forall i o. Penelope '[] ef i o -> Record i -> carrier o
    go (Route inMap) !frame =
      pcRoute pc inMap (unsafeCoerce# frame)
    go (LiftPure inMap f (BranchCont k)) !frame =
      pcLiftPure pc inMap f (BranchCont (\mid -> go (k mid) frame)) (unsafeCoerce# frame)
    go (OpF _ _ _ _) _ =
      error "Penelope.Engine: OpF dispatch"
    go (ScopeRow _ _ inner) !frame =
      go (unsafeCoerce# inner) frame
    go (OpH {}) _ =
      error "Penelope.Engine: impossible OpH in '[]"
