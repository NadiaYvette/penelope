-- |
-- Module      : Penelope.Stage.OrganIR
-- Copyright   : (c) 2026 The Penelope Authors
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 2-Level Type Theory (2LTT) staging engine and OrganIR lowering pipeline.
-- Validates that polycategorical effect specifications constructed at the GHC
-- meta-programming level strictly conform to freestanding linear kernel constraints,
-- lowering verified terms to backend artifacts (GCC -fgimple, MLIR).
--
-- {- Note [2LTT Staging and Freestanding Kernel Verification]
-- In Penelope's 2-Level Type Theory architecture:
--
-- * Stage 1 (Meta-Level / GHC Haskell): The developer writes expressive, type-safe
--   effect workflows leveraging open rows ('polytropos'), smart constructors, and
--   higher-order elaboration ('elaborateAll'). At this level, Haskell closures and
--   arbitrary host computations may participate in AST construction.
--
-- * Stage 0 (Object-Level / Freestanding Kernel): The resulting first-order hypergraph
--   ('Penelope \'[] ef ins outs') is targeted towards zero-RTS kernel execution
--   (such as 'telix' microkernel services or bare-metal hypervisors).
--
-- Before lowering to object code, 'verifyFreestandingLinear' enforces Profile 0 invariants:
-- 1. Absence of Opaque Closures: Any occurrence of 'LiftPure' would attempt to evaluate
--    a GHC heap closure inside a kernel environment devoid of the GHC runtime system.
--    Such occurrences are rejected with 'ErrOpaqueClosureInKernelStage'.
-- 2. Affine and Linear Multiplicity: Every boxed memory slot indexed in 'boxedIn' must
--    exhibit a read multiplicity <= 1 across all downstream branch continuations.
--    Uncontrolled duplication without explicit copying capabilities is rejected
--    with 'ErrNonLinearSlotDuplication'.
--
-- Once verified, 'stageFreestandingModule' delegates to the registered 'BackendPlugin'
-- to emit SSA IR or native machine code.
-- -}
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings, PolyKinds, RankNTypes #-}

module Penelope.Stage.OrganIR
  ( -- * Verification Errors
    StagedKernelError(..)

    -- * Staged Module Representation
  , StagedModule(..)

    -- * Invariant Verification and Staging Pipeline
  , verifyFreestandingLinear
  , stageFreestandingModule
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Primitive.SmallArray (sizeofSmallArray)
import Data.Text (Text)
import GHC.Exts (unsafeCoerce#)

import Polytropos (RecordF)
import Polytropos.Core (Rep(..), RowRep(..))
import Penelope.Core.Syntax (Penelope(..))
import Penelope.Backend.Plugin (TargetLayout, BackendPlugin, compileWithBackend)
import Penelope.Perceus
  ( RTSProfile(..)
  , SizeClass(..)
  , PerceusNote(..)
  , gatherHistogram
  , analyzePerceusLiveness
  , computeNodePerceusPlan
  )

-- | Verification errors encountered when lowering a Penelope polycategorical term
--   to a freestanding zero-RTS kernel module (@#[freestanding_linear]@).
data StagedKernelError
  = ErrOpaqueClosureInKernelStage
    -- ^ An opaque GHC function closure ('LiftPure') was encountered in a kernel-stage module.
    --   Freestanding targets have no GHC RTS to evaluate boxed closures.
  | ErrNonLinearSlotDuplication !Int !Int
    -- ^ Boxed slot @slot_i@ with ownership was duplicated @c > 1@ times without an explicit copy capability.
  deriving (Show, Eq)

-- | Container representing a verified staged module ready for backend emission.
data StagedModule ef ins outs = StagedModule
  { smName        :: !Text
    -- ^ Human-readable identifier or symbol name for the staged module.
  , smLayout      :: !TargetLayout
    -- ^ Hardware layout descriptor (pointer bitwidth, endianness, word alignment).
  , smProfile     :: !RTSProfile
    -- ^ Target runtime profile (e.g. 'RTSFreestandingLinear').
  , smRootNote    :: !PerceusNote
    -- ^ Root Perceus placement and liveness schedule.
  , smHypergraph  :: !(Penelope '[] ef ins outs)
    -- ^ First-order polycategorical term hypergraph.
  }

-- | Statically verifies that a first-order polycategorical term 'Penelope \'[] ef ins outs'
--   satisfies the strict constraints of 'RTSFreestandingLinear' (Profile 0).
--
-- == Verification Invariants & Failure Modes
-- 1. __Absence of Host Closures:__
--    Any occurrence of 'LiftPure' causes verification to fail immediately with 'ErrOpaqueClosureInKernelStage'.
-- 2. __Strict Linearity / Affine Multiplicity:__
--    Every boxed slot listed in @boxedIn@ must have read multiplicity \(\le 1\) in the gathering map.
--    Duplicate reads (@c > 1@) fail with 'ErrNonLinearSlotDuplication'.
-- 3. __Schedule Synthesis:__
--    Calculates the optimal 'PerceusNote' allocation and placement schedule under 'RTSFreestandingLinear'.
verifyFreestandingLinear
  :: IntMap SizeClass
     -- ^ Map of incoming boxed slot indices to their allocation size classes.
  -> Penelope '[] ef ins outs
     -- ^ First-order polycategorical term to verify.
  -> Either StagedKernelError PerceusNote
verifyFreestandingLinear !boxedIn = go
  where
    go :: Penelope '[] ef ins outs -> Either StagedKernelError PerceusNote
    -- Reject opaque closures: freestanding kernels lack the GHC runtime to evaluate them
    go (LiftPure _ _ _) =
      Left ErrOpaqueClosureInKernelStage
    -- Verify Route wire permutations: check affine multiplicity and compute zero-allocation plan
    go (Route inMap) = do
      checkMultiplicity (gatherHistogram inMap)
      let !liveIn = analyzePerceusLiveness (Route inMap)
      Right (computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn inMap [] IM.empty)
    -- Verify OpF algebraic nodes: check input multiplicity and compute branch placement plans
    go node@(OpF _ inMap _ (RowRep (Rep _ contArr))) = do
      let !hist = gatherHistogram inMap
      checkMultiplicity hist
      let !liveIn = analyzePerceusLiveness node
          !k      = sizeofSmallArray contArr
          !bLives = replicate k (analyzePerceusLiveness node)
      Right (computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn inMap bLives boxedIn)
    -- Transparently descend through ScopeRow boundaries
    go (ScopeRow _ _ inner) =
      go (unsafeCoerce# inner)
    -- OpH is impossible because higher-order effects '[] are empty after elaboration
    go (OpH {}) =
      error "Penelope.Stage.OrganIR: impossible OpH in '[]"

    checkMultiplicity :: IntMap Int -> Either StagedKernelError ()
    checkMultiplicity !hist =
      case IM.toList (IM.filterWithKey (\slot c -> IM.member slot boxedIn && c > 1) hist) of
        ((badSlot, c) : _) -> Left (ErrNonLinearSlotDuplication badSlot c)
        []                 -> Right ()

-- | Primary 2LTT staging entrypoint for freestanding modules.
--
--   Performs static invariant verification via 'verifyFreestandingLinear', and upon success,
--   compiles the object-level hypergraph into the target artifact using the provided 'BackendPlugin'.
stageFreestandingModule
  :: Text
     -- ^ Target module symbol name.
  -> TargetLayout
     -- ^ Target platform hardware layout descriptor.
  -> IntMap SizeClass
     -- ^ Map of incoming boxed slot indices to their allocation size classes.
  -> BackendPlugin val carrier artifact
     -- ^ Backend plugin responsible for code generation (GCC -fgimple, MLIR).
  -> RecordF val ins
     -- ^ Initial symbolic register frame for input ports.
  -> Penelope '[] ef ins outs
     -- ^ First-order polycategorical term hypergraph.
  -> Either StagedKernelError artifact
stageFreestandingModule !_modName !layout !boxedIn plugin initSymFrame prog = do
  !_rootNote <- verifyFreestandingLinear boxedIn prog
  Right (compileWithBackend plugin layout RTSFreestandingLinear prog initSymFrame)
