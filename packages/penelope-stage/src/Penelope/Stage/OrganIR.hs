-- FILE: src/Penelope/Stage/OrganIR.hs
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings, PolyKinds, RankNTypes #-}

module Penelope.Stage.OrganIR
  ( StagedKernelError(..)
  , StagedModule(..)
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

-- | 2LTT Staging Verification Errors when lowering a Haskell/Penelope eDSL module
--   to `#[freestanding_linear]` kernel code (`telix` / `pgcl`).
data StagedKernelError
  = ErrOpaqueClosureInKernelStage
    -- ^ `LiftPure` Haskell heap closure found in a zero-RTS freestanding module.
  | ErrNonLinearSlotDuplication !Int !Int
    -- ^ Boxed slot `i` was duplicated `c > 1` times without an explicit capability.
  deriving (Show, Eq)

data StagedModule ef ins outs = StagedModule
  { smName        :: !Text
  , smLayout      :: !TargetLayout
  , smProfile     :: !RTSProfile
  , smRootNote    :: !PerceusNote
  , smHypergraph  :: !(Penelope '[] ef ins outs)
  }

-- | Statically verifies that a first-order `Penelope '[] ef ins outs` module satisfies
--   Profile 0 (`#[freestanding_linear]`): zero opaque Haskell heap closures (`LiftPure`)
--   and strict multiplicity <= 1 on all boxed `Polytropos` slots.
verifyFreestandingLinear
  :: IntMap SizeClass
  -> Penelope '[] ef ins outs
  -> Either StagedKernelError PerceusNote
verifyFreestandingLinear !boxedIn = go
  where
    go :: Penelope '[] ef ins outs -> Either StagedKernelError PerceusNote
    go (LiftPure _ _ _) =
      Left ErrOpaqueClosureInKernelStage
    go (Route inMap) = do
      checkMultiplicity (gatherHistogram inMap)
      let !liveIn = analyzePerceusLiveness (Route inMap)
      Right (computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn inMap [] IM.empty)
    go node@(OpF _ inMap _ (RowRep (Rep _ contArr))) = do
      let !hist = gatherHistogram inMap
      checkMultiplicity hist
      let !liveIn = analyzePerceusLiveness node
          !k      = sizeofSmallArray contArr
          !bLives = replicate k ( analyzePerceusLiveness node )
      Right (computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn inMap bLives boxedIn)
    go (ScopeRow _ _ inner) =
      go (unsafeCoerce# inner)
    go (OpH {}) =
      error "Penelope.Stage.OrganIR: impossible OpH in '[]"

    checkMultiplicity :: IntMap Int -> Either StagedKernelError ()
    checkMultiplicity !hist =
      case IM.toList (IM.filterWithKey (\slot c -> IM.member slot boxedIn && c > 1) hist) of
        ((badSlot, c) : _) -> Left (ErrNonLinearSlotDuplication badSlot c)
        []                 -> Right ()

-- | 2-Level Type Theory (2LTT) staging entrypoint (`penelope-stage`):
--   Verifies `RTSFreestandingLinear` invariants at Meta-Level (GHC) and compiles
--   the Object-Level hypergraph via the provided `BackendPlugin` (`MLIR` or `GIMPLE`).
stageFreestandingModule
  :: Text
  -> TargetLayout
  -> IntMap SizeClass
  -> BackendPlugin val carrier artifact
  -> RecordF val ins
  -> Penelope '[] ef ins outs
  -> Either StagedKernelError artifact
stageFreestandingModule !_modName !layout !boxedIn plugin initSymFrame prog = do
  !_rootNote <- verifyFreestandingLinear boxedIn prog
  Right (compileWithBackend plugin layout RTSFreestandingLinear prog initSymFrame)