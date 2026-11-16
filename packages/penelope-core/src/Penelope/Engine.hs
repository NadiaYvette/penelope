{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Engine
  ( PolyCarrier(..)
  , Handler(..)
  , interpretAll
  ) where

import Data.Kind (Type)
import GHC.Exts (Any, Word#, unsafeCoerce#)

import Polytropos.Core (Row)
import Polytropos (Record)
import Polytropos.RowMorph (RowMorph)
import Penelope.Core.Syntax (Penelope(..), BranchCont(..), PolyEffect)

data PolyCarrier (carrier :: Row -> Type) = PolyCarrier
  { pcRoute :: forall outs ins. RowMorph outs ins -> Record ins -> carrier outs
  , pcLiftPure :: forall opIns opOuts ins outs.
                  RowMorph opIns ins
               -> (Record opIns -> Record opOuts)
               -> BranchCont (Penelope '[] '[] ins outs) opOuts
               -> Record ins
               -> carrier outs
  }

data Handler (carrier :: Row -> Type) (e :: PolyEffect) = Handler
  { runHandler :: Word# -> Any -> Any -> Any -> carrier Any
  }

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
    go (LiftPure inMap f cont) !frame =
      pcLiftPure pc inMap f (unsafeCoerce# cont) (unsafeCoerce# frame)
    go (OpF _ _ _ _) _ =
      error "Penelope.Engine: OpF dispatch"
    go (ScopeRow _ _ inner) !frame =
      go (unsafeCoerce# inner) frame
    go (OpH {}) _ =
      error "Penelope.Engine: impossible OpH in '[]"
