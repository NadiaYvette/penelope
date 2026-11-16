{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Engine
  ( PolyCarrier(..)
  , Handler(..)
  , interpretAll
  ) where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import GHC.Exts (Any, Word#, unsafeCoerce#)

import Polytropos.Core (Row)
import Polytropos (Record)
import Polytropos.RowMorph (RowMorph)
import Penelope.Core.Syntax (Penelope(..), BranchCont(..), PolyEffect)

data PolyCarrier (carrier :: Row Type -> Type) = PolyCarrier
  { pcRoute :: forall outs ins. RowMorph outs ins -> Record Any ins -> carrier outs
  , pcLiftPure :: forall opIns opOuts ins outs.
                  RowMorph opIns ins
               -> (Record Identity opIns -> Record Identity opOuts)
               -> BranchCont (Penelope '[] '[] ins outs) opOuts
               -> Record Any ins
               -> carrier outs
  }

data Handler (carrier :: Row Type -> Type) (e :: PolyEffect) = Handler
  { runHandler :: Word# -> Any -> Record Any -> Record Any -> carrier Any
  }

interpretAll
  :: PolyCarrier carrier
  -> Record (Handler carrier) ef
  -> Penelope '[] ef ins outs
  -> Record val ins
  -> carrier outs
interpretAll !pc !handlers prog !initFrame = go prog (unsafeCoerce# initFrame)
  where
    go :: Penelope '[] ef i o -> Record Any i -> carrier o
    go (Route inMap) !frame =
      pcRoute pc inMap frame
    go (LiftPure inMap f cont) !frame =
      pcLiftPure pc inMap f (unsafeCoerce# cont) frame
    go (OpF tag# inMap opAny conts) !frame =
      let !(Handler h) = unsafeCoerce# handlers :: Handler carrier Any
      in unsafeCoerce# (h tag# opAny (unsafeCoerce# frame) (unsafeCoerce# conts))
    go (ScopeRow _ _ inner) !frame =
      go (unsafeCoerce# inner) frame
    go (OpH {}) _ =
      error "Penelope.Engine: impossible OpH in '[]"
