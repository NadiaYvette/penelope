{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Penelope.Core.Send
Description : Zero-overhead smart constructors for first-order and higher-order effect operations
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

This module provides zero-overhead smart constructors ('sendPolyF', 'sendPolyH')
injecting algebraic effect operations into the 'Penelope' polycategorical core.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
module Penelope.Core.Send
  ( -- * Smart Constructors
    sendPolyF
  , sendPolyF'
  , sendPolyH
  , sendPolyH'
  ) where

import GHC.Exts (Any, Word (W#), unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Syntax (Penelope (..), PolyEffect, PolyHEffect)
import Polytropos (Record, RowOf)
import Polytropos.Core (ConstructorIndex (..), Row)
import Polytropos.RowMorph (KnownRowMorph (..))

{- |
Inject a first-order algebraic operation into the syntax tree.
Looks up the constructor tag via 'ConstructorIndex' solved at compile time by @Polytropos.Plugin@,
and wires input ports via 'KnownRowMorph'.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
sendPolyF ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (exits :: Row) (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l ef, KnownRowMorph opIns ins) =>
  Any
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyF #-}
sendPolyF payload conts =
  case constructorIndex @_ @l @ef of
    W# tag# -> OpF tag# (rowMorph @opIns @ins) payload conts

-- | Typed payload variant of 'sendPolyF' applying 'unsafeCoerce#' without runtime boxing.
sendPolyF' ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (exits :: Row) op (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l ef, KnownRowMorph opIns ins) =>
  op
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyF' #-}
sendPolyF' op conts = sendPolyF @l @opIns (unsafeCoerce# op) conts

{- |
Inject a higher-order scoped algebraic operation into the syntax tree with
explicit sub-regions and multi-exit branch continuations.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
sendPolyH ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (regions :: Row) (exits :: Row) (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l eh, KnownRowMorph opIns ins) =>
  Record regions
  -> Any
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyH #-}
sendPolyH regions payload conts =
  case constructorIndex @_ @l @eh of
    W# tag# -> OpH tag# (rowMorph @opIns @ins) regions payload conts

-- | Typed payload variant of 'sendPolyH' applying 'unsafeCoerce#' without runtime boxing.
sendPolyH' ::
  forall (l :: Symbol) (opIns :: RowOf (*)) (ins :: RowOf (*)) (regions :: Row) (exits :: Row) op (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf (*)).
  (ConstructorIndex l eh, KnownRowMorph opIns ins) =>
  Record regions
  -> op
  -> Record exits
  -> Penelope eh ef ins outs
{-# INLINE sendPolyH' #-}
sendPolyH' regions op conts = sendPolyH @l @opIns regions (unsafeCoerce# op) conts
