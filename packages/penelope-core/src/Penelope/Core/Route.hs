{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Penelope.Core.Route
Description : Pure port routing, identity elision, and route peephole optimization
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

This module provides smart constructors and fusion routines for pure port wire
permutations over Polytropos 't:RowMorph' structures.

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
module Penelope.Core.Route
  ( -- * Smart Constructors
    route
  , routeExplicit

    -- * Route Optimization & Fusion
  , fuseRoute
  , isIdentityRowMorph
  , isIdentityRoute
  ) where

import Data.Kind (Type)
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import Penelope.Core.Syntax (Penelope (..), PolyEffect, PolyHEffect)
import Polytropos (RowOf)
import Polytropos.RowMorph
  ( KnownRowMorph (..)
  , RowMorph (..)
  , composeRowMorph
  )

{- |
Construct a pure wire permutation between input ports and output ports using
a compile-time synthesized 't:RowMorph' solved by @Polytropos.Plugin@.

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
route ::
  forall {eh :: RowOf PolyHEffect} {ef :: RowOf PolyEffect} (ins :: RowOf Type) (outs :: RowOf Type).
  KnownRowMorph outs ins =>
  Penelope eh ef ins outs
{-# INLINE route #-}
route = Route (rowMorph @outs @ins)

-- | Construct a pure wire permutation with an explicit 't:RowMorph'.
routeExplicit ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (ins :: RowOf Type) (outs :: RowOf Type).
  RowMorph outs ins
  -> Penelope eh ef ins outs
{-# INLINE routeExplicit #-}
routeExplicit = Route

{- |
Fuse two consecutive pure route stages in \(O(|outs|)\) time with zero intermediate allocations.
Given a route from @a@ to @b@ ('t:RowMorph' @b@ @a@) and a route from @b@ to @c@ ('t:RowMorph' @c@ @b@),
computes the composite route from @a@ to @c@ ('t:RowMorph' @c@ @a@).
-}
fuseRoute ::
  RowMorph b a
  -> RowMorph c b
  -> RowMorph c a
{-# INLINE fuseRoute #-}
fuseRoute m1 m2 = composeRowMorph m2 m1

{- |
Check whether a 't:RowMorph' represents the identity permutation (slot @i@ maps to @i@).
-}
isIdentityRowMorph :: RowMorph r1 r2 -> Bool
{-# INLINE isIdentityRowMorph #-}
isIdentityRowMorph (RowMorph arr) =
  let !n = sizeofPrimArray arr
      go !i
        | i >= n    = True
        | otherwise = indexPrimArray arr i == i && go (i + 1)
  in go 0

{- |
Check whether a 'Penelope' computation is an identity wire permutation.
-}
isIdentityRoute :: Penelope eh ef ins outs -> Bool
isIdentityRoute (Route m) = isIdentityRowMorph m
isIdentityRoute _         = False
