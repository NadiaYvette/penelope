{- |
Module      : Penelope.Core
Description : High-level re-export facade for penelope-core
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Foundational exports for Penelope core polycategorical effect engine.
See Note [Core Architecture] [ref:note-core-architecture].

{- Note [Penelope Core System Architecture]
Penelope unifies higher-order unary algebraic effects and binary profunctor arrow pipelines
into a single, flat polycategorical representation backed by Polytropos open row polymorphism.

Key sub-modules re-exported by 'Penelope.Core':
* 'Penelope.Core.Syntax': Defines the core $(m,n,k,r)$-ary polycategorical GADT ('Penelope'),
  branch continuations ('BranchCont'), and regional scopes ('Region').
* 'Penelope.Core.Route': Implements pure wire permutations, identity elision, and peephole fusion
  using unboxed Polytropos row morphisms ('RowMorph').
* 'Penelope.Core.Send': Exposes compile-time smart effect injectors ('sendPolyF', 'sendPolyH')
  with zero-overhead literal constructor indexing solved by 'Polytropos.Plugin'.
* 'Penelope.Core.Wiring': Provides sequential composition ('seqPenelope', '(>>>)'), frame-rule
  auto-wiring ('(>&>)'), disjunctive fanout ('(>?<)'), and zero-traversal row weakening ('weakenRow').
* 'Penelope.Core.Engine': Implements the two-phase execution pipeline: Phase 1 elaboration
  ('elaborateAll'), Phase 2 vector interpretation ('interpretAll'), and partial peeling ('interpretSub').
-}
-}
module Penelope.Core
  ( module Penelope.Core.Syntax
  , module Penelope.Core.Route
  , module Penelope.Core.Send
  , module Penelope.Core.Wiring
  , module Penelope.Core.Engine
  ) where

import Penelope.Core.Syntax
import Penelope.Core.Route
import Penelope.Core.Send
import Penelope.Core.Wiring
import Penelope.Core.Engine
