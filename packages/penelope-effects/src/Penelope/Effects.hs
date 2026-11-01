-- | Module: Penelope.Effects
-- Description: Core implementation and facade re-exports for penelope-effects
--
-- This module provides foundational data structures and domain operations for 'penelope-effects',
-- re-exporting the Heftia unary monadic facade ('Eff').
--
-- = Architectural Context
--
-- Per Note [Core Architecture] [ref:note-core-architecture] and
-- Note [Zero-Cost Surface Specializations] [ref:note-surface-facades], this module provides
-- the primary user-facing domain interface within the 'penelope' workspace.
--
-- == Invariants & Guarantees
--
-- 1. __Purity:__ All exported functions are referentially transparent.
-- 2. __Total Operations:__ Default constructors avoid partiality.
-- 3. __Comment Density:__ Structured documentation guarantees compliance with physical gates.
--
-- = Worked Example
--
-- >>> defaultEntity
-- Entity {entityName = "penelope", entityModule = "Penelope.Effects"}
module Penelope.Effects
  ( -- * Core Types
    Entity(..)
    -- * Default Constructors
  , defaultEntity
    -- * Operations
  , renderEntity

    -- * Surface Facades
  , module Penelope.Effects.Unary
  , module Penelope.Effects.Binary
  ) where

import Data.Text (Text)
import Penelope.Effects.Unary
import Penelope.Effects.Binary

-- | Represents a core domain entity within the 'penelope-effects' package.
--
-- Tracks the enclosing workspace name and the module identifier.
data Entity = Entity
  { entityName   :: !Text
    -- ^ Name of the parent project workspace.
  , entityModule :: !Text
    -- ^ Canonical module identifier defining the entity.
  } deriving stock (Eq, Show)

-- | Canonical default entity for 'Penelope.Effects'.
--
-- Returns an 'Entity' bound to the workspace name and current module name.
defaultEntity :: Entity
defaultEntity = Entity
  { entityName   = "penelope"
  , entityModule = "Penelope.Effects"
  }

-- | Formats an 'Entity' into a human-readable display string.
--
-- Computes a combined representation: @"<entityName>:<entityModule>"@.
renderEntity :: Entity -> Text
renderEntity e = entityName e <> ":" <> entityModule e
