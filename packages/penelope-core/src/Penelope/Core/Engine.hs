{- |
Module      : Penelope.Core.Engine
Description : High-level re-export facade for Penelope execution engine
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides execution engines for Penelope: Phase 1 elaboration and Phase 2 interpretation.
See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
module Penelope.Core.Engine
  ( module Penelope.Core.Engine.Elaborate
  , module Penelope.Core.Engine.Interpret
  ) where

import Penelope.Core.Engine.Elaborate
import Penelope.Core.Engine.Interpret
