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
