{- |
Module      : Penelope.Static
Description : High-level re-export facade for penelope-static
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides static analysis, reflection, and liveness telemetry for Penelope pipelines.
See Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
module Penelope.Static
  ( module Penelope.Static.Analysis
  ) where

import Penelope.Static.Analysis
