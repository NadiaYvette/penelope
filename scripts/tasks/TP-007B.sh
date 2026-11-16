#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007B] Deploying Penelope.Backend.Plugin & Penelope.Stage.OrganIR ==="

mkdir -p packages/penelope-core/src/Penelope/Backend
mkdir -p packages/penelope-stage/src/Penelope/Stage
mkdir -p packages/penelope-stage/test

# 1. Mount seeds
cp -f /home/nyc/src/plans/seeds/TP-007B-BackendPlugin.hs packages/penelope-core/src/Penelope/Backend/Plugin.hs
cp -f /home/nyc/src/plans/seeds/TP-007B-StageOrganIR.hs packages/penelope-stage/src/Penelope/Stage/OrganIR.hs

# 2. Expose Penelope.Backend.Plugin in penelope-core.cabal
python3 -c '
with open("packages/penelope-core/penelope-core.cabal", "r") as f:
    content = f.read()
if "Penelope.Backend.Plugin" not in content:
    content = content.replace("    Penelope.Perceus\n", "    Penelope.Perceus\n    Penelope.Backend.Plugin\n")
with open("packages/penelope-core/penelope-core.cabal", "w") as f:
    f.write(content)
'

# 3. Create penelope-stage.cabal if not present
if [ ! -f packages/penelope-stage/penelope-stage.cabal ]; then
  cat << 'EOF' > packages/penelope-stage/penelope-stage.cabal
cabal-version: 3.0
name: penelope-stage
version: 0.1.0.0
synopsis: 2LTT Staging & OrganIR Lowering Pipeline for Penelope
description:
  2-Level Type Theory staging engine lowering high-level Penelope ASTs
  to freestanding linear kernel and backend plugins.
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
category: Control
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Stage.OrganIR
  hs-source-dirs: src
  default-language: Haskell2010
  default-extensions:
    BangPatterns
    DataKinds
    GADTs
    KindSignatures
    OverloadedStrings
    PolyKinds
    RankNTypes
    ScopedTypeVariables
  ghc-options: -Wall -Wcompat -Wredundant-constraints
  build-depends:
    base >= 4.14 && < 5,
    text >= 2.0 && < 2.2,
    containers >= 0.6 && < 0.9,
    primitive >= 0.9 && < 0.10,
    penelope-core,
    polytropos-core >= 0.1 && < 0.2

test-suite tests
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  hs-source-dirs: test
  default-language: Haskell2010
  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts
  build-depends:
    base >= 4.14 && < 5,
    hspec >= 2.11 && < 2.12,
    containers >= 0.6 && < 0.9,
    text >= 2.0 && < 2.2,
    penelope-core,
    penelope-stage,
    polytropos-core >= 0.1 && < 0.2,
    polytropos-plugin >= 0.1 && < 0.2
EOF
fi

# 4. Create test/Spec.hs if not present
if [ ! -f packages/penelope-stage/test/Spec.hs ]; then
  cat << 'EOF' > packages/penelope-stage/test/Spec.hs
{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
module Main (main) where

import Test.Hspec
import qualified Data.IntMap.Strict as IM
import Penelope.Core
import Penelope.Perceus
import Penelope.Stage.OrganIR
import Polytropos.Core (Pair(..))
import Polytropos.RowMorph (rowMorph)

main :: IO ()
main = hspec $ do
  describe "Penelope.Stage.OrganIR" $ do
    it "verifies pure Route term satisfies freestanding linear profile" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
          boxedIn = IM.singleton 0 (SizeClass 1)
          res = verifyFreestandingLinear boxedIn term
      case res of
        Right note -> pnOutputPlaces note `shouldBe` IM.empty
        Left err   -> expectationFailure (show err)

    it "rejects LiftPure closure in freestanding linear profile" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = LiftPure m id (BranchCont (\_ -> Route m))
          boxedIn = IM.singleton 0 (SizeClass 1)
          res = verifyFreestandingLinear boxedIn term
      res `shouldBe` Left ErrOpaqueClosureInKernelStage
EOF
fi

echo "=== [TP-007B] Building and testing ==="
cabal v2-build all -j20 --semaphore
cabal v2-test penelope-stage:test:tests

echo "=== [TP-007B] Script execution complete ==="
