#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007C] Deploying Penelope.Backend.MLIR & Penelope.Backend.Gimple ==="

mkdir -p packages/penelope-backend-mlir/src/Penelope/Backend
mkdir -p packages/penelope-backend-mlir/test
mkdir -p packages/penelope-backend-gimple/src/Penelope/Backend
mkdir -p packages/penelope-backend-gimple/test

# 1. Mount seeds
cp -f /home/nyc/src/plans/seeds/TP-007C-MLIR.hs packages/penelope-backend-mlir/src/Penelope/Backend/MLIR.hs
cp -f /home/nyc/src/plans/seeds/TP-007C-Gimple.hs packages/penelope-backend-gimple/src/Penelope/Backend/Gimple.hs

# 2. Create penelope-backend-mlir.cabal
cat << 'EOF' > packages/penelope-backend-mlir/penelope-backend-mlir.cabal
cabal-version: 3.0
name: penelope-backend-mlir
version: 0.1.0.0
synopsis: MLIR structured dialect code emission backend for Penelope
description:
  Direct code-generation backend lowering typed Penelope ASTs into
  MLIR SSA blocks and function regions.
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
category: Control
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Backend.MLIR
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
    primitive >= 0.9 && < 0.10,
    text >= 2.0 && < 2.2,
    penelope-core,
    penelope-backend-mlir,
    polytropos-core >= 0.1 && < 0.2,
    polytropos-plugin >= 0.1 && < 0.2
EOF

# 3. Create penelope-backend-mlir test/Spec.hs
cat << 'EOF' > packages/penelope-backend-mlir/test/Spec.hs
{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators, MagicHash #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
module Main (main) where

import Test.Hspec
import qualified Data.Text as T
import GHC.Exts (unsafeCoerce#)
import Penelope.Core
import Penelope.Backend.Plugin
import Penelope.Backend.MLIR
import Penelope.Perceus (PlacePlan(..), RTSProfile(..))
import Polytropos (RecordF)
import Polytropos.Core (Pair(..), Rep(..), RowRep(..))
import Polytropos.RowMorph (rowMorph)
import Data.Primitive.SmallArray (smallArrayFromList)

main :: IO ()
main = hspec $ do
  describe "Penelope.Backend.MLIR" $ do
    it "emits MLIR func and return for Route term" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
          initFrame :: RecordF SSAVal '["x" ':= Int]
          initFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# (SSAVal "%arg0")]))
          out = compileWithBackend mlirBackendPlugin defaultHostTargetLayout RTSFreestandingLinear term initFrame
      T.isInfixOf "func.func @penelope_entry" out `shouldBe` True
      T.isInfixOf "func.return" out `shouldBe` True

    it "emits MLIR slot update instruction with linear reuse" $ do
      let plan = PlaceInPlaceLinear 0 (unsafeCoerce# ())
          (_, linesOut, SSAVal outReg) = emitMLIRSlotUpdate plan (SSAVal "%r0") 0 (SSAVal "%v0") 1
      outReg `shouldBe` "%r_1"
      take 1 linesOut `shouldBe` ["    %r_1 = organ.rep.slot_update %r0[0], %v0 {reuse = #organ.freestanding_linear} : !organ.rep"]
EOF

# 4. Create penelope-backend-gimple.cabal
cat << 'EOF' > packages/penelope-backend-gimple/penelope-backend-gimple.cabal
cabal-version: 3.0
name: penelope-backend-gimple
version: 0.1.0.0
synopsis: GCC -fgimple SSA tuples emission backend for Penelope
description:
  Direct code-generation backend lowering typed Penelope ASTs into
  GCC -fgimple SSA tuples with explicit virtual memory state.
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
category: Control
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Backend.Gimple
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
    primitive >= 0.9 && < 0.10,
    text >= 2.0 && < 2.2,
    penelope-core,
    penelope-backend-gimple,
    polytropos-core >= 0.1 && < 0.2,
    polytropos-plugin >= 0.1 && < 0.2
EOF

# 5. Create penelope-backend-gimple test/Spec.hs
cat << 'EOF' > packages/penelope-backend-gimple/test/Spec.hs
{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators, MagicHash #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}
module Main (main) where

import Test.Hspec
import qualified Data.Text as T
import GHC.Exts (unsafeCoerce#)
import Penelope.Core
import Penelope.Backend.Plugin
import Penelope.Backend.Gimple
import Penelope.Perceus (PlacePlan(..), RTSProfile(..))
import Polytropos (RecordF)
import Polytropos.Core (Pair(..), Rep(..), RowRep(..))
import Polytropos.RowMorph (rowMorph)
import Data.Primitive.SmallArray (smallArrayFromList)

main :: IO ()
main = hspec $ do
  describe "Penelope.Backend.Gimple" $ do
    it "emits GIMPLE SSA entry and return for Route term" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route m
          initFrame :: RecordF GimpleVal '["x" ':= Int]
          initFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# (GimpleVal "r_3(D)")]))
          out = compileWithBackend gimpleBackendPlugin defaultHostTargetLayout RTSFreestandingLinear term initFrame
      T.isInfixOf "penelope_gimple_entry" out `shouldBe` True
      T.isInfixOf "return" out `shouldBe` True

    it "emits GIMPLE slot update with freestanding linear zero RC branch" $ do
      let plan = PlaceInPlaceLinear 0 (unsafeCoerce# ())
          (_, _, _, linesOut, GimpleVal dst) =
            emitGimpleComponentRefStore RTSFreestandingLinear plan (GimpleVal "r_3(D)") 0 (GimpleVal "val_1") 1 2 1
      dst `shouldBe` "r_3(D)"
      linesOut `shouldBe`
        [ "  // #[freestanding_linear] PlaceInPlaceLinear: zero RC branch"
        , "  // # .MEM_2 = VDEF <.MEM_1>"
        , "  r_3(D)->slots[0] = val_1;"
        ]
EOF

echo "=== [TP-007C] Building and testing ==="
cabal v2-build penelope-backend-mlir penelope-backend-gimple -j20 --semaphore
cabal v2-test penelope-backend-mlir:test:tests penelope-backend-gimple:test:tests

echo "=== [TP-007C] Script execution complete ==="
