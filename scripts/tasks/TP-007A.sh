#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007A] Deploying Penelope.Perceus ==="

mkdir -p packages/penelope-core/src/Penelope

# 1. Mount seed
cp -f /home/nyc/src/plans/seeds/TP-007A-Perceus.hs packages/penelope-core/src/Penelope/Perceus.hs

# 2. Expose Penelope.Perceus in penelope-core.cabal and add containers to test-suite if missing
if ! grep -q "Penelope.Perceus" packages/penelope-core/penelope-core.cabal; then
  sed -i '/Penelope.Core$/a \    Penelope.Perceus' packages/penelope-core/penelope-core.cabal
fi

if ! grep -A 10 "test-suite tests" packages/penelope-core/penelope-core.cabal | grep -q "containers"; then
  sed -i '/test-suite tests/,/build-depends:/ { /build-depends:/a \    containers >= 0.6 && < 0.9,
}' packages/penelope-core/penelope-core.cabal
fi

# 3. Add tests to packages/penelope-core/test/Main.hs if not present
if ! grep -q "Penelope.Perceus" packages/penelope-core/test/Main.hs; then
  sed -i '/import Penelope.Core$/a import Penelope.Perceus\nimport qualified Data.IntMap.Strict as IM\nimport qualified Data.IntSet as IS' packages/penelope-core/test/Main.hs
  cat << 'EOF' >> packages/penelope-core/test/Main.hs

  describe "Penelope.Perceus" $ do
    it "gathers histogram from a RowMorph" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          hist = gatherHistogram m
      IM.lookup 0 hist `shouldBe` Just 1

    it "analyzes liveness of a simple Route term" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int, "y" ':= Bool]
          term :: Penelope '[] '[] '["x" ':= Int, "y" ':= Bool] '["x" ':= Int]
          term = Route m
          live = analyzePerceusLiveness term
      IS.member 0 live `shouldBe` True

    it "computes Perceus place plan for in-place reuse in freestanding linear profile" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          boxedIn = IM.singleton 0 (SizeClass 1)
          liveIn = IS.singleton 0
          branchLives = [IS.empty]
          boxedOut = IM.singleton 0 (SizeClass 1)
          plan = computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn m branchLives boxedOut
      IM.lookup 0 (pnOutputPlaces plan) `shouldBe` Just (PlaceInPlaceLinear 0 (SizeClass 1))

    it "computes Perceus place plan for fresh alloc when no reusable slot matches" $ do
      let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
          boxedIn = IM.empty
          liveIn = IS.empty
          branchLives = [IS.empty]
          boxedOut = IM.singleton 0 (SizeClass 1)
          plan = computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn m branchLives boxedOut
      IM.lookup 0 (pnOutputPlaces plan) `shouldBe` Just (PlaceFreshAlloc (SizeClass 1))
EOF
fi

echo "=== [TP-007A] Building and testing penelope-core ==="
cabal v2-build penelope-core -j20 --semaphore
cabal v2-test penelope-core:test:tests

echo "=== [TP-007A] Script execution complete ==="
