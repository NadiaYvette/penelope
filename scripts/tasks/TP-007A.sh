#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007A] Deploying Penelope.Perceus ==="

mkdir -p packages/penelope-core/src/Penelope

# 1. Mount seed
cp -f /home/nyc/src/plans/seeds/TP-007A-Perceus.hs packages/penelope-core/src/Penelope/Perceus.hs

# 2. Expose Penelope.Perceus in penelope-core.cabal
python3 -c '
with open("packages/penelope-core/penelope-core.cabal", "r") as f:
    content = f.read()
if "Penelope.Perceus" not in content:
    content = content.replace("Penelope.Core\n", "Penelope.Core\n    Penelope.Perceus\n")
with open("packages/penelope-core/penelope-core.cabal", "w") as f:
    f.write(content)
'

# 3. Add tests to packages/penelope-core/test/Main.hs if not present
python3 -c '
with open("packages/penelope-core/test/Main.hs", "r") as f:
    content = f.read()
if "Penelope.Perceus" not in content:
    content = content.replace("import Penelope.Core\n", "import Penelope.Core\nimport Penelope.Perceus\nimport qualified Data.IntMap.Strict as IM\nimport qualified Data.IntSet as IS\n")
    test_block = """
  describe "Penelope.Perceus" $ do
    it "gathers histogram from a RowMorph" $ do
      let m = rowMorph @\x27["x" \x27:= Int] @\x27["x" \x27:= Int]
          hist = gatherHistogram m
      IM.lookup 0 hist `shouldBe` Just 1

    it "analyzes liveness of a simple Route term" $ do
      let m = rowMorph @\x27["x" \x27:= Int] @\x27["x" \x27:= Int, "y" \x27:= Bool]
          term :: Penelope \x27[] \x27[] \x27["x" \x27:= Int, "y" \x27:= Bool] \x27["x" \x27:= Int]
          term = Route m
          live = analyzePerceusLiveness term
      IS.member 0 live `shouldBe` True

    it "computes Perceus place plan for in-place reuse in freestanding linear profile" $ do
      let m = rowMorph @\x27["x" \x27:= Int] @\x27["x" \x27:= Int]
          boxedIn = IM.singleton 0 (SizeClass 1)
          liveIn = IS.singleton 0
          branchLives = [IS.empty]
          boxedOut = IM.singleton 0 (SizeClass 1)
          plan = computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn m branchLives boxedOut
      IM.lookup 0 (pnOutputPlaces plan) `shouldBe` Just (PlaceInPlaceLinear 0 (SizeClass 1))

    it "computes Perceus place plan for fresh alloc when no reusable slot matches" $ do
      let m = rowMorph @\x27["x" \x27:= Int] @\x27["x" \x27:= Int]
          boxedIn = IM.empty
          liveIn = IS.empty
          branchLives = [IS.empty]
          boxedOut = IM.singleton 0 (SizeClass 1)
          plan = computeNodePerceusPlan RTSFreestandingLinear boxedIn liveIn m branchLives boxedOut
      IM.lookup 0 (pnOutputPlaces plan) `shouldBe` Just (PlaceFreshAlloc (SizeClass 1))
"""
    content += "\n" + test_block
with open("packages/penelope-core/test/Main.hs", "w") as f:
    f.write(content)
'

echo "=== [TP-007A] Building and testing penelope-core ==="
cabal v2-build penelope-core -j20 --semaphore
cabal v2-test penelope-core:test:tests

echo "=== [TP-007A] Script execution complete ==="
