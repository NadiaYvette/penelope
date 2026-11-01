#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase6-04-benchmarks
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 6.4: Allocation & Throughput Benchmarks ==="

# 1. Verify penelope-effects:bench builds cleanly
echo "  -> Running cabal build penelope-effects:bench..."
cabal build penelope-effects:bench

# 2. Run benchmark suite with statistical variance check
echo "  -> Running cabal bench penelope-effects:bench --benchmark-options=\"--stdev 10\"..."
cabal bench penelope-effects:bench --benchmark-options="--stdev 10"

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Core Architecture], Note [Unpacked Polycategorical Representation], Note [Reified Row Morphisms and Gather-Scatter], Note [Two-Phase Weaving and Unweaving], and Note [Zero-Cost Surface Specializations]..."
grep -rnF "[ref:note-core-architecture]" packages/penelope-effects/bench/Main.hs
grep -rnF "[ref:note-unpacked-representation]" packages/penelope-effects/bench/Main.hs
grep -rnF "[ref:note-row-morph-routing]" packages/penelope-effects/bench/Main.hs
grep -rnF "[ref:note-two-phase-engine]" packages/penelope-effects/bench/Main.hs
grep -rnF "[ref:note-surface-facades]" packages/penelope-effects/bench/Main.hs

# 4. Audit for zero forbidden partial functions in packages/penelope-effects/bench/
echo "  -> Auditing penelope-effects/bench for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-effects/bench/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-effects/bench/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 6.4 verification checks passed successfully! ==="
