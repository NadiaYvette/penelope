#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase2-02-route-optimizer
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 2.2: Pure Route Peephole Optimizer ==="

# 1. Verify penelope-core builds cleanly
echo "  -> Running cabal build penelope-core..."
cabal build penelope-core

# 2. Run test suite
echo "  -> Running cabal test penelope-core:test:tests..."
cabal test penelope-core:test:tests

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Reified Row Morphisms and Gather-Scatter]..."
grep -rnF "[ref:note-row-morph-routing]" packages/penelope-core/src/

# 4. Audit for zero partial functions
echo "  -> Auditing penelope-core for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-core/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-core/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 2.2 verification checks passed successfully! ==="
