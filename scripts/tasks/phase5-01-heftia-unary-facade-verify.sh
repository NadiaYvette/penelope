#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase5-01-heftia-unary-facade
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 5.1: Heftia Unary Monad Facade (Eff) ==="

# 1. Verify penelope-effects builds cleanly
echo "  -> Running cabal build penelope-effects..."
cabal build penelope-effects

# 2. Run test suite
echo "  -> Running cabal test penelope-effects:test:tests..."
cabal test penelope-effects:test:tests

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Zero-Cost Surface Specializations]..."
grep -rnF "[ref:note-surface-facades]" packages/penelope-effects/src/

# 4. Audit for zero partial functions
echo "  -> Auditing penelope-effects for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-effects/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-effects/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 5.1 verification checks passed successfully! ==="
