#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase4-01-static-analysis
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 4.1: Static Pre-Execution Cayley Analysis ==="

# 1. Verify penelope-static builds cleanly
echo "  -> Running cabal build penelope-static..."
cabal build penelope-static

# 2. Run test suite
echo "  -> Running cabal test penelope-static:test:tests..."
cabal test penelope-static:test:tests

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Static Reflection and Cayley Analysis]..."
grep -rnF "[ref:note-static-cayley]" packages/penelope-static/src/

# 4. Audit for zero partial functions
echo "  -> Auditing penelope-static for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-static/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-static/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 4.1 verification checks passed successfully! ==="
