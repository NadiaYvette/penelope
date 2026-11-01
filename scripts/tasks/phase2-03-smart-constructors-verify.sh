#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase2-03-smart-constructors
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 2.3: Zero-Overhead Smart Constructors ==="

# 1. Verify penelope-core builds cleanly
echo "  -> Running cabal build penelope-core..."
cabal build penelope-core

# 2. Run test suite
echo "  -> Running cabal test penelope-core:test:tests..."
cabal test penelope-core:test:tests

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Unpacked Polycategorical Representation]..."
grep -rnF "[ref:note-unpacked-representation]" packages/penelope-core/src/

# 4. Audit for zero partial functions
echo "  -> Auditing penelope-core for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-core/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-core/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 2.3 verification checks passed successfully! ==="
