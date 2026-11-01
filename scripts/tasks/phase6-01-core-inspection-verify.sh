#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase6-01-core-inspection
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 6.1: GHC Core Inspection (inspection-testing) ==="

# 1. Verify penelope-core builds cleanly
echo "  -> Running cabal build penelope-core..."
cabal build penelope-core

# 2. Run inspection test suite
echo "  -> Running cabal test penelope-core:test:inspection..."
cabal test penelope-core:test:inspection

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Core Architecture] and Note [Unpacked Polycategorical Representation]..."
grep -rnF "[ref:note-core-architecture]" packages/penelope-core/test/Inspection.hs
grep -rnF "[ref:note-unpacked-representation]" packages/penelope-core/test/Inspection.hs

# 4. Audit for zero partial functions in packages/penelope-core/src/
echo "  -> Auditing penelope-core for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-core/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-core/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 6.1 verification checks passed successfully! ==="
