#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase6-02-reduct-properties
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 6.2: Institutional Satisfaction & Reduct Property Tests ==="

# 1. Verify penelope-static builds cleanly
echo "  -> Running cabal build penelope-static..."
cabal build penelope-static

# 2. Run property test suite
echo "  -> Running cabal test penelope-static:test:properties..."
cabal test penelope-static:test:properties

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Core Architecture], Note [Institutional Satisfaction and Zero-Traversal Weakening], Note [Two-Phase Weaving and Unweaving], and Note [Static Reflection and Cayley Analysis]..."
grep -rnF "[ref:note-core-architecture]" packages/penelope-static/test/Properties.hs
grep -rnF "[ref:note-satisfaction-weakening]" packages/penelope-static/test/Properties.hs
grep -rnF "[ref:note-two-phase-engine]" packages/penelope-static/test/Properties.hs
grep -rnF "[ref:note-static-cayley]" packages/penelope-static/test/Properties.hs

# 4. Audit for zero partial functions in packages/penelope-static/src/
echo "  -> Auditing penelope-static for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-static/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-static/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 6.2 verification checks passed successfully! ==="
