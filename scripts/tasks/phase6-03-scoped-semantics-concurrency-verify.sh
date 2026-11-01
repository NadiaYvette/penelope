#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Verification script for phase6-03-scoped-semantics-concurrency
set -euo pipefail

echo "=== [Verification Gate] Auditing Milestone 6.3: Heftia Scoped Semantics & Concurrency Test Suite ==="

# 1. Verify penelope-effects builds cleanly
echo "  -> Running cabal build penelope-effects..."
cabal build penelope-effects

# 2. Run test suite
echo "  -> Running cabal test penelope-effects:test:tests..."
cabal test penelope-effects:test:tests

# 3. Check for architectural design note references
echo "  -> Auditing for Note [Core Architecture], Note [Zero-Cost Surface Specializations], and Note [Two-Phase Weaving and Unweaving]..."
grep -rnF "[ref:note-core-architecture]" packages/penelope-effects/src/Penelope/Effects/Scoped.hs
grep -rnF "[ref:note-surface-facades]" packages/penelope-effects/src/Penelope/Effects/Scoped.hs
grep -rnF "[ref:note-two-phase-engine]" packages/penelope-effects/src/Penelope/Effects/Scoped.hs
grep -rnF "[ref:note-core-architecture]" packages/penelope-effects/src/Penelope/Effects/Concurrent.hs
grep -rnF "[ref:note-surface-facades]" packages/penelope-effects/src/Penelope/Effects/Concurrent.hs

# 4. Audit for zero partial functions in packages/penelope-effects/src/
echo "  -> Auditing penelope-effects for zero partial functions..."
if grep -rn -E "(\bhead\b|\btail\b|\bfromJust\b|!!)" packages/penelope-effects/src/; then
  echo "ERROR: Found forbidden partial functions in packages/penelope-effects/src/!"
  exit 1
fi
echo "  [PASS] Zero partial functions verified."

echo "=== [Verification Gate] Milestone 6.3 verification checks passed successfully! ==="
