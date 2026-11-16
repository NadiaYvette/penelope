#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007A-verify] Verifying Penelope.Perceus ==="
cabal v2-test penelope-core:test:tests
echo "=== [TP-007A-verify] All checks passed ==="
