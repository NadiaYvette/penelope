#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007B-verify] Verifying penelope-core and penelope-stage ==="
cabal v2-test penelope-stage:test:tests
echo "=== [TP-007B-verify] Verification complete ==="
