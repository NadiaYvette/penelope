#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007C-verify] Verifying penelope backends ==="
cabal v2-test penelope-backend-mlir:test:tests penelope-backend-gimple:test:tests
echo "=== [TP-007C-verify] Verification complete ==="
