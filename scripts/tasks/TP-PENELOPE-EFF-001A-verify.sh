#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-PENELOPE-EFF-001A Verification Gate] ==="
bin/penelope-verify
cabal v2-test penelope-effects
echo "=== [TP-PENELOPE-EFF-001A Verification Gate PASSED] ==="
