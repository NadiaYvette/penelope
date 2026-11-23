#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-PENELOPE-URING-001-verify] Starting physical verification ==="

test -f docs/notes/IO_URING_RTS_PENELOPE.md
cabal --with-compiler=ghc-9.14.1 v2-build --ghc-options=-Werror penelope-uring
cabal --with-compiler=ghc-9.14.1 v2-test penelope-uring
mkdir -p packages/penelope-uring/dist-newstyle dist-newstyle
cabal --with-compiler=ghc-9.14.1 v2-bench penelope-uring:bench:penelope-uring-bench --benchmark-options='--timeout 2 --csv=dist-newstyle/penelope-uring-bench.csv +RTS -T -RTS'
(cd packages/penelope-uring && cabal check)
./bin/penelope-verify

echo "=== [TP-PENELOPE-URING-001-verify] All verification checks passed ==="
