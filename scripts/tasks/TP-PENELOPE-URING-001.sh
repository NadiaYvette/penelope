#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-PENELOPE-URING-001] Setting up penelope-uring in worktree ==="

STAGING="/tmp/penelope-uring-staging"

# 1. Populate packages/penelope-uring from staging
mkdir -p packages/penelope-uring
cp -r "${STAGING}"/* packages/penelope-uring/
rm -f packages/penelope-uring/penelope-verify packages/penelope-uring/INDEX.md packages/penelope-uring/BENCHMARKS.md

# 2. Register packages/penelope-uring in cabal.project if not present
if ! grep -q "packages/penelope-uring" cabal.project; then
  sed -i '/packages\/penelope-backend-gimple/a \    packages/penelope-uring' cabal.project
fi

# 3. Update docs/INDEX.md
cp -f "${STAGING}/INDEX.md" docs/INDEX.md

# 4. Update docs/notes/BENCHMARKS.md
cp -f "${STAGING}/BENCHMARKS.md" docs/notes/BENCHMARKS.md

# 5. Update bin/penelope-verify
cp -f "${STAGING}/penelope-verify" bin/penelope-verify
chmod +x bin/penelope-verify

echo "=== [TP-PENELOPE-URING-001] Setup completed successfully ==="
