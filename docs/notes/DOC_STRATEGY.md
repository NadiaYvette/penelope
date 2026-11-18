# Documentation Strategy & Policy

**Project:** penelope  
**Index:** [`../INDEX.md`](../INDEX.md)

## Precedence Rule (§4)

Code wins at runtime; notes corrected regardless. Stale design documentation must be remediated in the same commit.

{- Note [Forge-Tiered CI Resource Allocation and Static Gate Partitioning]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-forge-tiered-ci]
In multi-forge environments (GitHub Actions, GitLab CI / Framagit, Radicle),
continuous integration pipelines are strictly partitioned to conserve compute
and provide instantaneous diagnostic feedback:

1. **Tier 1 (Fast Static Gates):** Tagref cross-reference verification, markdown
   link integrity checking, HLint static analysis, and CLOC comment density checks
   execute within seconds on lightweight runners without compiler downloads.
2. **Tier 2 (Full Compilation & Test Suites):** Multi-package Cabal builds, Hspec
   unit test execution, and fuel-bounded execution validation execute across
   supported GHC releases.
-}

Every CI manifest (`.github/workflows/ci.yml`, `.gitlab-ci.yml`) must preserve
this tiered execution structure.

