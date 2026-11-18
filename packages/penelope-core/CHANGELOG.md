# Changelog for penelope-core

All notable changes to `penelope-core` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## 0.1.1.0 — 2026-11-18

### Added
- Perceus in-place reference count analysis engine (`Penelope.Perceus`):
  - `gatherHistogram`: Multiplicity and slot usage counter.
  - `analyzePerceusLiveness`: Precise slot liveness tracking across branch continuations.
  - `computeNodePerceusPlan`: In-place mutation planning (`PlaceInPlaceLinear`, `PlaceInPlaceDiamond`).
- Extensible backend compilation interface (`Penelope.Backend.Plugin`):
  - `BackendPlugin` abstraction with carrier registration.
  - `compileWithBackend` dispatch supporting GCC `-fgimple` and MLIR lowering.
- Dual licensing under `EUPL-1.2 OR Apache-2.0`.

## 0.1.0.0 — Initial Release

- Core polycategorical engine generalizing Heftia and Kernmantle.
- Unpacked `(m,n,k,r)`-ary polycategorical syntax GADT: `Polycategorical`.
- $O(1)$ jump-table route optimization via Polytropos row indexing.
- High-level smart constructors: `inject`, `fanout`, `assocL`, `assocR`, `swapSum`, `swapProd`, `distributeL`, `distributeR`.
- Sequential polycategorical composition (`composePoly`, `>>>`).
- Categorical frame rule wiring (`frame`, `cross`).
- Disjunctive polycategorical fanout (`fanoutDisj`).
- Row weakening and structural embedding (`weakenPoly`).
- Higher-order algebraic elaboration and vector interpretation engine.
- Partial interpretation substrate with structural residualization.
