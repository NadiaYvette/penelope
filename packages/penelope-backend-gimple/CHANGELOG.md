# Changelog for penelope-backend-gimple

All notable changes to `penelope-backend-gimple` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## 0.1.1.0 — 2026-11-18

### Added
- GCC `-fgimple` SSA tuple emission module `Penelope.Backend.Gimple`.
- `GimpleVal` symbolic SSA variable wrapper.
- `GimpleCarrier` monad threading SSA variable, basic block, and virtual `.MEM` counters.
- `emitGimpleComponentRefStore` generating in-place `COMPONENT_REF` / `ARRAY_REF` assignments with `VDEF` virtual memory tokens.
- `gimpleBackendPlugin` integration for `Penelope.Stage.OrganIR`.
- HSpec test suite verifying GIMPLE SSA generation and memory state sequencing.
- Dual licensing under `EUPL-1.2 OR Apache-2.0`.

## 0.1.0.0 — Initial Release

- Initial design and stub specification for Penelope GIMPLE backend.
