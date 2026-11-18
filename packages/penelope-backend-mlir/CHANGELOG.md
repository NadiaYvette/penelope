# Changelog for penelope-backend-mlir

All notable changes to `penelope-backend-mlir` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## 0.1.1.0 — 2026-11-18

### Added
- MLIR structured dialect code emission module `Penelope.Backend.MLIR`.
- `SSAVal` symbolic MLIR SSA register type.
- `MLIRCarrier` state monad for region and basic block accumulation.
- `emitMLIRRegionCFG` for constructing function regions and terminators.
- `emitMLIRSlotUpdate` generating `organ.rep.slot_update` operations with Perceus reuse annotations.
- `mlirBackendPlugin` integration for `Penelope.Stage.OrganIR`.
- HSpec test suite validating MLIR SSA code generation.
- Dual licensing under `EUPL-1.2 OR Apache-2.0`.

## 0.1.0.0 — Initial Release

- Initial design and stub specification for Penelope MLIR backend.
