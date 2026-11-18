# Changelog for penelope-stage

All notable changes to `penelope-stage` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## 0.1.1.0 — 2026-11-18

### Added
- 2-Level Type Theory (2LTT) staging engine module `Penelope.Stage.OrganIR`.
- `verifyFreestandingLinear`: static verification of `RTSFreestandingLinear` (Profile 0) invariants for kernel code lowering.
- `StagedKernelError` data type:
  - `ErrOpaqueClosureInKernelStage` for rejecting opaque Haskell heap closures (`LiftPure`).
  - `ErrNonLinearSlotDuplication` for affine and linear multiplicity checks on boxed Polytropos slots.
- `stageFreestandingModule`: combined verification and backend compilation dispatch via `BackendPlugin`.
- Invariant verification test suite with `hspec`.
- Dual licensing under `EUPL-1.2 OR Apache-2.0`.

## 0.1.0.0 — Initial Release

- Initial design and stub specification for Penelope staging pipeline.
