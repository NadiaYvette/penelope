# Changelog for penelope-effects

All notable changes to `penelope-effects` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## 0.1.1.0 — 2026-11-18

### Added
- Convenience interpreter effects module `Penelope.Effects.Interpreter`:
  - `StepBudget` effect for bounded execution steps and cycle detection.
  - `FreshSym` effect for generating fresh hygienic AST names and SSA registers.
  - `MultiExitTrampoline` effect for managing non-local exits across multi-branch continuations.
  - `TraceStep` effect for execution trace recording.
- Highly optimized vector handlers for standard effects.
- Dual licensing under `EUPL-1.2 OR Apache-2.0`.

## 0.1.0.0 — Initial Release

- Domain effect battery for Penelope.
- Heftia-style unary monadic facade (`Eff`, `sendUnary`, `interpretUnary`).
- Kernmantle-style binary profunctor facade (`Flow`, `sendBinary`, `interpretBinary`).
- Standard effect suite: `State`, `Reader`, `Writer`, `Error`, `Coroutine`, `UnliftIO`, `Scoped`, `Concurrent`.
- Scoped semantics with robust continuation capturing and branch safety.
- Comprehensive allocation and throughput benchmarks.
