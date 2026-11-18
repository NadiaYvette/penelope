# penelope-stage

**2-Level Type Theory (2LTT) Staging & OrganIR Lowering Pipeline for Penelope.**

`penelope-stage` provides the 2-Level Type Theory (2LTT) meta-programming and staging engine for the Penelope polycategorical effect framework. It allows developers to construct, type-check, and algebraically compose high-level Penelope effect circuits at the meta-level (GHC Haskell), statically verify strict zero-RTS kernel invariants, and lower the resulting hypergraph to object-level freestanding machine code or compiler intermediate representations (GCC `-fgimple`, MLIR).

---

## Architectural Overview

Penelope separates meta-programming from target execution using a two-level model:

```
[Meta-Level: GHC Haskell eDSL]
  │  Construct & elaborate polycategorical hypergraph (`Penelope '[] ef ins outs`)
  │  Compute static Perceus liveness & allocation notes
  ▼
[Staging Verification: verifyFreestandingLinear]
  │  Ensure zero opaque Haskell heap closures (`LiftPure`)
  │  Enforce strict multiplicity <= 1 on boxed Polytropos slots
  ▼
[Object-Level Lowering: stageFreestandingModule]
  ├──> GCC -fgimple SSA tuples (`penelope-backend-gimple`)
  └──> MLIR Structured SSA dialects (`penelope-backend-mlir`)
```

### 1. Meta-Level vs. Object-Level Separation

- **Meta-Level (Stage 1):** Full Haskell type system, row polymorphism (`polytropos`), effect composition, and higher-order elaboration (`elaborateAll`).
- **Object-Level (Stage 0):** Zero-RTS freestanding linear kernel code (`RTSFreestandingLinear`), executing with:
  * No garbage collector or GHC runtime system dependency.
  * No dynamic heap allocation or boxing.
  * Direct in-place mutation of contiguous unboxed `Polytropos` representations (`Rep`).

---

## Freestanding Linear Invariants (`#[freestanding_linear]`)

When targeting freestanding kernels (`telix` / `pgcl`), the staging engine enforces Profile 0 invariants via `verifyFreestandingLinear`:

1. **Zero Opaque Closures (`ErrOpaqueClosureInKernelStage`):**  
   Freestanding kernels cannot evaluate opaque GHC function closures (`LiftPure`). All operations must consist purely of reified row morphisms (`Route`) or explicit algebraic operations (`OpF`).
2. **Affine & Linear Resource Multiplicity (`ErrNonLinearSlotDuplication`):**  
   Every boxed slot in the input representation (`boxedIn :: IntMap SizeClass`) may be consumed at most once across the hypergraph branches (multiplicity $\le 1$). Duplication without explicit copying capabilities is rejected at staging time.

---

## Verification & Staging API

The module `Penelope.Stage.OrganIR` exposes:

```haskell
-- | Verification error when lowering to freestanding kernel code.
data StagedKernelError
  = ErrOpaqueClosureInKernelStage
  | ErrNonLinearSlotDuplication !Int !Int
  deriving (Show, Eq)

-- | Statically verify that a first-order circuit satisfies Profile 0.
verifyFreestandingLinear
  :: IntMap SizeClass
  -> Penelope '[] ef ins outs
  -> Either StagedKernelError PerceusNote

-- | Verify and stage a module into a backend artifact (MLIR, GIMPLE).
stageFreestandingModule
  :: Text
  -> TargetLayout
  -> IntMap SizeClass
  -> BackendPlugin val carrier artifact
  -> RecordF val ins
  -> Penelope '[] ef ins outs
  -> Either StagedKernelError artifact
```

---

## Example: Verifying a Staged Circuit

```haskell
{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications #-}
import qualified Data.IntMap.Strict as IM
import Penelope.Core
import Penelope.Perceus
import Penelope.Stage.OrganIR
import Polytropos.RowMorph (rowMorph)

-- Verify that an identity route satisfies freestanding linear constraints:
verifyRoute :: Either StagedKernelError PerceusNote
verifyRoute =
  let m = rowMorph @'["x" ':= Int] @'["x" ':= Int]
      term = Route m
      boxedIn = IM.singleton 0 (SizeClass 1)
  in verifyFreestandingLinear boxedIn term
```

---

## Modules

- `Penelope.Stage.OrganIR`: Staging verification and backend plugin compilation entrypoints.

---

## License

Dual-licensed under either:
- **European Union Public Licence (EUPL-1.2)**
- **Apache License, Version 2.0 (Apache-2.0)**

See [LICENSE](LICENSE) for full legal text.
