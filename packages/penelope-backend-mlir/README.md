# penelope-backend-mlir

**MLIR Structured Dialect Code Emission Backend for Penelope.**

`penelope-backend-mlir` is a code-generation backend for the Penelope polycategorical effect framework. It lowers typed Penelope polycategorical ASTs into MLIR (Multi-Level Intermediate Representation) SSA blocks and function regions with dynamic Perceus in-place reuse annotations.

---

## Architectural Overview

MLIR provides extensible intermediate representation infrastructure for modular compilers. `penelope-backend-mlir` compiles polycategorical hypergraphs into structured function regions using LLVM pointer types and domain-specific `organ` dialect operations.

```
[Penelope Hypergraph: Penelope '[] ef ins outs]
                    │
                    ▼
        [mlirPolyCarrier / Handler]
                    │
                    ▼
     [SSA Block & Region CFG Construction]
                    │
                    ▼
  [MLIR Structured Dialect SSA Code Output]
```

### Key Technical Characteristics

1. **Structured Region CFG Lowering:**  
   The function `emitMLIRRegionCFG` wraps evaluated carriers into standard MLIR function syntax:
   ```mlir
   func.func @kernel_pipeline(%ev_vec: !llvm.ptr, %arg0: !llvm.ptr) -> !llvm.ptr {
     ^bb_entry(%ev_0: !llvm.ptr, %in_0: !llvm.ptr):
       ...
       func.return %res_0 : !llvm.ptr
   }
   ```

2. **Perceus Reuse Annotations:**  
   The function `emitMLIRSlotUpdate` emits `organ.rep.slot_update` operations with explicit compile-time reuse attributes derived from the static `PlacePlan`:
   ```mlir
   %r_2 = organ.rep.slot_update { slot = 0 : i32, reuse = true } %r_1, %val_0 : !llvm.ptr
   ```
   Downstream optimization passes can exploit these reuse guarantees to lower directly to in-place mutation without atomic reference count updates.

3. **Backend Plugin Integration:**  
   Exposes `mlirBackendPlugin` conforming to Penelope's standard `BackendPlugin` contract (`Penelope.Backend.Plugin`), facilitating uniform staging in `stageFreestandingModule`.

---

## Core Types and API

```haskell
-- | Symbolic MLIR SSA register name (`%0`, `%arg0`, `%ev_vec`).
newtype SSAVal (a :: Type) = SSAVal { unSSAVal :: Text }

-- | State-carrying MLIR region/block emitter.
newtype MLIRCarrier (outs :: Row) = MLIRCarrier
  { runMLIRCarrier :: Int -> (Int, [Text], Any) }

-- | Emit an MLIR region CFG from an evaluated `MLIRCarrier`.
emitMLIRRegionCFG :: Text -> MLIRCarrier outs -> Text

-- | Emit an MLIR `organ.rep.slot_update` instruction respecting the `PlacePlan`.
emitMLIRSlotUpdate
  :: PlacePlan
  -> SSAVal a
  -> Int
  -> SSAVal b
  -> (Int -> (Int, [Text], SSAVal a))

-- | Backend plugin instance for Penelope staging.
mlirBackendPlugin :: BackendPlugin SSAVal MLIRCarrier Text
```

---

## Modules

- `Penelope.Backend.MLIR`: SSA register types, region CFG emission, slot update generation, and backend plugin definition.

---

## License

Dual-licensed under either:
- **European Union Public Licence (EUPL-1.2)**
- **Apache License, Version 2.0 (Apache-2.0)**

See [LICENSE](LICENSE) for full legal text.
