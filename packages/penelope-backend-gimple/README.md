# penelope-backend-gimple

**GCC `-fgimple` SSA Tuples Emission Backend for Penelope.**

`penelope-backend-gimple` is a code-generation backend for the Penelope polycategorical effect framework. It lowers typed Penelope polycategorical ASTs into GCC `-fgimple` SSA (Static Single Assignment) tuples with explicit virtual memory state tracking.

---

## Architectural Overview

GCC's `-fgimple` format provides a human-readable and machine-verifiable representation of the compiler's high-level SSA intermediate representation. `penelope-backend-gimple` generates C-compatible GIMPLE syntax that can be compiled directly by GCC or inspected during staging verification.

```
[Penelope Hypergraph: Penelope '[] ef ins outs]
                    │
                    ▼
       [gimplePolyCarrier / Handler]
                    │
                    ▼
     [Virtual Memory SSA Threading (.MEM)]
                    │
                    ▼
  [GCC -fgimple SSA Tuples Output]
```

### Key Technical Characteristics

1. **Virtual Memory SSA Token Threading:**  
   Every in-place mutation or memory access threads virtual memory tokens:
   ```c
   // # .MEM_2 = VDEF <.MEM_1>
   r_3(D)->slots[0] = val_4;
   ```
   This ensures strict sequencing and prevents compiler optimizations from reordering stateful effect operations.

2. **In-Place Mutation with Perceus Plans:**  
   The function `emitGimpleComponentRefStore` utilizes the `PlacePlan` and `RTSProfile` determined by the staging engine:
   - For `RTSFreestandingLinear` (Profile 0), direct in-place mutation occurs without reference-count checks.
   - For dynamic profiles, it synthesizes Lean 4-style uniqueness checks (`m_rc == 1`) with in-place reuse or unsharing fallback.

3. **Backend Plugin Integration:**  
   Exposes `gimpleBackendPlugin` conforming to Penelope's standard `BackendPlugin` contract (`Penelope.Backend.Plugin`), allowing plug-and-play selection in `stageFreestandingModule`.

---

## Core Types and API

```haskell
-- | Symbolic GIMPLE SSA variable name (e.g. `r_3(D)`, `res_5`).
newtype GimpleVal (a :: Type) = GimpleVal { unGimpleVal :: Text }

-- | Threads `(ssaVarCounter, bbCounter, memTokenCounter)` to emit valid `-fgimple` SSA.
newtype GimpleCarrier (outs :: Row) = GimpleCarrier
  { runGimpleCarrier :: Int -> Int -> Int -> (Int, Int, Int, [Text], Any) }

-- | Emits an in-place GIMPLE COMPONENT_REF + ARRAY_REF store.
emitGimpleComponentRefStore
  :: RTSProfile
  -> PlacePlan
  -> GimpleVal a
  -> Int
  -> GimpleVal b
  -> Int -> Int -> Int
  -> (Int, Int, Int, [Text], GimpleVal a)

-- | Backend plugin instance for Penelope staging.
gimpleBackendPlugin :: BackendPlugin GimpleVal GimpleCarrier Text
```

---

## Modules

- `Penelope.Backend.Gimple`: SSA variable types, carrier monad, GIMPLE store emission, and backend plugin definition.

---

## License

Dual-licensed under either:
- **European Union Public Licence (EUPL-1.2)**
- **Apache License, Version 2.0 (Apache-2.0)**

See [LICENSE](LICENSE) for full legal text.
