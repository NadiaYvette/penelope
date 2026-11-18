# penelope-core

**Unpacked $(m,n,k,r)$-ary Polycategorical Effect System Core for Haskell.**

`penelope-core` is the core architectural engine of Penelope, generalizing both higher-order algebraic effects ([Heftia](https://hackage.haskell.org/package/heftia)) and binary profunctor arrow pipelines ([Kernmantle](https://hackage.haskell.org/package/kernmantle)) into a unified polycategorical representation backed by [`polytropos`](https://github.com/NadiaYvette/polytropos) open row polymorphism.

---

## The Polycategorical Paradigm

Traditional effect systems force a choice between two distinct computational models:
1. **Monadic Effect Systems:** Excellent for scoped higher-order handlers, but restricted to single-input, single-output pipelines ($1 \to 1$). Branching requires nested `Either` types, and multi-port parallel processing incurs tuple allocation cascades.
2. **Arrow / Profunctor Pipelines:** Support multi-wire routing, but suffer from the "Arrow Plumbing Trap" (continual nesting of pairs `((a, b), (c, d))`) and lack native higher-order delimited scoping.

`penelope-core` resolves this dichotomy through **$(m,n,k,r)$-ary polycategories**:

$$\text{Node} : \mathcal{C}\left(\bigotimes_{i=1}^m A_i, \bigoplus_{j=1}^k B_j \oplus \bigotimes_{l=1}^n C_l\right)$$

- **$m$ Conjunctive Input Ports:** Unboxed input registers indexed by open Polytropos row types.
- **$n$ Conjunctive Output Ports:** Primary output registers yielded on standard completion.
- **$k$ Disjunctive Control Exits:** First-class variant branching exits without `Either` nesting.
- **$r$ Higher-Order Scoped Regions:** Delimited continuation regions (`Local`, `Catch`, `Transaction`).

---

## Architectural Pillars

### 1. $O(1)$ Jump-Table Route Optimization

All record and variant operations in Penelope are lowered to flat arrays via Polytropos row morphisms (`RowMorph`). Rather than traversing type-level lists at runtime, routing between input and output ports evaluates in $O(1)$ through unboxed index jump tables:

```haskell
-- Route wires from inputs to outputs with zero heap allocation:
term = Route (rowMorph @'["x" ':= Int, "y" ':= Text] @'["y" ':= Text])
```

### 2. Two-Phase Execution (Weaving & Unweaving)

- **Phase 1: Higher-Order Elaboration (`elaborateAll`):**  
  Higher-order operations (`OpH`) and scoped blocks are systematically lowered into first-order algebraic operations (`OpF`) and reified wiring nodes (`Route`).
- **Phase 2: Vector Interpretation (`interpretAll`):**  
  First-order algebraic nodes are evaluated against vectorized handlers (`Handler`), unweaving effect layers directly into carrier representations without intermediate re-boxing.

### 3. Dynamic Perceus Reuse (`Penelope.Perceus`)

`penelope-core` incorporates Perceus-style reference count analysis:
- **Histogram Gathering (`gatherHistogram`):** Counts port usage across branches.
- **Liveness Analysis (`analyzePerceusLiveness`):** Detects dead bindings.
- **In-Place Mutation Planning (`computeNodePerceusPlan`):** Emits `PlaceInPlaceLinear` or `PlaceInPlaceDiamond` plans, allowing memory buffers to be mutated directly when uniqueness is proven.

---

## Module Hierarchy

- `Penelope.Core`: Top-level facade exporting syntax, smart constructors, and composition combinators.
- `Penelope.Core.Syntax`: The unpacked polycategorical GADT (`Penelope`).
- `Penelope.Core.Route`: $O(1)$ row morphism wire routing and permutation.
- `Penelope.Core.Send`: Smart effect injectors (`inject`, `fanout`, `assocL`, `distributeL`).
- `Penelope.Core.Wiring`: Frame rule combinators and disjunctive splitters.
- `Penelope.Core.Engine`: Two-phase elaboration and vector interpretation engine.
- `Penelope.Perceus`: Perceus liveness analysis and in-place memory allocation planner.
- `Penelope.Backend.Plugin`: Extensible backend compiler interface for GIMPLE, MLIR, and native backends.

---

## License

Dual-licensed under either:
- **European Union Public Licence (EUPL-1.2)**
- **Apache License, Version 2.0 (Apache-2.0)**

See [LICENSE](LICENSE) for full legal text.
