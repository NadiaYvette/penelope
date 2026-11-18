# penelope-static

**Static Reflection and Cayley Analysis for the Penelope Effect Engine.**

`penelope-static` provides static analysis, reflection, and formal algebraic verification tools for the Penelope polycategorical effect framework. It allows developers and compiler backends to inspect effect dependency hypergraphs, optimize circuit wiring, and formally verify institutional satisfaction properties without executing runtime computations.

---

## Architectural Overview

Static analysis in Penelope is grounded in **Cayley representations** and **institution theory**:

```
[Penelope Polycategorical AST]
               │
               ▼
   [Cayley Representation]
               │
               ▼
[Dependency Hypergraph Extraction] ──> [Circuit Simplification / Dead Code]
               │
               ▼
[Institutional Satisfaction Verification]
```

### Key Technical Characteristics

1. **Cayley Representation:**  
   Maps polycategorical nodes to endomorphisms over computational paths, enabling algebraic simplification, fusion of adjacent wiring morphisms, and dead-effect elimination.

2. **Static Effect Dependency Extraction:**  
   Extracts directed acyclic hypergraphs showing which effects depend on outputs of preceding nodes, identifying opportunities for parallelization across independent effect branches.

3. **Institutional Satisfaction Properties:**  
   Formally verifies that Penelope pipelines satisfy the Goguen-Burstall institutional satisfaction condition:

   $$\mathcal{M}' \models_{\Sigma'} \sigma(\phi) \iff \sigma^*(\mathcal{M}') \models_{\Sigma} \phi$$

   Ensuring that effect weakening (`weakenPoly`), signature translation, and modular composition preserve semantic validity across module boundaries.

---

## Module Hierarchy

- `Penelope.Static`: High-level static analysis API and re-exports.
- `Penelope.Static.Analysis`: Dependency graph extraction, topological scheduling, and satisfaction verification algorithms.

---

## Example: Static Dependency Inspection

```haskell
{-# LANGUAGE DataKinds, TypeApplications #-}
import Penelope.Core
import Penelope.Static.Analysis

-- Extract the static dependency graph of a polycategorical term:
inspectCircuit :: Penelope '[] ef ins outs -> DependencyGraph
inspectCircuit term = extractDependencyGraph term
```

---

## License

Dual-licensed under either:
- **European Union Public Licence (EUPL-1.2)**
- **Apache License, Version 2.0 (Apache-2.0)**

See [LICENSE](LICENSE) for full legal text.
