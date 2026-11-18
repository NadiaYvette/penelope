# penelope-effects

**Effect Battery, Heftia and Kernmantle Facades for Penelope.**

`penelope-effects` provides high-level ergonomic facades and a comprehensive standard battery of algebraic effects built upon the Penelope polycategorical effect engine. It seamlessly bridges the gap between monadic algebraic effect programming ([Heftia](https://hackage.haskell.org/package/heftia)) and binary profunctor arrow pipelines ([Kernmantle](https://hackage.haskell.org/package/kernmantle)).

---

## Dual Programming Facades

`penelope-effects` offers two complementary programming abstractions over the underlying $(m,n,k,r)$-ary polycategorical core:

### 1. The Monadic Facade (`Eff`)

For standard sequential workflows, `Eff` provides a conventional monadic interface with automatic higher-order effect elaboration:

```haskell
import Penelope.Effects
import Penelope.Effects.Unary

program :: Eff '["state" ':= State Int, "error" ':= Error Text] () Int
program = do
  s <- get @"state"
  if s < 0
    then throwError @"error" "Negative state encountered"
    else do
      put @"state" (s + 1)
      pure (s * 2)
```

### 2. The Profunctor Facade (`Flow`)

For stream processing, data pipelines, and hardware-near register routing, `Flow` offers a category-theoretic profunctor interface:

```haskell
import Penelope.Effects
import Penelope.Effects.Binary

pipeline :: Flow '["timer" ':= Timer] '["in" ':= Double] '["out" ':= Double]
pipeline =
  readSensor
    >>> scaleFactor 1.5
    >>> emitTelemetry
```

---

## Standard Effect Battery

The package ships with fully vectorized, zero-overhead handlers for standard effects:

| Module | Effect | Description |
|---|---|---|
| `Penelope.Effects.Unary` | `State`, `Reader`, `Writer` | Basic stateful, contextual, and accumulative computation. |
| `Penelope.Effects.Control` | `Error`, `NonDet`, `Fail` | Delimited control flow, failure, and backtracking. |
| `Penelope.Effects.Coroutine` | `Yield`, `Await` | Bidirectional coroutine generators and streaming transducers. |
| `Penelope.Effects.UnliftIO` | `UnliftIO` | Safe unlifting of arbitrary `IO` bracket operations. |
| `Penelope.Effects.Scoped` | `Scoped` | Dynamic resource brackets and lifetime management. |
| `Penelope.Effects.Concurrent` | `Async`, `Fork`, `MVar` | Structured concurrency primitives and synchronization. |
| `Penelope.Effects.Interpreter` | `StepBudget`, `FreshSym`, `TraceStep` | Convenience interpreter effects for compiler and interpreter stages. |

---

## Convenience Interpreter Effects (`Penelope.Effects.Interpreter`)

- **`StepBudget`:** Guarantees termination and bounds execution cycles in untrusted or recursive code.
- **`FreshSym`:** Generates unique hygienic symbols for AST elaboration and compiler backends.
- **`MultiExitTrampoline`:** Traps and redirects multi-branch non-local exits.
- **`TraceStep`:** Structured diagnostic tracing with minimal performance overhead.

---

## License

Dual-licensed under either:
- **European Union Public Licence (EUPL-1.2)**
- **Apache License, Version 2.0 (Apache-2.0)**

See [LICENSE](LICENSE) for full legal text.
