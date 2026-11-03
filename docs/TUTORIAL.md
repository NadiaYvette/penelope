<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# Interactive Tutorial: Practical Polycategorical Programming with Penelope

**Status:** LIVING TUTORIAL · v1.0 · 2026-11-03  
**Audience:** Haskell Programmers, Effect System Authors, Systems Architects, AI Coding Assistants  
**Prerequisites:** Familiarity with basic Haskell syntax, typeclasses, and monadic `do`-notation. No advanced type-level programming experience required.  
**Related Guides:** [`docs/NEWBIE_GUIDE.md`](NEWBIE_GUIDE.md) · [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md) · [`docs/notes/PENELOPE_DESIGN.md`](notes/PENELOPE_DESIGN.md) · [`docs/INDEX.md`](INDEX.md)

---

## Table of Contents

1. [Foundational Mental Model & Setup](#1-foundational-mental-model-&-setup)
2. [The Heftia Track: Unary Monadic Effects (`Eff`)](#2-the-heftia-track-unary-monadic-effects-eff)
   - [Monadic Effect Injection & Handlers](#monadic-effect-injection-&-handlers)
   - [Higher-Order Scoped Effects: Reader & Local](#higherorder-scoped-effects-reader-&-local)
   - [Lexically Scoped Exceptions: Throw & Catch](#lexically-scoped-exceptions-throw-&-catch)
   - [The Lexical Scoping Invariant](#the-lexical-scoping-invariant)
3. [The Kernmantle Track: Binary Arrow Rope Pipelines (`Rope`)](#3-the-kernmantle-track-binary-arrow-rope-pipelines-rope)
   - [Building Profunctor Arrow Strands](#building-profunctor-arrow-strands)
   - [Category Composition & Splitting](#category-composition-&-splitting)
   - [Disjunctive Branching with ArrowChoice](#disjunctive-branching-with-arrowchoice)
   - [Pre-Execution Jump-Table Routing (`deriveRouter`)](#preexecution-jumptable-routing-deriverouter)
4. [Penelope's Unique Polycategorical Superpowers ($(m, n, k, r)$)](#4-penelopes-unique-polycategorical-superpowers-$m-n-k-r$)
   - [Superpower 1: Multi-Port Conjunctive Register Wiring](#superpower-1-multiport-conjunctive-register-wiring)
   - [Superpower 2: Multi-Exit Disjunctive Control Flow (`Switch`)](#superpower-2-multiexit-disjunctive-control-flow-switch)
   - [Superpower 3: Multi-Region Scoped Control (`While` & `Saga`)](#superpower-3-multiregion-scoped-control-while-&-saga)
   - [Superpower 4: Compile-Time Static Routing & Peephole Fusion](#superpower-4-compiletime-static-routing-&-peephole-fusion)
   - [Superpower 5: Frame-Rule Automatic Register Wiring (`>&>`)](#superpower-5-framerule-automatic-register-wiring->&>)
   - [Superpower 6: Generalized Cayley Static Reflection (`penelope-static`)](#superpower-6-generalized-cayley-static-reflection-penelopestatic)
   - [Superpower 7: Partial Interpretation & Circuit Residualization](#superpower-7-partial-interpretation-&-circuit-residualization)
5. [Capstone Project: Resilient Multi-Stage Order Processing Engine](#5-capstone-project-resilient-multistage-order-processing-engine)
6. [Summary & Next Steps](#6-summary-&-next-steps)

---

## 1. Foundational Mental Model & Setup

In modern Haskell, developers typically choose between two contrasting effect styles:
* **Unary Monads (`m a`):** Expressive sequential `do`-notation, but limited to a single output value, zero named inputs, and opaque closure representations that defy pre-execution static analysis.
* **Binary Arrows (`p a b`):** Excellent for stream graphs and pre-execution capability reflection, but painful when passing multiple arguments, resulting in the "Arrow Plumbing Black Hole" of nested tuples `(a, (b, (c, d)))`.

`penelope` solves this dilemma by generalizing both paradigms to an **$(m, n, k, r)$-ary polycategorical structure**:

$$\operatorname{Penelope} \; (eh :: \operatorname{Row} \operatorname{PolyHEffect}) \; (ef :: \operatorname{Row} \operatorname{PolyEffect}) \; (ins :: \operatorname{Row} \operatorname{Type}) \; (outs :: \operatorname{Row} \operatorname{Type})$$

Every computational node processes:
* **$m$ named inputs** (`ins`): Flat Polytropos `Record` frame.
* **$n$ named outputs** (`outs`): Flat Polytropos `Record` frame.
* **$k$ disjunctive exits** (`exits`): Labeled alternative branches with explicit continuations.
* **$r$ scoped regions** (`regions`): Nested sub-pipelines with distinct port signatures.

### Toolchain Setup

To use Penelope in your project, include `penelope-core`, `penelope-effects`, `polytropos-core`, and `polytropos-plugin` in your `.cabal` file, and activate the compiler plugin:

```cabal
build-depends:
    base >= 4.14 && < 5
  , penelope-core >= 0.1
  , penelope-effects >= 0.1
  , polytropos-core >= 0.1
  , polytropos-plugin >= 0.1

ghc-options: -fplugin=Polytropos.Plugin
```

In your Haskell modules, import the primary front-door modules:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

import Polytropos
import Penelope.Core
import Penelope.Effects
```

---

## 2. The Heftia Track: Unary Monadic Effects (`Eff`)

If your application benefits from familiar monadic `do`-notation, the `Penelope.Effects.Unary` module provides **`Eff eh ef a`**—a zero-cost unary projection over Penelope's polycategorical engine.

### Monadic Effect Injection & Handlers

Let's define a stateful counter program using standard first-order effects:

```haskell
import Penelope.Effects.Unary

-- Define an effect signature for a Key-Value Store
data KV (ins :: Row Type) (exits :: Row (Row Type)) where
  GetK :: String -> KV '[] '[ "res" ':= '[ "val" ':= Maybe String ] ]
  SetK :: String -> String -> KV '[] '[ "res" ':= '[] ]

-- Write an intuitive monadic business logic procedure
program :: Eff '[] '[ "kv" ':= KV ] (Maybe String)
program = do
  sendEff @"kv" (SetK "user:101" "Alice")
  res <- sendEff @"kv" (GetK "user:101")
  pure (getField @"val" res)
```

Notice that effect injection (`sendEff @"kv"`) is parameterized by the symbolic label of the effect in the effect row `ef`. The `polytropos-plugin` resolves `ConstructorIndex @"kv" ef` at compile time to an unboxed integer literal (`Word#`), completely bypassing type-level list searches.

### Higher-Order Scoped Effects: Reader & Local

One of Heftia's breakthrough insights was the clean separation of higher-order scoped effects from first-order algebraic operations. In Penelope, scoped effects live in the `eh` row.

The `Penelope.Effects.Scoped` module provides lexically scoped Reader and Exception handling:

```haskell
import Penelope.Effects.Scoped

type Env = String

-- A computation that reads environment and temporarily modifies it locally
readerWorkflow :: Eff '[ "local" ':= Local Env ] '[ "ask" ':= AskEff Env ] String
readerWorkflow = do
  baseEnv <- ask @Env
  modifiedEnv <- local @Env (\e -> e ++ "/submodule") $ do
    nested <- ask @Env
    pure ("Inside: " ++ nested)
  pure ("Base: " ++ baseEnv ++ " | " ++ modifiedEnv)
```

### Lexically Scoped Exceptions: Throw & Catch

Similarly, scoped error handling is expressed via `ThrowEff` (first-order in `ef`) and `Catch` (higher-order in `eh`):

```haskell
safeDivision :: Int -> Int -> Eff '[ "catch" ':= Catch String ] '[ "throw" ':= ThrowEff String ] Int
safeDivision x y = catchError @String
  (if y == 0
     then throwError @String "Division by zero!"
     else pure (x `div` y))
  (\err -> pure (-1))
```

### The Lexical Scoping Invariant

In standard Haskell effect libraries (e.g., `mtl` or naive Freer monads), interleaving `local` or `catch` with multi-shot delimited continuations (such as logic programming backtracking or coroutines) can leak mutated environments or clobber exception handlers.

**Penelope's Guarantee:** Because Penelope enforces a two-phase architecture:
1. All scoped regions in `eh` are elaborated into pure first-order control flow during Phase 1 (`elaborateAll`).
2. Interpretation occurs over pure first-order vectors in Phase 2 (`interpretAll`).

As proven in our test suite ([`packages/penelope-effects/test/Main.hs`](../packages/penelope-effects/test/Main.hs)), environments and exception frames **never leak** across multi-shot resumptions.

---

## 3. The Kernmantle Track: Binary Arrow Rope Pipelines (`Rope`)

For stream graphs, data science pipelines, or compiler passes where computations have an explicit input and output, `Penelope.Effects.Binary` provides **`Rope eh ef a b`**.

### Building Profunctor Arrow Strands

A `Rope` is an instance of `Control.Category`, `Control.Arrow`, `Control.ArrowChoice`, and `Data.Profunctor.Profunctor`:

```haskell
import Control.Category (Category(..), (>>>))
import Control.Arrow (Arrow(..), ArrowChoice(..))
import Penelope.Effects.Binary

-- A pipeline step that reads a raw CSV line, parses fields, and validates age
parseLine :: Rope '[] '[] String (String, Int)
parseLine = arr $ \s ->
  let [name, ageStr] = words s
  in (name, read ageStr)

validateAge :: Rope '[] '[] (String, Int) (Either String String)
validateAge = arr $ \(name, age) ->
  if age >= 18
    then Right (name ++ " is an adult")
    else Left (name ++ " is underage")
```

### Category Composition & Splitting

Using standard arrow combinators, pipelines compose cleanly with zero syntax noise:

```haskell
-- Compose sequentially using (>>>)
auditPipeline :: Rope '[] '[] String (Either String String)
auditPipeline = parseLine >>> validateAge

-- Process dual streams in parallel using (***)
parallelAudit :: Rope '[] '[] (String, String) (Either String String, Either String String)
parallelAudit = auditPipeline *** auditPipeline
```

### Disjunctive Branching with ArrowChoice

`Rope` natively implements `ArrowChoice`, allowing conditional branching without losing static pipeline visibility:

```haskell
processAuditResult :: Rope '[] '[] (Either String String) String
processAuditResult =
  arr (\err -> "AUDIT FAILED: " ++ err)
  |||
  arr (\ok  -> "AUDIT PASSED: " ++ ok)

fullPipeline :: Rope '[] '[] String String
fullPipeline = auditPipeline >>> processAuditResult
```

### Pre-Execution Jump-Table Routing (`deriveRouter`)

Kernmantle's primary virtue is pre-execution capability inspection. In `penelope-effects`, `deriveRouter` compiles a collection of effect strands into a contiguous unboxed jump table:

```haskell
-- Run a pure rope pipeline instantaneously:
result :: String
result = runRopePure fullPipeline "Bob 25"
-- result == "AUDIT PASSED: Bob is an adult"
```

---

## 4. Penelope's Unique Polycategorical Superpowers ($(m, n, k, r)$)

While Penelope flawlessly executes Heftia-style unary monads and Kernmantle-style binary arrows, its true power lies in its native **$(m, n, k, r)$-ary polycategorical substrate**.

### Superpower 1: Multi-Port Conjunctive Register Wiring

In traditional arrows, passing 5 variables to a downstream stage produces nested tuple towers:
```haskell
-- The Arrow Plumbing Hell:
arr (\(req, (auth, (db, (cache, log)))) -> ...)
```
In Penelope, arguments are named ports in an unboxed Polytropos `Record` frame:

```haskell
type OrderFrame =
  '[ "orderId"  ':= Int
   , "customer" ':= String
   , "total"    ':= Double
   , "authTok"  ':= String
   ]

-- Pure stage consuming 2 ports and producing 1 port:
applyDiscount :: Penelope '[] '[] '[ "total" ':= Double ] '[ "discounted" ':= Double ]
applyDiscount = liftPure $ \frame ->
  let tot = getField @"total" frame
  in emptyRecord & insertField @"discounted" (tot * 0.90)
```

No tuple unpacking is performed. The compiler plugin directly indexes fields at unboxed byte offsets in a contiguous array.

### Superpower 2: Multi-Exit Disjunctive Control Flow (`Switch`)

Handling multiple exit branches in standard Haskell requires nested `Either` ladders:
```haskell
-- The Monadic Error Ladder:
Either OutOfStock (Either CardDeclined (Either RateLimited Success))
```
In Penelope, `Switch` defines $k$ distinct, named exit continuations:

```haskell
import Penelope.Effects.Control

type ExitBranches =
  '[ "ok"          ':= '[ "receipt" ':= String ]
   , "cardDeclined":= '[ "reason"  ':= String ]
   , "outOfStock"  ':= '[ "sku"     ':= String ]
   ]

-- An effect operation that exits to one of three labeled branches:
chargeCustomer
  :: Penelope '[] '[ "switch" ':= Switch ExitBranches ] '[ "amount" ':= Double ] '[]
chargeCustomer = ...
```

Each continuation branch receives only the ports relevant to its outcome.

### Superpower 3: Multi-Region Scoped Control (`While` & `Saga`)

Penelope supports operations enclosing multiple distinct sub-pipeline regions ($r$).

#### The Multi-Region `While` Effect
A while-loop has two distinct sub-regions: a `"cond"` predicate region and a `"body"` execution region:

```haskell
type WhileRegions =
  '[ "cond" ':= 'PortSig '[ "i" ':= Int ] '[ "keepGoing" ':= Bool ]
   , "body" ':= 'PortSig '[ "i" ':= Int ] '[ "i" ':= Int ]
   ]

-- Native While loop executing until condition fails:
countToTen :: Penelope '[ "while" ':= While WhileRegions ] '[] '[ "i" ':= Int ] '[ "i" ':= Int ]
countToTen = ...
```

#### The Transactional `Saga` Effect
A distributed Saga orchestrates a forward transaction region and a backward compensation region:

```haskell
type SagaRegions =
  '[ "tx"   ':= 'PortSig '[ "order" ':= Int ] '[ "txId" ':= String ]
   , "comp" ':= 'PortSig '[ "txId" ':= String ] '[]
   ]
```

If an error occurs downstream, Penelope automatically executes the compensation region without manual error tracking.

### Superpower 4: Compile-Time Static Routing & Peephole Fusion

When rearranging data wires, Penelope generates `Route (morph :: RowMorph outs ins)`.

```haskell
-- Wire swap: swap "a" and "b"
swapStage :: Penelope '[] '[] '[ "a" ':= Int, "b" ':= String ] '[ "b" ':= String, "a" ':= Int ]
swapStage = swapProd @"a" @"b"

-- Two sequential swaps:
doubleSwap :: Penelope '[] '[] '[ "a" ':= Int, "b" ':= String ] '[ "a" ':= Int, "b" ':= String ]
doubleSwap = swapStage >>> swapStage
```

**The Peephole Fusion Invariant:**  
`swapStage >>> swapStage` does **not** create two nested AST nodes! The sequential composition constructor immediately computes:
$$\text{composeRowMorph } m_1 \text{ } m_2 \to \text{IdentityMorph}$$
The entire double-swap collapses into an instantaneous identity route at compile time.

### Superpower 5: Frame-Rule Automatic Register Wiring (`>&>`)

The `(>&>)` operator connects a subprogram to an ambient register frame:

```haskell
-- Ambient Frame containing 4 registers:
type Ambient = '[ "x" ':= Int, "y" ':= Int, "user" ':= String, "token" ':= String ]

-- Sub-operation that only cares about x and y:
addXY :: Penelope '[] '[] '[ "x" ':= Int, "y" ':= Int ] '[ "sum" ':= Int ]
addXY = liftPure $ \f ->
  let x = getField @"x" f
      y = getField @"y" f
  in emptyRecord & insertField @"sum" (x + y)

-- Wire addXY into the ambient frame automatically:
wiredPipeline :: Penelope '[] '[] Ambient '[ "sum" ':= Int, "user" ':= String, "token" ':= String ]
wiredPipeline = addXY >&> idRoute
```

`FrameWire` automatically pulls `"x"` and `"y"`, executes `addXY`, scatters `"sum"` into the output frame, and preserves `"user"` and `"token"` untouched!

### Superpower 6: Generalized Cayley Static Reflection (`penelope-static`)

Because Penelope's syntax tree does not hide operations inside opaque `(->)` lambdas, `penelope-static` analyzes pipelines prior to execution:

```haskell
import Penelope.Static
import Penelope.Static.Analysis

-- Statically analyze capability strands and register liveness:
auditPipeline :: Penelope eh ef ins outs -> PipelineReport
auditPipeline pipe =
  let staticModel = analyzeHypergraph pipe
      strands     = extractCapabilityStrands staticModel
      liveness    = calculatePortLiveness staticModel
  in PipelineReport strands liveness
```

* **Zero-Leak Capability Auditing:** Verifies that untrusted plugins do not request forbidden capabilities (e.g., `"networkSend"` inside an isolated calculation).
* **Per-Port Register Liveness:** Statically identifies the exact step where an output register is read for the last time, enabling the runtime engine to overwrite that slot immediately without garbage collector churn.

### Superpower 7: Partial Interpretation & Circuit Residualization

Penelope can partially interpret static configuration or authorization effects at system startup, leaving dynamic database and network effects intact as a residualized polycategorical circuit:

```haskell
import Penelope.Core.Engine.Partial

-- Partially evaluate static configuration:
residualCircuit :: Penelope '[] '[ "db" ':= DBEffect ] InPorts OutPorts
residualCircuit = residualizeStatic @'[ "cfg" ':= ConfigEff ] fullPipeline configHandler
```

The resulting `residualCircuit` executes with zero configuration lookup overhead during request handling.

---

## 5. Capstone Project: Resilient Multi-Stage Order Processing Engine

Let's combine all of Penelope's capabilities into an end-to-end e-commerce order processing pipeline.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module OrderProcessing where

import Data.Functor.Identity (Identity (..))
import Penelope.Core
import Penelope.Effects
import Polytropos

-- 1. Define Input and Output Port Registers
type OrderInput =
  '[ "orderId"   ':= Int
   , "customerId":= String
   , "sku"       ':= String
   , "quantity"  ':= Int
   , "unitPrice" ':= Double
   ]

type OrderOutput =
  '[ "orderId"   ':= Int
   , "total"     ':= Double
   , "status"    ':= String
   ]

-- 2. Define the Pipeline
orderPipeline :: Penelope '[] '[] OrderInput OrderOutput
orderPipeline =
  -- Step A: Compute raw total price
  liftPure @'[ "quantity" ':= Int, "unitPrice" ':= Double ] @'[ "rawTotal" ':= Double ] (\f ->
    let q = getField @"quantity" f
        p = getField @"unitPrice" f
    in emptyRecord & insertField @"rawTotal" (fromIntegral q * p)
  )
  -- Step B: Apply volume discount if quantity > 5
  >&> liftPure @'[ "rawTotal" ':= Double, "quantity" ':= Int ] @'[ "total" ':= Double ] (\f ->
    let tot = getField @"rawTotal" f
        qty = getField @"quantity" f
        finalTot = if qty > 5 then tot * 0.90 else tot
    in emptyRecord & insertField @"total" finalTot
  )
  -- Step C: Format confirmation status
  >&> liftPure @'[ "orderId" ':= Int, "total" ':= Double ] @OrderOutput (\f ->
    let oid = getField @"orderId" f
        tot = getField @"total" f
    in emptyRecord
         & insertField @"orderId" oid
         & insertField @"total"   tot
         & insertField @"status"  "CONFIRMED"
  )

-- 3. Execute the Pipeline Purely
processOrder :: Int -> String -> String -> Int -> Double -> OrderOutput
processOrder oid cust sku qty price =
  let inFrame = emptyRecord
        & insertField @"orderId"    oid
        & insertField @"customerId" cust
        & insertField @"sku"        sku
        & insertField @"quantity"   qty
        & insertField @"unitPrice"  price
  in runPenelopePure orderPipeline inFrame
```

### Execution Output

```haskell
>>> processOrder 4001 "Alice" "WIDGET-A" 10 25.0
-- Output Frame:
-- orderId = 4001
-- total   = 225.0  (10 * 25.0 = 250.0, minus 10% volume discount)
-- status  = "CONFIRMED"
```

---

## 6. Summary & Next Steps

Penelope collapses the historic dichotomy between unary monadic effects and binary profunctor arrow ropes. Whether you are building high-throughput microservices, verifiable compiler pipelines, or complex distributed sagas, Penelope provides:
* **Zero-Cost Unary (`Eff`) & Binary (`Rope`) Facades.**
* **Flat Memory Physics:** Unpacked tags and payloads on the hot path.
* **$O(1)$ Jump-Table Dispatch:** Branchless execution powered by `polytropos-plugin`.
* **Zero-Traversal Weakening:** Institutional satisfaction without AST walking.
* **Pre-Execution Static Reflection:** Cayley hypergraph analysis for capability and liveness auditing.

### Where to Go Next

* **New to Penelope?** Read [`docs/NEWBIE_GUIDE.md`](NEWBIE_GUIDE.md) for a token-saving orientation funnel and mental model breakdown.
* **Architectural Decisions:** Explore [`docs/notes/PENELOPE_DESIGN.md`](notes/PENELOPE_DESIGN.md) for deep dives into decisions D1–D10.
* **Corpus Map:** Check [`docs/INDEX.md`](INDEX.md) for the full reading map.
* **Maintenance & Verification:** See [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md) for instructions on running verification gates and benchmarks.
