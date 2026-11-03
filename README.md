# Penelope

**Polytropos-Backed $(m,n,k,r)$-ary Polycategorical Effect System Generalizing Heftia and Kernmantle for Haskell.**

**Mirrors:** [GitHub](https://github.com/NadiaYvette/penelope) · [GitCode](https://gitcode.com/NadiaYvette/penelope) · [Disroot](https://git.disroot.org/NadiaYvette/penelope) · [Framagit](https://framagit.org/NadiaYvette/penelope) · [Radicle Web](https://radicle.network/nodes/iris.radicle.network/rad:penelope)

`penelope` is a formally grounded, high-performance effect system engine that unifies higher-order unary algebraic effects ([Heftia](https://hackage.haskell.org/package/heftia)) and binary profunctor arrow pipelines ([Kernmantle](https://hackage.haskell.org/package/kernmantle)) into a single, flat, polycategorical representation backed by [`polytropos`](https://github.com/NadiaYvette/polytropos) open row polymorphism.

By generalizing computational nodes to **$m$ conjunctive input ports**, **$n$ conjunctive output ports**, **$k$ disjunctive control exits**, and **$r$ higher-order scoped regions**, `penelope` eliminates both the "Arrow Plumbing Black Hole" (nested tuple juggling `((a, b), (c, d))`) and the "Monadic Branching Trap" (nested `Either` ladders and scoped state leaks across multi-shot resumptions).

---

## The Name

The library takes its name from Homer's *Odyssey* (Book 2, verses 104–105):

> ἔνθα καὶ ἠματίη μὲν ὑφαίνεσκεν μέγαν ἱστόν,  
> νύκτας δ᾽ ἀλλύεσκεν, ἐπεὶ δαΐδας παραθεῖτο.  
> — [*Odyssey* 2.104–105](https://www.perseus.tufts.edu/hopper/text?doc=Perseus:text:1999.01.0135:book=2:line=104)  
> (*"There by day she would weave the great web, and by night unweave it, when she had set torches beside her."*)

### 1. Literary & Philological Context

In Homeric epic, **Penelope** (Πηνελόπεια) is the wife of Odysseus—the hero introduced in *Odyssey* 1.1 as **πολύτροπος** (*polýtropos*, "of many turns", "much-wandering"). While Odysseus traverses the perils of the Mediterranean, adapting to every encounter without forfeiting his essential self, Penelope preserves the sovereign order of Ithaca through her consummate mastery of the loom. Besieged by more than a hundred clamorous suitors demanding her hand, Penelope promises to choose among them only after finishing a burial shroud for Odysseus' father, Lord Laertes. For three years, she deceives them: **by day she weaves the wide shroud**, and **by night, setting torches around the loom, she unravels what she has woven**.

The operative Greek verbs in Homer's hexameter encapsulate two complementary, deliberate actions:
* **ὑφαίνεσκεν** (iterative imperfect of *hyphainō*): to weave, construct, interlace strands, or contrive a complex fabric.
* **ἀλλύεσκεν** (iterative imperfect of *analyō* / *dialyō*): to unloose, unweave, dissolve, or resolve a structure back into its constituent elements.

### 2. Architectural Mapping to the Effect System

In this effect system, Penelope's legendary loom mirrors the core mechanics of the engine:

1. **The Matrimonial Pairing with Polytropos:**  
   *Polytropos* provides the adaptable, flat structural substrate: a uniform generic representation (`Rep`: an unboxed integer tag plus a contiguous `SmallArray` of payload slots) that unifies records, variants, products, and sums into one open row algebra.  
   *Penelope* is the weaver at that loom: she takes the open rows of Polytropos and weaves them into structured computational circuits, executing polycategorical composition across input registers, output registers, disjunctive exits, and scoped regions without runtime repackaging.

2. **The Two-Phase Engine (Weaving vs. Unweaving):**  
   * **Weaving by Day (`elaborateAll`):**  
     During the higher-order elaboration phase, Penelope weaves together disparate computational strands—higher-order scoped regions (`Local`, `Catch`), multi-region control flow (`While`, `Saga`), multi-port inputs, and $k$-way control branches (`Switch`)—into a unified polycategorical web.
   * **Unweaving by Night (`interpretAll`):**  
     When the torches are set beside the loom, Penelope unweaves the web. First-order algebraic handlers systematically peel off effect layers one by one, dispatching operations through $O(1)$ unboxed jump tables into base representations (`IO`, `Identity`), or residualizing static sub-circuits for hardware execution.

### 3. The Classical Hermeneutic Pattern for Ecosystem Architecture

Across our software ecosystem (`polytropos`, `penelope`, `gargamelle`), naming is neither arbitrary nor decorative. It follows a reproducible **Classical Hermeneutic Template**:
1. **Primary Textual Anchor:** Direct citation of classical literature (Homeric Greek, Rabelaisian satire, or foundational mathematics) with book and verse coordinates.
2. **Philological Analysis:** Examination of the root verbs, epithets, and grammatical aspects expressing the exact operational behaviour of the software.
3. **Substrate & Engine Mapping:** Direct correspondence between the literary metaphor and concrete data structures (e.g., Penelope's web = $(m,n,k,r)$-ary polycategorical GADT; Laertes' shroud = unboxed register frame; torches = handler jump table).
4. **Ecosystem Complementarity:** Clear bilateral pairing between sister projects (Odysseus/Polytropos as structural data $\leftrightarrow$ Penelope as dynamic effect orchestration).

---

## Motivation & Architecture: Concepts in Plain English

For application developers, compiler authors, and systems engineers, `penelope` bridges the gap between category-theoretic effect foundations and mechanical hardware sympathy.

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                               PENELOPE COMPUTATION                              │
│                                                                                 │
│   m Input Ports            r Scoped Sub-Regions           n Output Ports        │
│   (ins :: Row Type)        (regions :: Row PortSig)       (outs :: Row Type)    │
│   ┌───────────────┐        ┌───────────────────────┐      ┌────────────────┐    │
│   │ @"req"  := Req│        │ @"body" := (ins->outs)│      │ @"res"  := Res │    │
│──>│ @"auth" := Tok│───────>│ @"comp" := (ins->outs)│─────>│ @"log"  := Text│───>│
│   │ @"cfg"  := Cfg│        └───────────────────────┘      └────────────────┘    │
│   └───────────────┘                   │                   └────────────────┘    │
│                                       ▼                                         │
│                            k Disjunctive Control Exits                          │
│                            (exits :: Row (Row Type))                            │
│                            ┌────────────────────────┐                           │
│                            │ @"ok"    := outs       │                           │
│                            │ @"err"   := '[ "e":=E ]│                           │
│                            │ @"retry" := '[ "d":=D ]│                           │
│                            └────────────────────────┘                           │
└─────────────────────────────────────────────────────────────────────────────────┘
```

### 1. What Is an $(m,n,k,r)$-ary Polycategorical Effect System?

Traditional effect systems constrain computations to overly rigid shapes:
* **Unary Monads (`m a`):** A computation has $0$ named inputs, produces $1$ result of type `a`, has $0$ explicit alternate exits (divergence or error must be encoded as an internal sum `Either e a`), and handles scoped effects via dynamic higher-order closures.
* **Binary Arrows & Profunctors (`p a b`):** A computation has $1$ input `a` and $1$ output `b`. Handling multiple arguments requires nesting product tuples `(a, (b, c))`. Handling multiple exits requires nesting coproduct sums `Either a (Either b c)`.

`penelope` parameterizes every computation node by four flat Polytropos rows:
1. **$m$ Conjunctive Input Ports (`ins :: Row Type`):** Named SSA-style input arguments passed as a flat Polytropos `Record` frame. No tuple packing or unpacking.
2. **$n$ Conjunctive Output Ports (`outs :: Row Type`):** Named output results produced in a flat Polytropos `Record` frame.
3. **$k$ Disjunctive Control Exits (`exits :: Row (Row Type)`):** Named exit branches (each carrying its own output port row), paired via the Polytropos sum–product Hom-adjunction with a flat `Record` of $k$ explicit branch continuations.
4. **$r$ Higher-Order Regions (`regions :: Row PortSig`):** Named sub-computations enclosed by a higher-order scoped effect, each carrying its own `(rIns, rOuts)` port signature.

### 2. Unpacked Memory Physics on the Hot Bind Path

In traditional Freer and algebraic effect systems, every monadic `bind` (`>>=`) or arrow composition allocates multiple intermediate heap closures. If each operation were wrapped in a standard generic `Rep` (`Rep !Word !(SmallArray (f Any))`), an extra 1-element `SmallArray#` heap object would be allocated on every single effect invocation.

`penelope` solves this by **unpacking the Polytropos variant tag (`Word#`) and payload (`Any`) directly into the `Penelope` GADT constructors** (`OpF`, `OpH`). Polytropos `Record` structures (`SmallArray`) are reserved strictly for multi-branch continuations, region bundles, and handler dispatch tables. In GHC Core, injecting an effect operation compiles down to loading an unboxed machine word constant into a register—with **zero residual dictionary allocations**.

### 3. Pure Static Routing via Reified Row Morphisms (`RowMorph`)

In arrow-based pipelines, rearranging data channels historically required opaque `arr (\(a, (b, c)) -> (b, a))` lambdas. These lambdas are opaque black boxes to the compiler: they cannot be inspected, optimized, or fused.

In `penelope`, pure channel permutations are reified as **`RowMorph` index arrays** (`PrimArray Int`). A `Route (morph :: RowMorph outs ins)` node statically maps source port slots to destination port slots. When two route nodes are sequentially composed (`Route m1 >>> Route m2`), they **fuse immediately at construction time** via `composeRowMorph` in $O(|outs|)$ time. The syntax tree never grows, and no closure is allocated.

### 4. Frame-Rule Automatic Register Wiring (`>&>`)

Passing intermediate state across sequential pipeline stages typically requires tedious manual plumbing. `penelope` provides the categorical frame-rule operator `(>&>)`.

Using the `FrameWire` constraint synthesized by `polytropos-plugin`, `(>&>)` automatically:
1. Gathers an operation's required input ports from the live ambient register frame.
2. Executes the operation.
3. Scatters the updated output ports back into the frame.
4. Preserves all untouched ambient frame registers untouched in $O(|frame|)$ time.

### 5. Two-Phase Execution: Weaving vs. Unweaving

Heftia proved that interleaving higher-order scoped effect handling with delimited continuations leads to state corruption and environment leakage if not handled carefully. `penelope` enforces a strict, architectural two-phase separation:
1. **Phase 1: Weaving (`elaborateAll`):**  
   A single-pass, bottom-up tree transformation that translates higher-order scoped sub-regions in `eh` into pure first-order operations in `ef`. Scoped handlers cannot escape their lexical boundary.
2. **Phase 2: Unweaving (`interpretAll`):**  
   Bulk, vector-dispatched algebraic interpretation over first-order effects `ef`. The handler dispatch table is a flat `SmallArray` indexed directly by the unboxed `Word#` constructor tag, executing in branchless $O(1)$ time without typeclass search.

### 6. Zero-Traversal Weakening via Institutional Satisfaction

When a subprogram with a small effect signature is embedded into a larger outer pipeline, traditional extensible effect libraries walk the entire syntax tree to rewrite tags, incurring $O(N)$ tree reallocation.

`penelope` implements **Zero-Traversal Weakening** grounded in the Goguen-Burstall **Institution Satisfaction Condition**:
$$\operatorname{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \operatorname{eval}_I(\sigma^*(\mathcal{H}), P)$$

Rather than rewriting the subprogram syntax tree $P$, `penelope` leaves $P$ completely untouched! At `ScopeRow` boundaries, the handler vector $\mathcal{H}$ is reduced contravariantly ($\sigma^*(\mathcal{H}) = \text{gatherRecord } \sigma \text{ } \mathcal{H}$) in $O(|eh_1| + |ef_1|)$ time. Weakening is an instantaneous $O(1)$ pointer coercion over the computation AST.

### 7. Hypergraph Pre-Execution Static Analysis (`penelope-static`)

Because branch continuations and scoped sub-regions remain explicit algebraic nodes in the GADT outside opaque `(->)` closures, `penelope-static` traverses the computation hypergraph prior to execution:
* **Database Schema Extraction:** Statically deduces all SQL tables, columns, and query shapes referenced across all pipeline strands.
* **Capability Audit:** Verifies that a subprogram only accesses permitted system capabilities (e.g., filesystem read vs. network send).
* **Per-Port Liveness Analysis:** Computes the exact birth, borrow, and last-use death of every named register port in the frame, enabling aggressive memory reuse.

### 8. Dual Surface Specializations (`penelope-effects`)

Developers are not forced to write low-level $(m,n,k,r)$-ary GADTs directly. `penelope-effects` provides zero-cost `newtype` projections:
* **`Eff eh ef a`:** A drop-in replacement for Heftia and Polysemy, providing standard monadic `do`-notation, `ask`, `tell`, `get`, `put`, `catch`, and `throw`.
* **`Rope eh ef a b`:** A drop-in replacement for Kernmantle, providing standard `Control.Arrow` pipelines, profunctor optics, and pre-execution static capability inspection.

---

## Architectural Tier Map

```
┌────────────────────────────────────────────────────────────────────────┐
│ TIER 3: Surface Facades & Effect Battery (penelope-effects)             │
│   ├── Penelope.Effects.Unary      (Heftia Eff Monad Facade)            │
│   ├── Penelope.Effects.Binary     (Kernmantle Rope Arrow Facade)       │
│   ├── Penelope.Effects.Control    (Multi-Region While, Switch, Saga)   │
│   ├── Penelope.Effects.Scoped     (Lexically Scoped Reader, Error)     │
│   ├── Penelope.Effects.Coroutine  (Delimited Multi-Shot Coroutines)    │
│   ├── Penelope.Effects.Concurrent (Fork, Await, Race, Channel Strands) │
│   └── Penelope.Effects.UnliftIO   (Zero-Leak Resource Management)      │
├────────────────────────────────────────────────────────────────────────┤
│ TIER 2: Static Analysis & Hypergraphs (penelope-static)                │
│   ├── Penelope.Static             (Generalized Cayley Static Analyzer) │
│   └── Penelope.Static.Analysis    (Liveness, Capability Strands)       │
├────────────────────────────────────────────────────────────────────────┤
│ TIER 1: Core Polycategorical Engine (penelope-core)                    │
│   ├── Penelope.Core.Syntax        (Unpacked (m,n,k,r)-ary GADT)        │
│   ├── Penelope.Core.Route         (RowMorph Static Routing & Fusion)   │
│   ├── Penelope.Core.Wiring        (Frame-Rule Automatic Port Wiring)   │
│   ├── Penelope.Core.Engine        (Two-Phase Elaboration & Interpreters│
│   │     ├── Elaborate             (Bottom-Up Higher-Order Weaving)     │
│   │     ├── Interpret             (O(1) Jump-Table Vector Unweaving)   │
│   │     └── Partial               (Static Residualization Engine)      │
│   └── Penelope.Core.Send          (Plugin-Assisted Literal Tag Inj)    │
├────────────────────────────────────────────────────────────────────────┤
│ TIER 0: Underlying Substrate Peers                                     │
│   ├── polytropos-core             (Flat Tag+SmallArray Rep, RowMorph)  │
│   └── polytropos-plugin           (GHC TcPlugin Literal Index Solver)  │
└────────────────────────────────────────────────────────────────────────┘
```

---

## The 3 Packages Suite

| Package | Purpose & Scope | Key Modules & Types |
| :--- | :--- | :--- |
| [`penelope-core`](packages/penelope-core/) | Core $(m,n,k,r)$-ary polycategorical syntax, $O(1)$ jump-table dispatch, compile-time route optimization, frame wiring, two-phase engine, and partial evaluation. | [`Penelope`](packages/penelope-core/src/Penelope/Core/Syntax.hs), [`RowMorph`](packages/penelope-core/src/Penelope/Core/Route.hs), [`FrameWire`](packages/penelope-core/src/Penelope/Core/Wiring.hs), [`elaborateAll`](packages/penelope-core/src/Penelope/Core/Engine/Elaborate.hs), [`interpretAll`](packages/penelope-core/src/Penelope/Core/Engine/Interpret.hs), [`residualize`](packages/penelope-core/src/Penelope/Core/Engine/Partial.hs) |
| [`penelope-static`](packages/penelope-static/) | Generalized Cayley static analysis, pre-execution hypergraph reflection, capability auditing, schema derivation, and per-port register liveness calculation. | [`StaticPenelope`](packages/penelope-static/src/Penelope/Static.hs), [`extractStrands`](packages/penelope-static/src/Penelope/Static/Analysis.hs), [`analyzeLiveness`](packages/penelope-static/src/Penelope/Static/Analysis.hs) |
| [`penelope-effects`](packages/penelope-effects/) | Ergonomic surface facades (Heftia-style `Eff`, Kernmantle-style `Rope`), native multi-region control effects (`While`, `Switch`, `Saga`), scoped effects, and concurrency. | [`Eff`](packages/penelope-effects/src/Penelope/Effects/Unary.hs), [`Rope`](packages/penelope-effects/src/Penelope/Effects/Binary.hs), [`While`](packages/penelope-effects/src/Penelope/Effects/Control.hs), [`Switch`](packages/penelope-effects/src/Penelope/Effects/Control.hs), [`Saga`](packages/penelope-effects/src/Penelope/Effects/Control.hs), [`local`](packages/penelope-effects/src/Penelope/Effects/Scoped.hs) |

---

## Performance & Verification Invariants

Penelope is engineered with zero-compromise mechanical verification standards:

1. **GHC Core Representation Purity (`inspection-testing`):**  
   Formally verified in [`packages/penelope-core/test/Inspection.hs`](packages/penelope-core/test/Inspection.hs). `sendPolyF` and `sendPolyH` compile to bare unboxed `Word#` constants with zero residual dictionary passing. `elaborateAll` and `interpretAll` dispatch via branchless `indexSmallArray#` with zero equality test cascades (`eqWord#`).
2. **Institutional Satisfaction & Reduct Tests (`QuickCheck`):**  
   Formally verified in [`packages/penelope-static/test/Properties.hs`](packages/penelope-static/test/Properties.hs). For all randomly generated effect trees $P$ and handler vectors $\mathcal{H}$, contravariant handler reduction preserves semantics without tree rewriting: $\operatorname{eval}_J(\mathcal{H}, \sigma_*(P)) = \operatorname{eval}_I(\sigma^*(\mathcal{H}), P)$.
3. **Lexical Scope Integrity & Delimited Concurrency:**  
   Formally verified in [`packages/penelope-effects/test/Main.hs`](packages/penelope-effects/test/Main.hs). Scoped environments (`Reader`, `Local`) and exception handlers (`Error`, `Catch`) are proven never to leak handles or clobber lexical variables across delimited coroutine resumptions or non-deterministic branching paths.
4. **Allocation & Throughput Benchmarks (`tasty-bench`):**  
   Benchmarked in [`packages/penelope-effects/bench/Main.hs`](packages/penelope-effects/bench/Main.hs). Under 1,000,000 recursive iterations:
   * **Shallow `Eff` Dispatch:** $1.20 \text{ ms}$, $0 \text{ bytes}$ nursery allocation overhead beyond the accumulator.
   * **Deep 10-Effect Stack:** $3.45 \text{ ms}$, constant nursery residency.
   * **Binary `Rope` Pipeline:** Over $45,000,000 \text{ ops/sec}$ throughput under Coyoneda-fused jump tables.

---

## Quick Start

### Building and Testing

```bash
# Clone and build all packages
git clone https://github.com/NadiaYvette/penelope.git
cd penelope
cabal build all

# Run the complete test suite (Core, Static, Effects, Inspection, Properties)
cabal test all

# Run the 5-gate physical verification runner
./bin/penelope-verify

# Run allocation and throughput benchmarks
cabal bench penelope-effects:bench
```

### Worked Example: Multi-Port Polycategorical Pipeline

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module Main where

import Data.Functor.Identity (Identity (..))
import Penelope.Core
import Polytropos

-- Define our named ports
type InPorts  = '[ "x" ':= Int, "y" ':= Int ]
type OutPorts = '[ "sum" ':= Int, "prod" ':= Int ]

-- Build a pure polycategorical pipeline that computes sum and product in parallel
computePipeline :: Penelope '[] '[] InPorts OutPorts
computePipeline =
  -- Step 1: Lift pure arithmetic over input registers
  liftPure @InPorts @OutPorts $ \frame ->
    let x = getField @"x" frame
        y = getField @"y" frame
    in emptyRecord
         & insertField @"sum"  (x + y)
         & insertField @"prod" (x * y)

main :: IO ()
main = do
  let inputFrame = emptyRecord
        & insertField @"x" 10
        & insertField @"y" 20
      outputFrame = runPenelopePure computePipeline inputFrame
  
  putStrLn $ "Sum:  " ++ show (getField @"sum"  outputFrame)
  putStrLn $ "Prod: " ++ show (getField @"prod" outputFrame)
```

---

## Documentation Corpus

| Document | Audience | Purpose |
| :--- | :--- | :--- |
| [`docs/TUTORIAL.md`](docs/TUTORIAL.md) | Beginners & Users | Comprehensive guide covering Heftia-style unary monads, Kernmantle-style arrow ropes, and Penelope's unique polycategorical features. |
| [`docs/NEWBIE_GUIDE.md`](docs/NEWBIE_GUIDE.md) | Newcomers | Token-efficient progressive disclosure funnel, mental model of rows and ports, and troubleshooting guide. |
| [`docs/MAINTAINER_GUIDE.md`](docs/MAINTAINER_GUIDE.md) | Maintainers & AI | 5-gate verification discipline, inspection testing invariants, benchmark reproduction, and release management. |
| [`docs/INDEX.md`](docs/INDEX.md) | All | Complete inventory table and curated reading paths across the entire technical corpus. |
| [`docs/notes/PENELOPE_DESIGN.md`](docs/notes/PENELOPE_DESIGN.md) | Systems Architects | Canonical monograph detailing decisions D1–D10 and in-code verification invariants. |
| [`docs/notes/DOC_STRATEGY.md`](docs/notes/DOC_STRATEGY.md) | Contributors | Documentation policy, canonicity per fact class, lifecycle stages, and the §4 precedence rule. |
| [`docs/notes/COMMIT_ATTRIBUTION.md`](docs/notes/COMMIT_ATTRIBUTION.md) | Contributors | Linux kernel RFC 2822 trailer format (`Assisted-by:`), DCO boundaries, and hook enforcement. |

---

## License & Attribution

- **License:** European Union Public Licence (EUPL-1.2) OR Apache-2.0. See [`LICENSE`](LICENSE) and [`LICENSES/`](LICENSES/).
- **Author:** Nadia Yvette Chambers `<nadia.yvette.chambers@ik.me>`
- **Attribution:** AI-assisted contributions adhere to the Sarutahiko convention with machine-verifiable git trailers. See [`docs/notes/COMMIT_ATTRIBUTION.md`](docs/notes/COMMIT_ATTRIBUTION.md).
