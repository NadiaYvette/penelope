<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# `penelope` Newbie Quickstart & Token-Conservation Guide

**Status:** LIVING GUIDE · v1.0 · 2026-11-03  
**Audience:** New Human Contributors, Effect System Authors, and AI Assistant Drivers (Antigravity, Claude Code, Cursor, Aider).  
**Parent Strategy:** [`docs/notes/DOC_STRATEGY.md`](notes/DOC_STRATEGY.md)  
**Corpus Inventory:** [`docs/INDEX.md`](INDEX.md)  
**Interactive Tutorial:** [`docs/TUTORIAL.md`](TUTORIAL.md)  
**Maintainer Guide:** [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md)  
**Design Monograph:** [`docs/notes/PENELOPE_DESIGN.md`](notes/PENELOPE_DESIGN.md)

---

## 0. Learning the Concepts First: Start with `TUTORIAL.md`

If you are new to `penelope` and want to build practical applications, read the hands-on tutorial in [`docs/TUTORIAL.md`](TUTORIAL.md):

1. **Foundational Mental Model & Setup:** The $(m, n, k, r)$-ary polycategorical perspective unifying monads and arrows.
2. **The Heftia Track (`Eff`):** Unary monadic effects, `send`, `runEff`, scoped `Reader`/`Local`, and lexically scoped `Error`/`Catch`.
3. **The Kernmantle Track (`Rope`):** Binary arrow rope pipelines, profunctor composition, disjunctive branching with `ArrowChoice`, and jump-table routing.
4. **Penelope's 7 Unique Superpowers:**
   - Multi-port conjunctive register wiring (`Record` frames).
   - Multi-exit disjunctive control flow (`Switch`).
   - Multi-region scoped control (`While` & `Saga`).
   - Compile-time static routing & peephole fusion (`RowMorph`).
   - Frame-rule automatic register wiring (`>&>`).
   - Generalized Cayley static reflection (`penelope-static`).
   - Partial interpretation & circuit residualization (`residualize`).
5. **Capstone Project:** A resilient multi-stage order processing engine (`OrderProcessing`).

Once you are comfortable with these concepts, return to this guide for repository navigation, token conservation, and debugging workflows.

---

## 1. The Core Invariant: Never Dump Whole Modules

In a Haskell codebase employing typechecker plugins and extensive GADT syntax trees, dumping full 400-line source modules into the conversation context wastes 2,500–4,000 tokens per turn and causes the **"lost in the middle" attention collapse** in language models.

Always follow the **Progressive Disclosure Funnel**:

```
Level 0: Global Navigation  (docs/INDEX.md, ./tags)       →   ~10–50 tokens
Level 1: Structural Outline (ast-grep, sg patterns)       →   ~100–250 tokens
Level 2: Targeted Slice     (view_file Start/EndLine)     →   ~200–500 tokens
```

---

## 2. Fast Symbol Navigation with `./tags` (Level 0 — <15 tokens)

Before opening any file, find the symbol's exact location in `./tags`:

```bash
grep -w "^<SymbolName>" tags
```

### Examples
```bash
grep -w "^Penelope" tags      # packages/penelope-core/src/Penelope/Core/Syntax.hs
grep -w "^RowMorph" tags      # packages/penelope-core/src/Penelope/Core/Route.hs
grep -w "^FrameWire" tags     # packages/penelope-core/src/Penelope/Core/Wiring.hs
grep -w "^elaborateAll" tags  # packages/penelope-core/src/Penelope/Core/Engine/Elaborate.hs
grep -w "^interpretAll" tags  # packages/penelope-core/src/Penelope/Core/Engine/Interpret.hs
grep -w "^Eff" tags           # packages/penelope-effects/src/Penelope/Effects/Unary.hs
grep -w "^Rope" tags          # packages/penelope-effects/src/Penelope/Effects/Binary.hs
grep -w "^StaticPenelope" tags# packages/penelope-static/src/Penelope/Static.hs
```

This returns the exact file path and line number in under 15 tokens. Next, view **only** the relevant lines using `view_file` (e.g. ±25 lines around the symbol definition).

To regenerate the index after adding new functions or types:
```bash
./bin/generate-tags
```

---

## 3. The Port and Row Mental Model Demystified

`penelope` generalizes computations from simple functions `a -> b` into polycategorical nodes parameterized by four row types:

```haskell
data Penelope (eh :: [Type -> Type])      -- Higher-order scoped effect signatures
              (ef :: [Type -> Type])      -- First-order algebraic effect signatures
              (ins :: Row Type)           -- m Conjunctive input registers
              (outs :: Row Type)          -- n Conjunctive output registers
              (exits :: Row (Row Type))   -- k Disjunctive control exits
              (regions :: Row PortSig)    -- r Higher-order scoped sub-regions
```

### 1. Conjunctive Ports (`ins` and `outs`)
- `ins` and `outs` are Polytropos row types (`'[ "x" ':= Int, "y" ':= Text ]`).
- At runtime, input registers arrive as a flat Polytropos `Record ins`, and output registers exit as a flat Polytropos `Record outs`.
- Because Polytropos records are backed by a single flat `SmallArray`, passing multiple arguments involves **zero tuple allocations**.

### 2. Disjunctive Exits (`exits`)
- In traditional Haskell, branching requires nested `Either` trees (`Either (Either a b) c`).
- In Penelope, `exits` maps exit branch labels to their respective output port rows:
  `'[ "ok" ':= '[ "res" ':= Double ], "err" ':= '[ "msg" ':= Text ] ]`.
- Each exit branch is paired with an explicit continuation in a Polytropos `Record` of continuations, dispatched in branchless $O(1)$ time.

### 3. Scoped Regions (`regions`)
- Enclosed sub-computations for higher-order effects (`local`, `catch`, `while`, `saga`).
- Each region specifies its required input and output port signature `(rIns, rOuts)`.
- Enclosing handlers cannot observe or clobber registers outside their declared port signature.

---

## 4. Instant Compiler Verification & Building (<50 tokens)

Do **NOT** run full workspace test sweeps after minor edits. Full rebuilds dump hundreds of lines of terminal output (~3,000 tokens).

### In-Memory Background GHCi Daemon (Tricorder)
If running with the Tweag Tricorder daemon:
```bash
# Query machine-readable compiler errors and warnings in JSON (<50 tokens)
tricorder status --json

# Block until current incremental compilation finishes
tricorder status --wait --json

# Inspect dependency source code directly without downloading tarballs
tricorder source Penelope.Core.Syntax
```

### Tier 0 POSIX Baseline Fallback
If the Tricorder daemon is not active, target specific packages:
```bash
# Quick dependency and configuration check
cabal build all --dry-run

# Run core syntax and two-phase engine tests (fastest)
cabal test penelope-core-test

# Run static analysis and property tests
cabal test penelope-static-test

# Run effect facades and concurrency tests
cabal test penelope-effects-test

# Complete workspace test suite
cabal test all
```

---

## 5. Structural Code Audits with `ast-grep` (<300 tokens)

When querying or refactoring structural patterns across `packages/`, use `ast-grep` (`sg`):

```bash
# Find all GADT constructor definitions
sg -p 'OpF $TAG $PAYLOAD' packages/

# Find all higher-order scoped elaborator handlers
sg -p 'handleScoped $ENV $OP = $$$BODY' packages/

# Find all FrameWire register wiring compositions
sg -p '$A >&> $B' packages/
```

---

## 6. First 15 Minutes Walkthrough: Constructing a Polycategorical Circuit

Let's build a minimal polycategorical pipeline that reads two input registers, branches on a condition, and emits either an error or a calculated output.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module Main where

import Penelope.Core
import Polytropos

-- Define port rows
type InPorts   = '[ "amount" ':= Double, "rate" ':= Double ]
type OutPorts  = '[ "converted" ':= Double ]
type ErrPorts  = '[ "error" ':= String ]

type Exits     = '[ "ok" ':= OutPorts, "err" ':= ErrPorts ]

-- A branching node: checks for negative rate
currencyConverter :: Penelope '[] '[] InPorts OutPorts
currencyConverter =
  switch @InPorts @Exits
    -- Branch condition
    (\frame ->
       let r = getField @"rate" frame
       in if r <= 0
            then injectVariant @"err" (insertField @"error" "Non-positive rate" emptyRecord)
            else injectVariant @"ok"  (insertField @"converted" (getField @"amount" frame * r) emptyRecord)
    )
    -- Branch continuations
    ( emptyRecord
        & insertField @"ok"  idPoly
        & insertField @"err" (liftPure $ \errFrame ->
            insertField @"converted" 0.0 emptyRecord)
    )

main :: IO ()
main = do
  let input = emptyRecord
        & insertField @"amount" 100.0
        & insertField @"rate" 1.25
      result = runPenelopePure currencyConverter input
  putStrLn $ "Converted: " ++ show (getField @"converted" result)
```

---

## 7. Troubleshooting Common Errors & Pitfalls

### 1. `No instance for (RowMorph src dst)`
- **Cause:** You attempted to compose two pipelines where the output ports of the first stage do not align with the input ports of the second stage, and no automatic permutation was synthesized.
- **Remedy:** Ensure `polytropos-plugin` is enabled via `{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}`. If port names differ, insert an explicit route adapter: `route (permuteRowMorph @"src" @"dst")`.

### 2. `No instance for (FrameWire frame ins outs frame')`
- **Cause:** The `(>&>)` frame-rule wiring combinator cannot find one or more required input ports in the active ambient register frame.
- **Remedy:** Verify that preceding stages or the initial frame provide all required input port labels and types.

### 3. `Leaked Scoped Handler Across Continuation`
- **Cause:** Attempting to invoke higher-order scoped handlers (`local`, `catch`) inside a deferred first-order algebraic interpreter.
- **Remedy:** Scoped effects must be resolved during Phase 1 (`elaborateAll`). Phase 2 (`interpretAll`) is strictly reserved for first-order algebraic operations.

---

## 8. Git Commit Attribution Protocol

All commits must follow the `sarutahiko` attribution schema enforced by `.githooks/commit-msg`:

```gitcommit
<Imperative subject line (<= 72 chars)>

<Detailed explanation of why this change was made, design trade-offs,
and verification invariants satisfied.>

Assisted-by: <Tool> (<Provider> / <Model>)
Orchestrated-by: <Engine>
```

**Legal Boundaries:** Never use `Co-authored-by:` or `Signed-off-by:` for AI tools. See [`docs/notes/COMMIT_ATTRIBUTION.md`](notes/COMMIT_ATTRIBUTION.md) for complete details.
