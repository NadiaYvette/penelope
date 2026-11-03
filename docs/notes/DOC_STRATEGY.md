<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# The Documentation Strategy (`DOC_STRATEGY.md`)

**Status:** POLICY · v1.0 · 2026-11-03 · blessed  
**Project:** penelope  
**Corpus Inventory:** [`../INDEX.md`](../INDEX.md)  
**Parent Strategy:** Adapted from `polytropos` and `sarutahiko` (same maintainer, same license). Section numbering is preserved deliberately so cross-references stay stable across sister projects.  
**Related Documents:** [`PENELOPE_DESIGN.md`](PENELOPE_DESIGN.md), [`IMPLEMENTATION_PLAN.md`](IMPLEMENTATION_PLAN.md), [`COMMIT_ATTRIBUTION.md`](COMMIT_ATTRIBUTION.md), [`../../README.md`](../../README.md).

---

## 0. What this document is

`penelope` is a three-package polycategorical effect system engine (`penelope-core`, `penelope-static`, `penelope-effects`) generalizing Heftia's higher-order algebraic effects and Kernmantle's profunctor arrow ropes, backed by the Polytropos unboxed storage substrate.

Its documentation policy governs how technical claims, architectural rationale, formal specifications, and tutorials are created, maintained, and verified across time. `AGENTS.md` carries four of these policies in operational form; this document is where they become one authoritative policy with an explicit rationale.

### 0.1 Not carried over

Recorded deliberately so that the absence of these items reads as a conscious architectural decision rather than an oversight:

| Dropped | Why |
|---|---|
| LaTeX whitepaper programme & CJK typesetting | No print whitepaper or CJK publication apparatus is required; GitHub Flavored Markdown and Haddock serve all consumers. |
| The doc-drift LLM judge | Machinery for multi-team corpora. The *precedence rule* it enforces (code wins at runtime; notes corrected regardless) is already in force via `AGENTS.md` and §4 here. |
| Radicle multi-party vassal gate | Single release lead and maintainer. Verification is enforced deterministically by `./bin/penelope-verify` rather than bureaucratic multi-party approval. |
| Docs-words to code-LOC ratio | Its purpose was to monitor prose volume against an arbitrary metric. Penelope values structural density, mathematical precision, and executable verification over sheer word count. §11 is repurposed for benchmark and inspection testing interaction. |

---

## 1. The audience map — four audiences, purpose-built genres

Canonicity is determined **per fact class** rather than globally per document:

1. **Arrivals & Newcomers → Orientation & Guided Learning:**  
   `README.md`, `docs/NEWBIE_GUIDE.md`, and `docs/TUTORIAL.md`.  
   Canonical for high-level orientation, quick-start reproduction, and the progressive disclosure navigation funnel. Defer to canonical design monographs for formal invariants.
2. **API Consumers & Library Authors → Haddock Documentation:**  
   Module intros, inline type signatures, and docstrings in `src/`. Haddock runs to full paragraphs—invariants, failure modes, algebraic laws, worked examples, and Unicode mathematics ($\forall \text{ins}, \mathcal{O}(|outs|), \sigma^*(\mathcal{H})$)—with no filler. `--hyperlinked-source` enabled across all packages.
3. **Maintainers & Systems Architects → Canonical Notes & Task Packets:**  
   `docs/notes/PENELOPE_DESIGN.md` (decisions D1–D10 and verification invariants), `docs/notes/IMPLEMENTATION_PLAN.md` (milestone specifications), and `docs/task_packets/` (executable task specifications). Canonical for rationale, design trade-offs, and open questions.
4. **AI Coding Assistants → Bounded Pointer Projections:**  
   `AGENTS.md` (~4 KB context budget) and `.agents/rules/`. A projection into selectively loadable canonical files, never a restatement.

### Canonicity per Fact Class

| Fact Class | Canonical Location | Secondary / Linked References |
|---|---|---|
| Architectural Decisions (D1–D10) | `docs/notes/PENELOPE_DESIGN.md` §1 | `README.md`, `docs/INDEX.md` |
| In-Code Verification Notes | `docs/notes/PENELOPE_DESIGN.md` §2 & `src/` | `docs/MAINTAINER_GUIDE.md` |
| Classical Naming & Hermeneutics | `docs/notes/DOC_STRATEGY.md` §12 & `README.md` | Package module intros |
| Verification Gates & Test Standards | `docs/MAINTAINER_GUIDE.md` & `bin/penelope-verify` | `AGENTS.md` |
| Commit Attribution Format | `docs/notes/COMMIT_ATTRIBUTION.md` | `.gitmessage`, `.githooks/commit-msg` |
| Allocation & Throughput Figures | `packages/penelope-effects/bench/Main.hs` | `README.md`, `docs/MAINTAINER_GUIDE.md` |
| Multi-Package Boundary Specs | `.cabal` files and `cabal.project` | `docs/INDEX.md`, `README.md` |

---

## 2. Single source of truth and drift

Any technical fact recorded twice drifts. When figures, package inventories, or design choices are repeated across files, exactly one document is the canonical home; all other occurrences must link to it.

1. **Mechanical Net Before Semantic Net:**  
   Link validity and tag integrity are checked deterministically:
   - `./bin/penelope-linkcheck`: Validates that every relative link and section anchor resolves without escaping the repository root.
   - `./bin/penelope-tagref`: Validates that all cross-reference tags (`tag:...` and `ref:...`) match valid target definitions without dangling references.
2. **Repairs are Proposed, Never Applied Silently:**  
   Mechanical repairs (e.g. updating a renamed path) are applied directly in the refactoring commit. Semantic changes that touch design decisions require an explicit entry in the decision log (§13).

---

## 3. Lifecycle and authority

Documents carry their status in the top header. Stages govern document maturity:

1. `seed` — Issue space or optimization opportunity recorded; policy pending.
2. `draft` — Full draft written, under initial development.
3. `review` — Under formal maintainer review.
4. `blessed` — Canonical decisions adopted; blessing date recorded in header.
5. `implemented` — Blessed design fully realized in code; stamped with release tag or commit hash.
6. `superseded` — Replaced by a newer document; **must link directly to its successor**.
7. `dead` — Removed from active documentation; tombstoned in repository history.
8. `live` — Living document, register, or tutorial: updated in place, never blessed or superseded.
9. `archive` — Preserved historical record or conversation transcript: verbatim, never edited.

### Invalidity Flags
An orthogonal overlay on stages 2–5 to flag defects without forcing a re-blessing cycle:
- `erroneous`: Contains known technical inaccuracies identified by subsequent testing or proof.
- `inconsistent`: Diverges from actual codebase behaviour (triggers §4 precedence repair).
- `stale`: Written against an older API or GHC version; needs refreshing.
- `dead-links`: Contains unresolvable links flagged by `./bin/penelope-linkcheck`.
- `needs-work`: Incomplete draft or partially realized task packet.

---

## 4. The rot problem — design ahead of code

Most architecture in advanced effect systems begins as formal notes ahead of implementation. Divergence between blessed notes and realizing code is prevented by the **Precedence Rule**:

> **Code wins at runtime; documentation notes must be corrected in the same change set.**  
> A behaviour change without a corresponding documentation update is an incomplete change. Blessed design notes are never granted mere advisory status.

### Operational Principles
1. **Source Anchoring is Load-Bearing:**  
   When a design note cites external theory (e.g. Heftia's two-phase elaboration, Kernmantle's profunctor routing, Goguen-Burstall institutions, or Homeric hexameters), the citation anchor goes inline at the claim.
2. **The Partial-Realization Shield:**  
   When work is intentionally deferred or staged across task packets, the design note records an explicit *unrealized-surface inventory*. Automated linting tools must not flag intentional pending items.
3. **Pre-Birth Markers:**  
   Prospective references to unwritten modules carry an inline `[planned: <path>]` marker, removed upon implementation.

---

## 5. Structure and discoverability

The documentation directory follows strict structural conventions:

```
docs/
├── INDEX.md                 # Corpus inventory table and curated reading paths
├── TUTORIAL.md              # 6-lesson hands-on interactive tutorial and capstone
├── NEWBIE_GUIDE.md          # Token-conservation progressive disclosure funnel
├── MAINTAINER_GUIDE.md      # 5 physical verification gates and release protocols
├── notes/                   # Canonical design monographs and strategy documents
│   ├── PENELOPE_DESIGN.md   # Decisions D1–D10 and in-code verification invariants
│   ├── IMPLEMENTATION_PLAN.md# Milestone roadmaps and formal specifications
│   ├── DOC_STRATEGY.md      # This document: documentation governance policy
│   └── COMMIT_ATTRIBUTION.md# RFC 2822 git trailer standards (Assisted-by:)
├── transcripts/             # Historical ideation dialogues and transcripts
└── task_packets/            # Isolated, machine-executable task definitions
```

### Path Conventions
- **IDs Canonical, Hyperlinks Convenience:** Formal design decisions (`D1`–`D10`) and verification tags (`tag:note-...`) are stable identifiers surviving file renames.
- **Sister Project References:** Sibling repositories are referenced as `~/src/<project>` for local co-development, and via canonical forge URLs for public consumption.

---

## 6. Rendering and build infrastructure

1. **Markdown is Canonical:** GitHub Flavored Markdown (`.md`) is the canonical storage format. No proprietary rendering engines or intermediate compilation passes are required.
2. **Haddock Build:** Generated via `cabal haddock all` with `--hyperlinked-source` and `--quickjump`. Every exported symbol must carry documentation.
3. **Link Checking:** Validated automatically by `./bin/penelope-linkcheck`.

---

## 7. Terminology and style control

1. **Dense Rationale-First Register:**  
   Design monographs use a dense, table-heavy, present-tense engineering register. Rationale precedes implementation details.
2. **Second-Person Active Voice for Tutorials:**  
   User-facing guides (`TUTORIAL.md`, `NEWBIE_GUIDE.md`) use direct, second-person imperative voice ("Run `cabal test`", "Define your port signature").
3. **Assistant-Context Budgets:**  
   `AGENTS.md` is strictly bounded to ~4 KB to avoid token degradation across AI coding sessions. Detailed explanations live in linked documents.

---

## 8. Co-authorship and authority

The Penelope corpus is **AI-drafted and maintainer-blessed**:
- Drafts, tests, and documentation may be authored in collaboration with AI coding assistants.
- Maintainer blessing certifies that every sentence, type signature, and verification check has been reviewed, executed, and adopted by the human maintainer.
- Git commits strictly separate technical assistance from legal copyright certification via RFC 2822 trailers (§4 of `COMMIT_ATTRIBUTION.md`).

---

## 9. Exposure, credit and tone

1. **Public by Default:** All architectural notes, transcripts, and benchmarks are public.
2. **Generous Academic Credit:** Attribution is recorded for conceptual forebears:
   - **Heftia** (Kozawa & Kobayashi): Two-phase higher-order algebraic effect elaboration.
   - **Kernmantle** (Vassor): Profunctor arrow ropes and static capability routing.
   - **Polytropos** (Chambers): Flat `Rep` unboxed row storage and typechecker plugin index synthesis.
   - **Goguen & Burstall**: Institutional satisfaction and contravariant model reduction.
3. **Respectful Engineering Voice:** Technical comparisons focus on trade-offs, formal invariants, and hardware physics, never disparaging alternatives.

---

## 10. Boundaries with adjacent artifacts

1. **Benchmarks (`bench/`):** Measurement harnesses live in `packages/penelope-effects/bench/Main.hs`, outside `docs/`. Prose cites the harness; prose does not invent numbers.
2. **Inspection Tests (`test/Inspection.hs`):** Formally verify zero-dictionary GHC Core representations via `inspection-testing`.
3. **No ADR Duplication:** Architecture decision records are consolidated into `docs/notes/PENELOPE_DESIGN.md` §1 rather than fragmented across ephemeral files.

---

## 11. The measurement interaction

1. **Reproducibility Conditions:** Benchmarks must be compiled with `-O2 -fproc-alignment=64` under isolated execution environments. Core line counts and inspection tests are deterministic; wall-clock times are subject to runner CPU throttling and must report variances.
2. **Inspection Testing as Proof:** Core inspection tests (`inspection-testing`) are treated as formal theorems. A commit that introduces a residual dictionary or indirect jump into the hot path fails the test gate immediately.

---

## 12. The Classical Hermeneutic Pattern

Naming in Penelope follows the **Classical Hermeneutic Pattern** shared across our ecosystem (`polytropos`, `penelope`, `gargamelle`):

```
┌────────────────────────────────────────────────────────────────────────┐
│                   THE CLASSICAL HERMENEUTIC PATTERN                    │
├────────────────────────────────────────────────────────────────────────┤
│ 1. PRIMARY CLASSICAL TEXTUAL ANCHOR                                    │
│    Direct citation of primary Greek/Latin epic, philosophical, or      │
│    satirical literature with book and verse coordinates                │
│    (e.g., Homer, Odyssey 2.104–105).                                   │
├────────────────────────────────────────────────────────────────────────┤
│ 2. PHILOLOGICAL ROOT VERB ANALYSIS                                     │
│    Etymological analysis of the operative verbs and grammatical aspects│
│    encapsulating the mechanical behavior of the software               │
│    (e.g., ὑφαίνεσκεν = higher-order weaving / elaboration;             │
│           ἀλλύεσκεν   = algebraic unweaving / interpretation).         │
├────────────────────────────────────────────────────────────────────────┤
│ 3. STRUCTURAL DATA & ENGINE MAPPING                                    │
│    Isomorphic mapping between classical objects and concrete data types│
│    (e.g., Penelope's web = (m,n,k,r)-ary GADT;                         │
│           Laertes' shroud = unboxed register frame;                     │
│           Torches by night = O(1) jump table dispatch).                │
├────────────────────────────────────────────────────────────────────────┤
│ 4. ECOSYSTEM PAIRING DYNAMICS                                          │
│    Bilateral synergy between sister repositories                       │
│    (e.g., Odysseus/Polytropos = versatile structural data substrate;   │
│           Penelope = dynamic, sovereign effect weaver).                │
└────────────────────────────────────────────────────────────────────────┘
```

This pattern ensures that naming is not whimsical decoration, but an instructional mnemonic grounding technical abstractions in durable human culture.

---

## 13. Decision Log

| Date | Decision | Rationale |
|---|---|---|
| 2026-10-31 | Adopt four-audience documentation model | Unifies Newbies, API Consumers, Maintainers, and AI Assistants under distinct genres. |
| 2026-10-31 | Establish the §4 Precedence Rule | Eliminates documentation rot by requiring code and design note synchronization in the same commit. |
| 2026-11-03 | Codify the Classical Hermeneutic Pattern (§12) | Standardizes Homeric and classical textual anchoring across Penelope and sister ecosystem repositories. |
| 2026-11-03 | Integrate 5-Gate Physical Verification (§11) | Binds `./bin/penelope-verify` (HLint, Tests, Tagref, Linkcheck, CLOC) directly into documentation governance. |
