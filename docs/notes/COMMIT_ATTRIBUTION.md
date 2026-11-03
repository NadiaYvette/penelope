<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# Commit Attribution & Git Trailer Standards (`COMMIT_ATTRIBUTION.md`)

**Status:** POLICY · v1.0 · 2026-11-03 · blessed  
**Project:** penelope  
**Corpus Inventory:** [`../INDEX.md`](../INDEX.md)  
**Parent Strategy:** [`DOC_STRATEGY.md`](DOC_STRATEGY.md)  
**Related Documents:** [`../../AGENTS.md`](../../AGENTS.md), [`PENELOPE_DESIGN.md`](PENELOPE_DESIGN.md), [`../MAINTAINER_GUIDE.md`](../MAINTAINER_GUIDE.md).

---

## 1. The Attribution Convention

All commits in `penelope` assisted by artificial intelligence coding tools, large language models, or workflow orchestrators must use standard RFC 2822 git trailers:

```gitcommit
Assisted-by: <Tool> (<Provider> / <Model>)
Orchestrated-by: <Engine>
```

### Canonical Examples

- **Assisted by model with explicit provider:**
  ```gitcommit
  Assisted-by: Antigravity (Google DeepMind / Gemini 3.8 Flash)
  ```
  ```gitcommit
  Assisted-by: Antigravity (Anthropic / Claude 3.7 Sonnet)
  ```
  ```gitcommit
  Assisted-by: Freebuff (Upstage AI / Solar Pro 4)
  ```
- **Assisted where model is unspecified or varied across sessions:**
  ```gitcommit
  Assisted-by: Antigravity
  ```
- **Orchestrated by an autonomous workflow runner:**
  ```gitcommit
  Assisted-by: Antigravity (Google DeepMind / Gemini 3.8 Flash)
  Orchestrated-by: Yamaarashi Task Runner
  ```

---

## 2. Rationale & Legal Integrity

This convention is inherited from the `sarutahiko` and `polytropos` ecosystem standards:

1. **Auditability & Provenance:** Identifying which model and provider generated or assisted code facilitates diagnosing model-specific regressions, hallucinations, or stylistic patterns across the codebase history.
2. **Orchestrator Distinction:** Disentangles the *coding model* (`Assisted-by:`) from the *workflow or task engine* (`Orchestrated-by:`).
3. **Accuracy without Misattribution:** Prevents generic or inaccurate tool labeling by recording the precise model tier in the commit log.

---

## 3. Disallowed Trailers & Legal Boundaries

The following git trailers are strictly forbidden for AI tools:

| Forbidden Trailer | Legal / Technical Defect |
|---|---|
| `Co-authored-by: <AI-Tool>` | Claims legal co-authorship. An AI system cannot hold copyright or certify the Developer Certificate of Origin (DCO). |
| `Co-developed-by: <AI-Tool>` | Same legal defect as `Co-authored-by`. |
| `Signed-off-by: <AI-Tool>` | Certifies the DCO. Only human individuals can legally sign off on a commit. |
| Robot emojis (`🤖 Generated with ...`) | Unstructured decoration carrying no machine-parseable metadata; rejected by repository validation. |

*Note:* `Signed-off-by:` with a human committer's name and email is fully permitted and welcomed.

---

## 4. Repository Enforcement

This policy is enforced through three complementary layers:

1. **Git Commit Hook (`.githooks/commit-msg`):**  
   Validates trailer syntax, rejects forbidden AI co-authorship trailers, and ensures proper format.
2. **Commit Message Template (`.gitmessage`):**  
   Provides formatting instructions directly in the interactive commit editor.
3. **AI Assistant Guidance (`AGENTS.md`):**  
   Encodes the attribution format into the ~4 KB root orientation projection.
