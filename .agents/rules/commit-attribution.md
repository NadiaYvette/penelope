<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# Commit Authoring & AI Attribution Rules

**Status:** ACTIVE RULE · Bounded Context Extension
**Enforcement:** `.githooks/commit-msg` · `.gitmessage`

## 1. Linux Kernel RFC 2822 Trailer Schema

AI coding assistant attribution must strictly use RFC 2822 trailer format:

1. `Assisted-by:` (Required when AI assisted):
   - Format: `Assisted-by: <Tool> (<Provider> / <Model>)` or bare `Assisted-by: <Tool>`
2. `Orchestrated-by:` (Optional when an orchestration framework directed the task):
   - Format: `Orchestrated-by: <Engine>`

## 2. Legal Boundary: The Developer Certificate of Origin

- NEVER use `Co-authored-by:` or `Co-developed-by:` for AI assistants.
- NEVER add `Signed-off-by:` on an AI's behalf.
