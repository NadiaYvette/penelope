<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# Compiler Verification & Zero-Contamination Rules

**Status:** ACTIVE RULE · Bounded Context Extension

- Zero warnings tolerated under `-Wall -Wcompat -Wredundant-constraints`.
- All 5 physical verification gates (`./bin/<project>-verify`) must pass.
- Never commit broken builds, failing tests, or unverified changes.
