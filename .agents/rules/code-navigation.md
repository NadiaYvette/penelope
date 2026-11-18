<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# Token-Saving Code Navigation Funnel

**Status:** ACTIVE RULE · Bounded Context Extension

Always follow the token-conservation funnel when exploring code:
1. `./tags` symbol jump via ripgrep/grep (cheapest token cost).
2. Targeted file slice viewing (view_file with StartLine/EndLine).
3. Avoid full-file reading when symbol lookups suffice.
