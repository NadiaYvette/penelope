# Changelog for penelope-core

## 0.1.0.0 — Initial Release

- Core polycategorical engine generalizing Heftia and Kernmantle.
- Unpacked `(m,n,k,r)`-ary polycategorical syntax GADT: `Polycategorical`.
- $O(1)$ jump-table route optimization via Polytropos row indexing.
- High-level smart constructors: `inject`, `fanout`, `assocL`, `assocR`, `swapSum`, `swapProd`, `distributeL`, `distributeR`.
- Sequential polycategorical composition (`composePoly`, `>>>`).
- Categorical frame rule wiring (`frame`, `cross`).
- Disjunctive polycategorical fanout (`fanoutDisj`).
- Row weakening and structural embedding (`weakenPoly`).
- Higher-order algebraic elaboration and vector interpretation engine.
- Partial interpretation substrate with structural residualization.
