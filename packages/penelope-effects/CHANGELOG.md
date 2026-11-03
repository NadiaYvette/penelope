# Changelog for penelope-effects

## 0.1.0.0 — Initial Release

- Domain effect battery for Penelope.
- Heftia-style unary monadic facade (`Eff`, `sendUnary`, `interpretUnary`).
- Kernmantle-style binary profunctor facade (`Flow`, `sendBinary`, `interpretBinary`).
- Standard effect suite: `State`, `Reader`, `Writer`, `Error`, `Coroutine`, `UnliftIO`, `Scoped`, `Concurrent`.
- Scoped semantics with robust continuation capturing and branch safety.
- Comprehensive allocation and throughput benchmarks.
