# Pervasive `io_uring`: `StaticPenelope` Batch Fusion, Pinned `pykinos` Buffer Rings, and the `frankenstein` Zero-GHC-RTS Reactor

**Status:** Canonical Architecture Note  
**Constellation:** Cellarius (`penelope-uring`) / PykinosCEK (`pykinos-core`) / Ingolstadt (`frankenstein` / `organ-bank`) / Sectile (`telix` / `tessera`)  
**Target Environments:** Linux 6.1–6.12+ (GHC 9.14.1 user-space + freestanding C/MLIR/GIMPLE `frankenstein` RTS), with deterministic `UringSim` fallback for airgapped sandboxes  
**Date:** 2026-11-23  

---

## 1. Why `io_uring` + `Penelope` + Perceus Fit Together Structurally

Conventional asynchronous runtimes bolt `io_uring` onto callbacks, futures, or `epoll`-era reactors and immediately run into three architectural mismatches:

1. **Missed Batching (Syscall Thrashing):** Monad-only `>>=` hides independent I/O operations behind opaque closures, submitting one SQE per `io_uring_enter(2)` instead of amortizing one syscall across 16–64 operations.
2. **The Cancellation Use-After-Free Cliff (Rust/C `async`):** Dropping a future or throwing an exception while the kernel still holds a DMA pointer into a user-space buffer causes silent heap corruption unless ownership is detached from the caller's stack frame.
3. **Cross-Core Contention & Copying:** Shared global event loops bounce cache lines across cores, and allocating a fresh `ByteString` per read/write saturates both the allocator and major-GC scanner.

The Nadiverse stack eliminates all three problems by construction:

| Layer | Mechanism | What It Solves for `io_uring` |
| :--- | :--- | :--- |
| **`penelope-static` (`StaticPenelope`)** | `ApplicativeDo` (`apPenelope`) + `Rope` (`***` / `|||`) static syntax tree | **Compile-time / pre-submit SQE fusion:** independent operations lower into one unlinked parallel SQE batch; dependent sequences lower into kernel-linked `IOSQE_IO_LINK` chains; static resource extraction pre-registers `IORING_REGISTER_FILES` fixed FDs and `IORING_REGISTER_BUFFERS`. |
| **`pykinos-core` (`WalSlab`, `MsgAssoc`, `WordRing`, `TimingWheel`)** | Pinned, page-aligned `ByteArray#` slabs & zero-pointer `Word32` rings | **Zero-copy DMA & `IORING_REGISTER_PBUF_RING`:** `WalSlab` writes directly via `O_DIRECT` + `IORING_OP_WRITE_FIXED` + linked `FSYNC`; `MsgAssoc` receives directly from provided buffer rings via `IORING_OP_RECV_MULTISHOT`; `TimingWheel` backs `IORING_OP_LINK_TIMEOUT`. |
| **`frankenstein` / `organ-bank` (Perceus + 3-Profile `OrganIR`)** | Explicit `ERetain`/`EDrop`/`EReuse` + thread-per-core reactor | **Cancellation-safe DMA ownership & zero GHC RTS:** the ring's flight table owns the buffer token until the matching CQE (completion or `-ECANCELED`) arrives—making cancellation UAF structurally impossible. |
| **`gargamelle` / `tessera` (`gpfsl` + Iris)** | Explicit acquire/release ring ops in `Penelope` effect rows | **Formal verification:** the exact same SQ/CQ `head`/`tail` memory-ordering protocol verifies cleanly under `gpfsl` in Rocq/Iris and converges with `telix`'s kernel-v2 `UringBatchCtx`. |

---

## 2. Part I: User-Space `penelope-uring` (GHC RTS Integration)

### 2.1 Native Multi-Exit `UringOp` Effect (`Penelope.Uring.Effect`)

Instead of returning `Either Errno Int` boxes per I/O call, `UringOp` is a native `(m, n, k)`-ary `PolyEffect` dispatching completion outcomes directly into `Polytropos` continuation ports:

* **`"ok"`** (`'["bytes" ':= Int, "buf" ':= PinnedBufSlice, "flags" ':= Word32]`): normal CQE (`res >= 0`).
* **`"eof"`** (`'["buf" ':= PinnedBufSlice]`): zero-byte EOF / peer shutdown.
* **`"canceled"`** (`'["buf" ':= PinnedBufSlice]`): `-ECANCELED` / `-ETIME` (linked timeout or explicit cancel), returning the buffer handle safely so ownership is never leaked.
* **`"err"`** (`'["errno" ':= Int32, "buf" ':= PinnedBufSlice]`): negative kernel `errno` (`-res`).

### 2.2 `StaticPenelope` SQE Batch & Chain Fusion (`Penelope.Uring.Planner`)

Before touching the kernel ring, `Penelope.Uring.Planner` walks the static `Penelope` DAG exposed by `ApplicativeDo` (`apPenelope`) and `Rope` (`(***)`, `(|||)`) and compiles it into an explicit `SqePlan`:

1. **Parallel Fan-Out (`apPenelope` / `(***)`):** Independent `UringOp` leaves emit into the same submission batch with `IOSQE_IO_LINK = 0`, submitted together in a **single `io_uring_enter`**.
2. **Dependent Chains (`seqPenelope` with fixed-buffer handoff):** Sequential ops where step $i+1$ depends only on step $i$ succeeding (e.g., `WRITE_FIXED` $\to$ `FSYNC` on `WalSlab`, or `READ_FIXED` + `LINK_TIMEOUT`) fuse into kernel-executed **`IOSQE_IO_LINK`** chains—if step $i$ fails, the kernel automatically cancels step $i+1$ with `-ECANCELED` without an extra user/kernel round-trip!
3. **Static FD & Buffer Pre-Registration:** `StaticPenelope` (`Const ResourceManifest`) collects all long-lived FDs and pinned slab pools before execution begins, issuing one `IORING_REGISTER_FILES` + `IORING_REGISTER_BUFFERS` setup call so hot-path SQEs set `IOSQE_FIXED_FILE` (skipping kernel `fget`/`fput` atomic refcounting on every I/O).
4. **Dead-Result Elision:** Combining `analyzeLivenessAndEffects` with `SqePlan` marks unused completion payloads (`IOSQE_CQE_SKIP_SUCCESS` on Linux 5.17+) for fire-and-forget intermediate ops in a linked chain, slashing CQE ring traffic.

### 2.3 GHC RTS Safety Rules (Pinning, FFI Calling Conventions, & Capabilities)

Running `io_uring` under GHC's moving, generational GC requires four strict invariants:

1. **Strictly Pinned DMA Memory (`PinnedBuf`):**
   Every buffer passed to a kernel SQE MUST be allocated with `newPinnedByteArray#` or `newAlignedPinnedByteArray#` (4096-byte aligned for `O_DIRECT` `WalSlab` segments). Passing an unpinned `ByteArray#` is a guaranteed memory-corruption bug if GHC's copying GC relocates the object mid-DMA.
2. **Split `unsafe` vs. `safe` FFI Boundary:**
   * **Zero-syscall ring ops (`submitSqeMem`, `peekCqeMem`, `advanceCqHead`):** Pure user-space shared-memory loads/stores with `atomic_load_acquire` / `atomic_store_release`. Invoked via **`unsafe` FFI** (or Cmm primops) in ~5–15 ns without releasing the GHC capability.
   * **Blocking waits (`io_uring_enter(..., IORING_ENTER_GETEVENTS)`):** Either run via **`safe` FFI** on a dedicated OS thread, or register an `eventfd` via `IORING_REGISTER_EVENTFD` and wake GHC green threads through `threadWaitRead` when coexisting with GHC's standard M:N scheduler.
3. **Capability-Pinned Single-Issuer Rings:**
   Allocate **one `io_uring` instance per GHC capability** (`IORING_SETUP_SINGLE_ISSUER | IORING_SETUP_COOP_TASKRUN | IORING_SETUP_DEFER_TASKRUN`) driven by a `forkOn cap` reactor worker. Workers submit via a capability-local `Pykinos.Queue.WordRing`—eliminating cross-core mutex contention entirely.
4. **ABA-Safe 64-Bit `user_data` Flight Table:**
   Never pack a raw Haskell pointer (`StablePtr`) directly into `sqe->user_data` without generation checks. Instead, allocate a dense slot from a fixed-capacity flight table (`WordRing` free-list) and pack:
   ```text
   user_data (Word64) = (generation :: Word32 << 32) | (slotIndex :: Word32)
   ```
   Every CQE validates `slotGen[slot] == generation` before resuming the continuation and returning the buffer token, catching stale/double completions in $O(1)$ with zero heap allocation.

### 2.4 Bridging `pykinos` Structures to Registered Rings

| `pykinos` Structure | `io_uring` Mechanism | Zero-Copy Path |
| :--- | :--- | :--- |
| **`Pykinos.Storage.WalSlab`** | `IORING_REGISTER_BUFFERS` + `IORING_OP_WRITE_FIXED` + `IOSQE_IO_LINK` $\to$ `IORING_OP_FSYNC` | Fixed 4 KiB–64 KiB aligned pinned segments flush to NVMe via `O_DIRECT` in 1 syscall per commit group. |
| **`Pykinos.Net.MsgAssoc` & `SackWindow`** | `IORING_REGISTER_PBUF_RING` + `IORING_OP_RECV_MULTISHOT` + `IORING_OP_SEND_ZC` | Kernel selects receive buffers directly from the provided ring; incoming TSNs update `SackWindow` in place with zero per-packet `ByteString` allocation. |
| **`Pykinos.Queue.TimingWheel`** | `IORING_OP_LINK_TIMEOUT` + single ring-level `IORING_OP_TIMEOUT` | Arms at most one kernel timer per wheel tick instead of one timer SQE per active socket/task. |

---

## 3. Part II: Pervasive `io_uring` in the `frankenstein` Zero-GHC-RTS (`liborgan_rt`)

When `Penelope.Stage.OrganIR` compiles `Penelope` + `Polytropos` programs through `frankenstein` / `organ-bank` (targeting MLIR/LLVM or GCC GIMPLE), **the GHC RTS disappears completely**. The target binary runs on `liborgan_rt`—a minimal, Perceus-managed, thread-per-core `io_uring` runtime:

### 3.1 Thread-Per-Core Shared-Nothing Reactor (`organ_core_t`)

Each OS thread is pinned to one physical CPU core (matching Barrelfish / `telix` per-core dispatchers) and owns:
* One private `io_uring` (`IORING_SETUP_SINGLE_ISSUER | IORING_SETUP_DEFER_TASKRUN | IORING_SETUP_SUBMIT_ALL`).
* One private fixed-slot flight table (`organ_flight_t[RING_DEPTH]`) with 64-bit `(gen << 32 | slot)` `user_data` tags.
* One private `IORING_REGISTER_PBUF_RING` provided buffer ring + fixed FD table.
* One private Perceus local allocator (`mimalloc` v3.5.3 in Profile 1/2; static arena in Profile 0 `#[freestanding_linear]`).
* Cross-core messages travel via `IORING_OP_MSG_RING` (Linux 5.18+, waking another core's ring directly without userspace locks or `eventfd` syscalls!) or lock-free SPSC `WordRing` queues.

### 3.2 Perceus Cancellation-Safe DMA Ownership (3-Profile Memory Model)

Every in-flight SQE holds a typed ownership witness matching `OrganIR`'s 3-profile memory model:

| `OrganIR` Profile | Buffer Ownership While SQE Is In-Flight | On Caller Cancellation / Scope Exit |
| :--- | :--- | :--- |
| **Profile 0 (`#[freestanding_linear]`)** | Linear move into `flight[slot].buf`; caller statically loses access (`multiplicity = 1` proven by `LiquidInstitution`) | Caller emits `IORING_OP_ASYNC_CANCEL(user_data)`; buffer returns linearly to the `"canceled"` continuation port when the CQE arrives. **Zero RC header, zero malloc.** |
| **Profile 1 (`#[thread_local_perceus]`)** | Non-atomic `ERetain(buf)` stored in `flight[slot].buf` before `io_uring_enter` | Caller drops its local ref (`m_rc` stays $\ge 1$); kernel DMA completes safely; CQE handler executes non-atomic `EDrop(buf)` (or hands unique `m_rc == 1` buffer directly to `EReuse`!). |
| **Profile 2 (`#[concurrent_shared]`)** | Atomic `Release` `ERetain(buf)` across ring/core boundary | CQE handler executes `atomic_fetch_sub(Release)` + `Acquire` fence on destruction, matching `tessera`'s `gpfsl` `Arc` proof. |

**Result:** The notorious `io_uring` async-drop use-after-free bug is **impossible by construction** across all three memory profiles.

### 3.3 `Penelope` $\to$ `OrganIR` Lowering of Fused Batches

During `Penelope.Stage.OrganIR