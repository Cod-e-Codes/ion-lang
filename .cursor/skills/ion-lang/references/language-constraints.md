# Ion Language Constraints

Summarized from ION_SPEC.md §1. Read the full spec for grammar and edge cases.

## Ownership

- Every value has exactly one owner
- Assignment, argument passing, and `return` **move** by default
- Primitives, references, function pointers, and aggregates of `Copy` fields with no `impl Drop` are **copied**, not moved (ION_SPEC section 4.8 and 5.2). `Box`, `Vec`, `String`, channels, `JoinHandle`, `File`, raw pointers, and protocol endpoints are not `Copy`. `Allocator` is `Copy`.
- Use-after-move on non-copy types → compile error (`UseAfterMove`)

## No-escape borrowing

References `&T` and `&mut T` are stack-local views. **Rejected:**

- Returning references from functions
- Storing references in struct fields or enum variants
- Storing references in `Box`, `Vec`, or arrays (including locals and parameters)
- Sending references through channels
- Capturing references in `spawn` (non-`Send`)
- Any pattern requiring cross-function lifetime reasoning

**Borrow conflicts** (ION_SPEC section 5.3): at most one `&mut T` or any number of `&T` on the same place. `s.x` and `s.y` are different places. Nested paths conflict on a shared field prefix (`s.a.b` conflicts with `s.a.c` and with `s.a`). A literal index is its own path. A non-literal index and a slice path borrow the whole owner. A lasting borrow stays live until the last use of every binding that holds it. Copies, field and index reborrows, tuple and `match` results, assignments, enum values, and struct literals that store the reference carry that loan. A reference created inside that value (`Hold { v: &mut s }`, `Option::Some(&mut s)`, `(&mut s.x, 1)`, or a `match` arm that yields `&mut s.x`) is the same loan, live everywhere the holding value is live. A `match` arm that stores the reference in a local and then yields the local keeps that loan on the match result. An enum, struct, or array that holds it keeps the loan until that binding leaves scope. A nested block does not end an outer loan. A carrier used only in one `if` arm is not live in the other arm. A loan still held after the `if` stays live. A use inside a loop covers the whole loop. Ephemeral `&` / `&mut` in call arguments are checked but not stored. Mutation through `&mut` uses struct field paths or `&mut` callee parameters; there is no unary `*` and no assign-through a bound scalar `&mut T`.

APIs that would return `&T` in Rust must use owned values, indices, or the patterns in [writing-ion-code/references/verified-patterns.md](../writing-ion-code/references/verified-patterns.md) (`Vec::get_ref` / `Slice::get_ref` / `Arena::get_ref` / `String::get`). Prefer `Handle` + `Arena<T>` (`stdlib/handle.ion`) over a raw `int` when slots can be reused.

## Concurrency

- `spawn { ... };` outside `scope` creates a detached OS thread. `let h: JoinHandle<T> = spawn { ... };` is joinable. `T` defaults to `void`. `join(h)` moves `T` out. Drop detaches. `scope { ... }` joins handles still owned in that block.
- `protocol Name { send T; recv U; end; }` and `endpoint<Name>()` return a client and its dual. `send` and `recv` consume the endpoint and return the next step. Drop before `end` is allowed. Clone is not.
- `select { let v = recv(&mut rx) => { ... } default => { ... } }` (or `timeout(ms)`). Without `default`/`timeout`, wait until a recv arm can take a message or disconnects.
- `channel<T>()` / `channel<T>(cap)` → `(Sender<T>, Receiver<T>)` - bounded MPSC
- `clone_sender(&tx) -> Sender<T>` (Receiver stays unique)
- `send(&tx, v) -> SendResult<T>` moves `v` into the channel; unused `Closed(T)` still drops `T`
- `recv(&mut rx) -> Option<T>`; `None` after the last sender is dropped and the buffer is empty
- `try_send` / `try_recv` are nonblocking (`TrySendResult<T>` / `TryRecvResult<T>`)
- Only `Send` types cross thread boundaries (`JoinHandle` is `Send`; `File` is not)

## Memory

- Stack by default; `Box<T>` for explicit heap
- `defer` for deterministic cleanup at scope exit (`break` / `continue` included)
- `Vec<T>`, `String`, tuples, and arrays drop at scope end (elements of a dropping `T`, then the backing array for `Vec`)

## Unsafe boundaries

Inside `unsafe { ... }`:

- Array and slice indexing and index assignment skip bounds checks
- All `extern "C"` calls must be in `unsafe` blocks
- There is no unary `*` deref; raw `*T` is FFI pass-through only

## Types (surface)

Primitives, `bool`, integers (`i8`-`i64`, `u8`-`u64`), `f32`/`f64`, structs, enums (tuple + struct variants), generics, `[T; N]`, `[]T`, `Box<T>`, `Vec<T>`, `String`, `JoinHandle<T>`, `Allocator`, `File`, raw `*T`, protocol endpoints.

`if`/`while` conditions must be `bool`. `for x in expr` over `Vec<T>`, `[T; N]`, `String` (bytes as `u8`), or `Iter<T>`. A capture-free fn literal is `fn(...) -> R`. A literal that moves owned outer bindings is a closure value. A reference capture is `ClosureCapture`. Postfix `?` works on owned `Option`/`Result` and on an owned enum with one success variant when the function returns that same enum.
