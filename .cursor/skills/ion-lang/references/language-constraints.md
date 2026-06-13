# Ion Language Constraints

Summarized from ION_SPEC.md §1. Read the full spec for grammar and edge cases.

## Ownership

- Every value has exactly one owner
- Assignment, argument passing, and `return` **move** by default
- Use-after-move → compile error (`UseAfterMove`)

## No-escape borrowing

References `&T` and `&mut T` are stack-local views. **Rejected:**

- Returning references from functions
- Storing references in struct fields or enum variants
- Sending references through channels
- Capturing references in `spawn` (non-`Send`)
- Any pattern requiring cross-function lifetime reasoning

APIs that would return `&T` in Rust must use owned values, indices, or callbacks in Ion.

## Concurrency

- `spawn { ... }` creates an OS thread
- `channel<T>()` → `(Sender<T>, Receiver<T>)` - bounded MPSC
- `send(&tx, v)` moves `v` into channel; `recv(&mut rx)` receives by move
- Only `Send` types cross thread boundaries

## Memory

- Stack by default; `Box<T>` for explicit heap
- `defer` for deterministic cleanup at scope exit
- `Vec<T>`, `String` drop at scope end (runtime-assisted)

## Unsafe boundaries

Inside `unsafe { ... }`:

- Array indexing skips bounds checks
- Raw pointer ops allowed
- All `extern "C"` calls must be in `unsafe` blocks

## Types (surface)

Primitives, `bool`, integers (`i8`-`i64`, `u8`-`u64`), `f32`/`f64`, structs, enums (tuple + struct variants), generics, `[T; N]`, `[]T`, `Box<T>`, `Vec<T>`, `String`, raw `*T`.

`if`/`while` conditions must be `bool`. `for x in expr` over `Vec<T>`, `[T; N]`, or `String` (bytes as `u8`).
