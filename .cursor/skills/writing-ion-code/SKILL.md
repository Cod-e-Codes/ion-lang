---
name: writing-ion-code
description: >-
  Write new Ion programs and .ion source files using only verified language
  features from ION_SPEC.md, examples/, tests/, and stdlib/. Guides ownership,
  no-escape borrows, channels, and stdlib imports. Use when the user asks to
  write Ion code, create an Ion program, script, example, or application, or
  implement logic in .ion files. Do not invent syntax or APIs from Rust, C, Go,
  or other languages.
---

# Writing Ion Code

Ion is move-only, no-GC, and channels-only for threads. Before writing any Ion source, **read authoritative sources**. Do not port patterns from Rust, C++, Go, or training-data guesses.

## Verify before you write

```
Task progress:
- [ ] Read the relevant section of ION_SPEC.md for the feature
- [ ] Find a matching example in examples/, tests/, or stdlib/
- [ ] If no match exists, say the feature may be missing and stop guessing
- [ ] Compile and run (or add an integration test) before calling it done
```

| Source | Use for |
|--------|---------|
| [ION_SPEC.md](../../../ION_SPEC.md) | Grammar, semantics, stdlib API, known limitations |
| [examples/](../../../examples/) | Working single-file programs |
| [tests/](../../../tests/) | Edge cases, negative patterns, modules |
| [stdlib/](../../../stdlib/) | `io`, `fmt`, `fs`, `result` APIs |

For ownership and no-escape rules, see [ion-lang/references/language-constraints.md](../ion-lang/references/language-constraints.md).

For syntax templates and anti-patterns, see [references/verified-patterns.md](references/verified-patterns.md).

## Program workflow

1. **Pick entry shape**: single file with `fn main() -> int`, or multi-file with `import "path.ion" as alias;` and `pub` exports.
2. **Annotate function signatures**: parameter and return types are required on `fn` declarations (no inference across function boundaries).
3. **Respect moves**: owned values (`String`, `Vec<T>`, structs, enums) move on assign, call, return, `send`, and `spawn` capture. Primitives and references copy.
4. **Borrow locally only**: `&T` / `&mut T` inside the current function; never return them, store them in structs/enums, send them on channels, or capture them in `spawn`.
5. **Use stdlib for I/O**: prefer `import "stdlib/io.ion" as io;` over raw `extern "C"` in application code.
6. **Compile and run** before finishing.

## Minimal program (stdlib I/O)

From [examples/hello_world_safe/hello_world_safe.ion](../../../examples/hello_world_safe/hello_world_safe.ion):

```ion
import "stdlib/io.ion" as io;

fn main() -> int {
    io::println(String::from("Hello, World!"));
    return 0;
}
```

Import paths like `import "stdlib/io.ion" as io;` resolve via stdlib search paths (`ion.toml` `stdlib_paths`, `ION_STDLIB`, project `stdlib/`, then install-relative `stdlib/` next to the compiler). Same-directory and `../` relative paths still work. Use `pub fn` in library modules; call with `alias::name(...)`.

## Core syntax (verified)

**Bindings**

```ion
let x: int = 10;
let mut v: Vec<int> = Vec::new();
let items: Vec<MyStruct> = Vec::new(); // element type from annotation
```

**Integer limits**

```ion
if n == int::MIN { ... }
let cap: int = int::MAX;
```

**Structs and enums**

```ion
struct Point { x: int; y: int; }

enum Status {
    Ok { value: int };
    Err { code: int };
}
```

Tuple values (flat only, no nesting): `let t: (int, int) = (1, 2);` then `t.0`, `t.1`, or `let (a, b) = t;`.

**Control flow**

- `if` / `else if` / `else` conditions must be `bool`.
- `while`, `loop`, `break`, `continue`, `for x in expr;` (`for` ends with `;` per grammar).
- `match expr { Pattern => { ... } }` with guards `pattern if cond =>`.

**Methods**

`vec.push(10)` desugars to `Vec::push(&mut vec, 10)`. Qualified `Vec::new()` also works.

**Channels and spawn**

From [examples/spawn_channel/spawn_channel.ion](../../../examples/spawn_channel/spawn_channel.ion):

```ion
let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
let (tx_back, rx_back): (Sender<int>, Receiver<int>) = channel<int>();

spawn {
    let mut rx_mut: Receiver<int> = rx;
    let value: int = recv(&mut rx_mut);
    send(&tx_back, value);
};

send(&tx, 42);

let mut rx_back_mut: Receiver<int> = rx_back;
let result: int = recv(&mut rx_back_mut);
```

`spawn` captures owned values by move. `T` in `channel<T>()` must be `Send`.

**Fn literals (capture-free)**

```ion
let f: fn(int) -> int = fn(x: int) -> int { return x + 5; };
return f(7);
```

Fn literals lower to static C functions and must not reference outer bindings (owned or by reference). Use named functions with extra parameters for customized behavior. See [tests/test_fn_literal_basic.ion](../../../tests/test_fn_literal_basic.ion) and [tests/test_fn_literal_callback.ion](../../../tests/test_fn_literal_callback.ion).

## Documentation comments

Ion uses plain `//` line comments only. No `///` or `//!`. Contiguous `//` lines **immediately above** a declaration (no blank line before the declaration) are attached as documentation for IDE hover; the same rule applies when `pub` precedes the item. A blank line breaks association. File-level overview comments go at the top of the file before imports. Section-divider comment blocks in examples should be separated from declarations by a blank line. See ION_SPEC section 12.6.

**Unsafe and FFI**

All `extern "C"` calls belong inside `unsafe { ... }`. See [tests/test_ffi_basic.ion](../../../tests/test_ffi_basic.ion).

## Stdlib (implemented)

Import with paths like `import "stdlib/io.ion" as io;`:

| Module | Functions |
|--------|-----------|
| `io.ion` | `print`, `println`, `print_str`, `print_int` |
| `fmt.ion` | `int_to_string`, `print_int`, `println_int` |
| `fs.ion` | `read_to_string_result(path: String) -> ReadResult` (POSIX/MinGW) |
| `result.ion` | generic `Result<T, E>` for library authors |

No stdlib stdin/line input. For POSIX `read` on fd 0, see [examples/todo_demo/](../../../examples/todo_demo/).

Built-ins: `Vec<T>`, `String`, `Box<T>`, `Option<T>`, `Result<T, E>` (define enums in-file or import). `Vec::get` / `Vec::pop` move elements out; use `Vec::get_ref(&v, i)` for read-only in-function peek (`Option<&T>`, local only). Match on `&Enum` from `get_ref` dispatches variants directly (no `*` deref). Struct field paths support `=` and `+=` on owned and `&mut` receivers. Nested generics such as `Vec<Vec<int>>` parse as consecutive `>` closings. String literals and `&String` coerce to `&str` at call sites.

## Build and verify

With `ion.toml` in the example directory:

```powershell
cargo build --release --bin ion-build
cd examples\spawn_channel
..\..\target\release\ion-build.exe build
.\target\spawn_channel.exe
.\target\spawn_channel.exe
echo $LASTEXITCODE
```

Multi-file: `cd examples\data_lib` then `..\..\target\release\ion-build.exe build`. See [examples/data_lib/README.md](../../../examples/data_lib/README.md). Interactive stdin: [examples/todo_demo/README.md](../../../examples/todo_demo/README.md).

For codegen inspection or `tests/` integration harness work, use `ion-compiler` plus manual `gcc` (see `ion-lang` skill). On Windows add `-lws2_32` for channel/spawn when linking manually. Stop `ion-lsp` if rebuild fails with "Access is denied".

For programs under `tests/`, follow the `ion-integration-tests` skill (`test_expectations.tsv` row required).

## Do not assume (common hallucinations)

These are **not** in Ion today. Check ION_SPEC.md section 10.3 before using anything similar:

- Capturing closures (fn literals that reference outer variables), or `impl` blocks in user code
- User-defined traits, `where` clauses, or bounds other than built-in `Copy`, `Eq`, and `Send`
- Returning `&T` / `&mut T` or `Option<&T>` from functions
- References in struct fields, enum payloads, or channels
- Shared mutable state across threads (only channels + move)
- Macros (`println!`, `vec!`, etc.)
- `?` operator, `async`/`await`, `panic!`, `unwrap()` except `Box::unwrap`
- Union types `A | B` (reserved; use enums)
- Nested tuples, tuple `==`, or generic tuple type parameters
- `mut` on function parameters (use `&mut T` in the signature instead)
- `///` / `//!` doc comment syntax (use adjacent `//` instead; see ION_SPEC §12.6)
- File APIs beyond `fs::read_to_string_result` (streaming `File` is deferred in spec)

When unsure, **grep** `tests/` and `examples/` for the construct. If nothing matches, tell the user it is likely unsupported rather than inventing syntax.

## Related skills

| Skill | When |
|-------|------|
| `ion-lang` | Repo orientation, compiler pipeline |
| `ion-integration-tests` | Adding runnable tests under `tests/` |
| `adding-ion-features` | Changing the compiler or ION_SPEC |
| `finding-ion-bugs` | Program compiles but behavior is wrong |
