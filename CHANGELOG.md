# Changelog

## 0.1.10 - 2026-08-12

- **IR / Codegen**: `Box::new(StructLit)` as a direct `let` initializer allocates `sizeof(Struct)` / `Struct*` (IR no longer types struct literals as `int`; `Box::new` prefers the let expected type like `Vec::new`).

## 0.1.9 - 2026-08-12

- **Type checker**: recursive structs/enums no longer stack-overflow the compiler. Cycle-aware walks for no-escape, `Send`, `Eq`, and drop analysis; bare value cycles (`next: Node`, `next: Option<Node>`) are rejected as `InfiniteSize`, while `Box` / `Vec` / raw-pointer indirection remains allowed (and `Box<&T>` still fails no-escape).
- **Codegen**: `Box::new` uses the argument's real type for `sizeof` / pointer type (no longer always `int`). Monomorphized `Option<Box<Struct>>` is emitted before the struct body (with a struct forward decl) so recursive boxes produce valid C.
- **Docs**: ION_SPEC §1.3 clarifies no self-referential *borrows* vs recursive owned types via indirection; skills and tests cover Send/Eq on recursive Box nodes.

## 0.1.8 - 2026-08-11

- **Type checker**: matching `&UserGenericEnum<Concrete>` substitutes type parameters into variant payload bindings (for example `Status::Ready(v)` on `&Status<int>` binds `v` as `int`, so `v + 0` type-checks). Previously the subst map only matched bare `Generic`, leaving `&T`.
- **Type checker**: `resolve_type_name` recurses into `&T` so `&Flag` parameters resolve `Flag` as an enum (not a struct), matching `&f` arguments.
- **Codegen**: match on `&Enum` / `&GenericEnum` deref-copies the scrutinee into a value temporary (`Status_int x = *s`, `Flag x = *f`), including when the param type still carries parser `Struct("Flag")` for an enum name. Live binding types keep `Vec::get_ref` copy payloads from being double-dereferenced.

## 0.1.7 - 2026-08-11

- **Codegen**: reborrowed non-copy struct fields passed to `&T` / `&mut T` parameters (for example `peek(c.data, 0)` with `c: &Container` and `data: Vec<int>`) emit `&(c->data)` so C pointer arity matches `Vec_T**`. Nested embedded paths use `.` after the first hop (`&(w->inner.data)`). Previously the bare field load (`Vec_T*`) compiled under default GCC flags and segfaulted at runtime.

## 0.1.6 - 2026-08-11

- **Parser**: `if 5 < x { f(x); }` (literal on the left of `<`/`<=`, function call in the body) no longer misparses as a struct literal. Struct-vs-block lookahead after `{` requires `name:` for fields, so calls like `f(` stay block statements.

## 0.1.5 - 2026-08-11

- **CLI**: `ion-compiler --help` / `-h` prints usage and exits 0 (aligned with `ion-build`).
- **Parser**: trailing `;` after a statement-form `match` is **optional** (ION_SPEC `match_stmt` needs none; existing `match { ... };` still parses). Rvalue `match` is unchanged.
- **Driver**: `ion-compiler` and `ion-build` reject programs with no `fn main` after merge (empty or helper-only entry files fail with `MissingMain` instead of claiming a successful compile).
- **Diagnostics**: invalid UTF-8 source fails at the file-read site with a clear path and byte offset (no Debug-formatted path noise).
- **Docs**: ION_SPEC §5.3 and skills clarify that scalar `&mut` write-through is unsupported; mutate via `&mut Struct` fields, `&mut` callee parameters, or owner writes under the borrow checker.

## 0.1.4 - 2026-08-11

- **Builtins**: `String::get(&String, int) -> Option<u8>` (non-panicking byte peek) and `Slice::get_ref(&[]T, int) -> Option<&T>` (local borrow mirroring `Vec::get_ref`, including array coercion). `Slice` is a lexer keyword for `Slice::` qualification. Indexed writes on a root owner conflict with a live `get_ref` borrow. Spec, skills, LSP completions, TextMate grammar, and integration tests updated.

## 0.1.3 - 2026-08-11

- **Language / type checker**: loop ownership uses structured reentry/exit edge snapshots (ION_SPEC §5.2). Move then `break`/`return` is allowed when there is no reentering path; after-loop state stays affine; exit-path disagreement errors at the loop join. Replaces the old "any move in a loop body" beta rule. Integration tests cover break/return/continue, while head-vs-break disagreement, and nested inner break.

## 0.1.2 - 2026-07-22

- **Fix**: string literals passed directly to `String`-typed call arguments now lower through `ion_string_from_literal` (same as `let s: String = "…"`). Previously call sites could pass a raw C string (segfault or silent no-op).
- **Fix**: extern call codegen restores `(uint8_t*)` on string literals for `*u8` parameters when resolving types from `extern_functions` (Linux CI `-Werror=pointer-sign` on io/ffi tests).
- **Docs**: `tests/README.md` notes that release archives ship this catalog only, not the `.ion` harness files.

## 0.1.1 - 2026-07-22

- **Fix**: method-call syntax on `&Vec<T>`, `&mut Vec<T>`, `&String`, and `&mut String` parameters (missing dereference in cgen; `String::len` routed through `Vec::len` in IR).
- **Fix**: `examples/http_server` links on Linux/macOS (`cflags_windows` for Winsock `close` mapping).
- **Tooling**: GitHub Actions release workflow (multi-platform archives with docs/examples verification), Dependabot for pinned action SHAs.
- **Docs**: `ion-build` runtime/stdlib walk-up discovery; `cflags_windows` / `cflags_unix` in `ion.toml`.

## 0.1.0 - 2026-07-21

First tagged release of the Ion toolchain.

- **Binaries**: `ion-compiler`, `ion-build`, `ion-lsp` (Cargo package version `0.1.0`)
- **Language**: move-only ownership, no-escape borrows, channels/`spawn`, generics, `match`, `defer`, FFI via `extern "C"` / `unsafe`
- **Tooling**: `ion.toml` project builds, Linux and Windows CI, integration harness, VS Code/Cursor extension
- **Docs**: `ION_SPEC.md`, `docs/BETA.md`, `docs/ABI.md`, `SECURITY.md`
- **Status**: First tagged `0.x` release. See monthly sections below for the full history leading to this tag.

## 2026-07

- **VM-style idioms**: `match` on `&Enum` from `Vec::get_ref`; struct field assignment and `+=` on owned/`&mut` paths; method desugaring (`vec.push`, `vec.get_ref`) with correct borrows; nested generic types (`Vec<Vec<int>>`); match-arm control-flow unification (`break`/`return` with value arms); `&str` call-site coercion; enum literals in `Vec::push`/`set` without double-wrapped C; `&mut Struct` field access via `->` in codegen. Integration tests and examples `bytecode_vm`, updated `showcase`, `todo_demo`, `http_server`, `text_summary`. Fix extern call typing so `&T` arguments match `&T` parameters (no erroneous copy-type ref stripping). Fix `Option<T>` match codegen to use the scrutinee type instead of the first registered monomorph. Former negative match-arm rvalue tests now pass as positive runs.
- **Trait bounds follow-up**: ION_SPEC §4.8 `Eq` row documents function pointers; integration test `test_trait_bound_eq_fn_ok`; method-call signature help threads generic bounds through `fn_hover_doc`. Fix IR generic monomorphization when the first type argument is a function identifier (`identity(add_one)`): infer fn-pointer types from program signatures so mangled instantiations are emitted and call sites use the correct name. Integration test `test_trait_bound_copy_fn_ok`; `examples/trait_bounds` exercises `identity(add_one)`.

## 2026-06

- **Readiness hardening**: beta compatibility and runtime ABI documents, a lightweight security policy, CLI/`ion-build` multi-error type diagnostics, sanitizer CI smoke (6 tests), and full integration harness `-Wall -Wextra -Werror` on Linux CI. Cgen warning-hygiene improvements (binding usage tracking and `(void)` silences, borrow/defer silences, string literal `.data`/`uint8_t*` casts, string `for...in` length casts), `String` runtime data as `uint8_t*`, and `CFLAGS`/`LDFLAGS` support in the integration harness.
- **Language**: `for` iteration, `match` guards, `else if`, `break`/`continue`, `loop {}`, `+=`, hex/bin literals, function types `fn(T) -> R`, tuple literals and destructuring. Capture-free fn literals (`fn(T) -> R` lowered to static C function pointers; `ClosureCapture` for outer bindings).
- **Stdlib & runtime**: `fmt.ion`, `Result<T, E>`, `fs.read_to_string`, `String::push_byte`.
- **Compiler**: scope-drop codegen, `pthread` spawn, slice bounds checks, array-to-slice coercion, struct/enum field drops, `String` equality, module function name mangling, lasting-borrow rules (ION_SPEC 5.3), field-path borrow exclusivity, move/copy tracking fixes, generic monomorphization, generated C file banner (repo-relative source labels via `portable_source_label`, GNU C note, merged stdlib note, multi-file provenance, comment-safe path escaping).
- **LSP**: diagnostics, hover, completion, go-to-definition; multi-error reporting; symbol table mirroring; diagnostics cleared on close; hover fixes for `let` bindings and module-qualified calls. Contiguous `//` doc comments attached to declarations (including `pub` items) for hover.
- **Tooling**: GitHub Actions CI (Linux and Windows), pinned toolchain (1.96.0), `test_expectations.tsv` manifest, `--version`, line-numbered errors, Cursor agent skills. Split `tc` and `cgen` into submodules. `ion-build` driver and `ion.toml` manifests (`single`/`multi`, `out_dir`, `cflags`, `ldflags`, `stdlib_paths`, `emit_in_source`); per-example manifests and `build_hello`/`build_bad_main` harness smoke tests. Shared `discover_import_config` and stdlib search paths for `ion-compiler`, `ion-build`, and LSP. Integration harness precompiles `runtime/ion_runtime.c` once per run (`RUNTIME_OBJ`). `writing-ion-code` agent skill; `creating-ion-skills` examples index lists all eight project skills. Documented checked-in `examples/*.c` codegen snapshots in README and integration-test skill; regenerated example C output. Fixed `researching-pl-literature` skill `paper-seeds` reference formatting.
- **Docs**: README, CONTRIBUTING, ION_SPEC, and agent skills aligned on project layout, `ion-build` workflow, `emit_in_source`, stdlib import order, LSP features and limitations, `src/build/` checklists, portable Git Bash paths for `test_runner.sh`, and rebuilding release binaries before harness or example C regen (stale `target/release/ion-compiler` note).
- **Fixes**: match rvalue codegen, `Vec` struct drops, channel codegen, parser handling of `alias::call()`, scope-drop for moved-into-call bindings, HTTP server on Windows, integration harness on Windows. Cgen return-unwind: stop marking bindings dropped after inner-branch `return` (restores outer-path drops), dedupe `_`-prefixed unused silences, mark call arguments moved at emission to avoid double-free on unwind, and unify all function returns via `emit_function_return` (including diverging rvalue `match` arms). Cgen struct field move-out neutralizes owned fields after partial move, deferred to statement end when the move is a call argument; `Vec::push` passes address of struct lvalues; non-generic enums emit before structs; tuple IR uses resolved element types and `tuple_Vec_T` mangling with compound-zero `ret_val` for tuple returns (functions and fn literals). Integer indexing and `Vec<i32>` inference in the type checker. Match rvalue arms: reject diverging arms mixed with value arms; structural control-flow analysis for arm bodies (nested `if`/`else`, `loop` without `break`, `unsafe` blocks); reject mixed diverge and value-producing paths within one arm; cgen assigns through `if`/`else` value branches. `fmt::int_to_string` uses `String::push_byte` per digit instead of per-digit `push_str` branches; integration test asserts `0`, negatives, and `int::MIN`. `int::MIN`/`int::MAX` on integer primitives; `Vec::new()`/`with_capacity()` infer `T` from a `let` type annotation. Clippy fix in `portable_source_label`. `Vec::get`/`Vec::pop`: IR infers `Option<T>` from the vector argument; cgen dereferences `&mut Vec<T>` parameters, unpacks runtime `Option` via `ion_option_from_raw`, and uses a temp for struct-returning `Vec::push`/`Vec::set` call values. Grouped `match` switch arms: scope drops before the case `break` (GCC `-Wimplicit-fallthrough` under `-Werror`); guarded arms in a shared variant case break inside the guard `if` so fallback arms do not run. Cgen: enum literals lower to compound initializers (no per-variant `_new` helpers); unused bindings and parameters silenced with `(void)` at scope unwind instead of blanket `ION_MAYBE_UNUSED`. Cgen monomorphization: `Vec<T>`/`Option<T>` typedefs mangle Ion type names (`Vec_String`, not C typedefs); `match Vec::get`/`Vec::pop` resolves `Option<T>` from return type or vector element type; `String::push_str` with owned `String` reads source `.data`/`.len`. Integration tests `test_vec_string_mangle`, `test_vec_get_multi_option`, `test_string_push_str_owned`, `test_vec_get_putback`. ION_SPEC documents `Vec::get` move-out and put-back scan. **`Vec::get_ref`**: stack-local `Option<&T>` for read-only vector peek; shared borrow on the vector owner; match arms bind `&T` as a pointer (no by-value copy of struct elements with nested owned fields); IR records match pattern bindings for nested scans; integration tests `test_vec_get_ref_*` including `test_vec_get_ref_scan_nested_vec`. `String::len` codegen: null-check the `String` pointer, not `&local` (fixes GCC `-Waddress` under CI `-Werror`).
- **Examples**: `text_summary` (fixture file), `data_lib` (multi-module), `channel_worker` (flat single-file), `todo_demo` (interactive stdin, `Vec` of structs with `String`). Per-example `*.toml` manifests for `ion-build`. `http_server` accepts clients until stdin `quit`.
- **Multi-file fixes**: merge private struct types for cross-module type checking; per-module C symbol prefixes in multi-file codegen (`io_print_int`); walk-up `runtime/` discovery for nested build directories. Integration tests `test_multi_struct`, `test_multi_fmt_io`. Example policy: each demo under `examples/<name>/` with `ion.toml`; build output under `target/` only (no committed `.c`).

## 2025-12

- `ion-lsp` and VS Code extension; go-to-definition. Runtime, tests, examples, and print lowering updates. Type-alias resolution in C prototypes.

## 2025-11

- Initial compiler (lexer, parser, tc, IR, cgen), C runtime, `ION_SPEC.md`, examples, integration tests, `io`/`fs` stdlib. Core language: ownership, borrows, channels, `spawn`, generics, `match`, `defer`, FFI.
