# Changelog

## 2026-06

- **Language**: `for` iteration, `match` guards, `else if`, `break`/`continue`, `loop {}`, `+=`, hex/bin literals, function types `fn(T) -> R`, tuple literals and destructuring.
- **Stdlib & runtime**: `fmt.ion`, `Result<T, E>`, `fs.read_to_string`, `String::push_byte`.
- **Compiler**: scope-drop codegen, `pthread` spawn, slice bounds checks, array-to-slice coercion, struct/enum field drops, `String` equality, module function name mangling, lasting-borrow rules (ION_SPEC 5.3), field-path borrow exclusivity, move/copy tracking fixes, generic monomorphization, generated C file banner (source path, GNU C note, merged stdlib note, multi-file provenance, comment-safe path escaping).
- **LSP**: diagnostics, hover, completion, go-to-definition; multi-error reporting; symbol table mirroring; diagnostics cleared on close; hover fixes for `let` bindings and module-qualified calls.
- **Tooling**: GitHub Actions CI (Linux and Windows), pinned toolchain, `test_expectations.tsv` manifest, `--version`, line-numbered errors, Cursor agent skills. Split `tc` and `cgen` into submodules. Documented checked-in `examples/*.c` codegen snapshots in README and integration-test skill; regenerated example C output. Fixed `researching-pl-literature` skill `paper-seeds` reference formatting.
- **Fixes**: match rvalue codegen, `Vec` struct drops, channel codegen, parser handling of `alias::call()`, scope-drop for moved-into-call bindings, HTTP server on Windows, integration harness on Windows.

## 2025-12

- `ion-lsp` and VS Code extension; go-to-definition. Runtime, tests, examples, and print lowering updates. Type-alias resolution in C prototypes.

## 2025-11

- Initial compiler (lexer, parser, tc, IR, cgen), C runtime, `ION_SPEC.md`, examples, integration tests, `io`/`fs` stdlib. Core language: ownership, borrows, channels, `spawn`, generics, `match`, `defer`, FFI.
