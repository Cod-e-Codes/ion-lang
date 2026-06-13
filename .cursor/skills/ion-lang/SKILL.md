---
name: ion-lang
description: >-
  Orient agents working on the Ion programming language and ion-compiler Rust
  codebase. Covers ownership/no-escape rules, compiler pipeline (lexer → parser
  → type checker → IR → C codegen), build commands, and project layout. Use
  whenever the user mentions Ion, ion-compiler, ion-lang, ION_SPEC, ownership,
  channels, spawn, C backend, or is editing any file in this repository - even
  if they only say "fix the compiler" or "add a language feature."
---

# Ion Language & Compiler

Ion is a move-only, no-GC systems language transpiled to C. This skill orients you in the repo before making changes.

## Quick orientation

| Resource | Purpose |
|----------|---------|
| [ION_SPEC.md](../../../ION_SPEC.md) | Authoritative language semantics and grammar |
| [README.md](../../../README.md) | Build, run, project structure |
| [tests/README.md](../../../tests/README.md) | Integration test catalog |

**Compiler pipeline** (see [references/compiler-pipeline.md](references/compiler-pipeline.md)):

```
.ion → compiler::parse_module (imports/cycles, then lex+parse per file) → type checker → IR → cgen → .c → gcc + runtime → executable
```

Rust modules in `src/`: `lexer`, `parser`, `ast`, `compiler` (module resolution), `tc` (`mod.rs`, `ownership.rs`, `builtins.rs`, `types.rs`), `ir`, `cgen` (`mod.rs`, `types.rs`, `builtins.rs`, `drop.rs`), `lsp`.

## Non-negotiable language constraints

Ion identity depends on these - do not weaken them without explicit user direction. Details in [references/language-constraints.md](references/language-constraints.md).

- Move-only single ownership; use-after-move is a compile error
- No-escape references (`&T`, `&mut T`) - stack-local only
- Channels-only concurrency with structural `Send` checking
- No GC; explicit heap via `Box<T>`
- C backend with C-compatible layout

## Build and verify

```bash
# Rust unit tests (lexer has inline tests)
cargo test

# Release compiler (integration tests expect this path)
cargo build --release --bin ion-compiler

# Integration tests (Git Bash on Windows - not WSL)
cd tests && ./test_runner.sh

# Lint
cargo clippy
```

**Windows:** Stop `ion-lsp` / `ion-compiler` before rebuilding if you get "Access is denied". Use Git Bash for `test_runner.sh`.

**Manual compile cycle** (single-file; on Windows use `ion-compiler.exe`, `hello_world.exe`, and Git Bash or adjust paths):

```bash
./target/release/ion-compiler examples/hello_world.ion
gcc examples/hello_world.c runtime/ion_runtime.c -o hello_world \
    -I. -I.. -Iruntime -I../runtime -lpthread
```

## Specialized skills

Read these when the task matches:

| Skill | When |
|-------|------|
| `adding-ion-features` | Compiler work in `src/`, `runtime/`, `stdlib/`, or `ION_SPEC.md` |
| `ion-integration-tests` | Adding or running `.ion` integration tests in `tests/` |
| `ion-lsp-vscode` | LSP server or VS Code/Cursor extension in `ion-vscode/` |

## Change discipline

1. **Spec first** - If semantics change, update `ION_SPEC.md` before or alongside code.
2. **Minimal diff** - Match existing patterns in the module you're editing; don't refactor adjacent code.
3. **Test both paths** - Positive compile+run tests and negative compile-error tests where applicable.
4. **Rebuild release** - Integration harness defaults to `target/release/ion-compiler`.

## Common pitfalls

- References cannot escape functions, structs, enums, channels, or `spawn` - reject at type-check time.
- `extern "C"` calls require `unsafe` blocks.
- Multi-file mode: `--mode multi --output <name> <main.ion>` generates per-module `.c`/`.h`.
- Stdlib lives in `stdlib/` (`io.ion`, `fmt.ion`); imported module functions are emitted as `{alias}_{name}` in single-file merge mode (e.g. `io::print_int` -> `io_print_int`).
- Integration tests: add `tests/test_*.ion` plus one row in `tests/test_expectations.tsv` (see `ion-integration-tests` skill). Run `cd tests && ./test_runner.sh` to verify.
- LSP parses the **open buffer** in memory (lexer → parser), then `register_imports` which **fully `parse_module`s imported files from disk**. Parser/tc/import changes may need LSP updates (`ion-lsp-vscode` skill).
