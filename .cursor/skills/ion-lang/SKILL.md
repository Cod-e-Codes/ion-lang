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

Rust modules in `src/`: `bin` (ion-build, ion-lsp), `lexer`, `parser`, `ast`, `compiler` (module resolution), `build` (manifest, C toolchain, `ion build` driver), `tc` (`mod.rs`, `ownership.rs`, `builtins.rs`, `types.rs`), `ir`, `cgen` (`mod.rs`, `types.rs`, `builtins.rs`, `drop.rs`), `lsp`.

Binaries: `ion-compiler` (transpile/codegen), `ion-build` (full project build via `ion.toml`), `ion-lsp`.

## Non-negotiable language constraints

Ion identity depends on these - do not weaken them without explicit user direction. Details in [references/language-constraints.md](references/language-constraints.md).

- Move-only single ownership; use-after-move is a compile error
- No-escape references (`&T`, `&mut T`) - stack-local only
- Channels-only concurrency with structural `Send` checking
- No GC; explicit heap via `Box<T>`
- C backend with C-compatible layout

## Build and verify

```powershell
cargo test
cargo build --release --bin ion-compiler --bin ion-lsp --bin ion-build
cargo clippy -- -D warnings
& "${env:ProgramFiles}\Git\bin\bash.exe" -lc 'cd tests && ./test_runner.sh'
```

**Application builds:** `ion-build` reads `ion.toml` (walks up from cwd), transpiles, compiles C, links runtime. Default output under `target/`:

```powershell
.\target\release\ion-build.exe build
```

**Codegen inspection / integration harness:** `ion-compiler` still transpiles only (or `--mode multi` with in-tree link). Integration tests call `ion-compiler` directly; `test_runner.sh` also runs `ion-build` smoke tests in `tests/build_hello/`.

**Stdlib imports:** `import "stdlib/io.ion" as io;` resolves via manifest `stdlib_paths`, `ION_STDLIB`, `{project_root}/stdlib`, then install-relative `stdlib/` next to the compiler. CLI and LSP share `build::discover_import_config`.

**Windows:** Stop `ion-lsp` / `ion-compiler` before rebuilding if you get "Access is denied". After compiler or import-resolution changes, rebuild LSP and reload the editor window:

```powershell
cargo build --release --bin ion-lsp
```

**Manual transpile cycle** (advanced; integration tests use this path):

```powershell
.\target\release\ion-compiler.exe examples\hello_world.ion
gcc examples\hello_world.c runtime\ion_runtime.c -o hello_world.exe -I. -Iruntime -lpthread
```

Add `-lws2_32` on Windows when linking programs that use channels, `spawn`, or sockets.

## Specialized skills

Read these when the task matches:

| Skill | When |
|-------|------|
| `writing-ion-code` | Writing new `.ion` programs, examples, or application logic |
| `adding-ion-features` | Compiler work in `src/`, `runtime/`, `stdlib/`, or `ION_SPEC.md` |
| `ion-integration-tests` | Adding or running `.ion` integration tests in `tests/` |
| `ion-lsp-vscode` | LSP server or VS Code/Cursor extension in `ion-vscode/` |
| `finding-ion-bugs` | Debugging, regressions, test failures, codegen mistakes |
| `researching-pl-literature` | Academic PL papers relevant to Ion design or compiler work |
| `creating-ion-skills` | Authoring or editing skills in `.cursor/skills/` |

## Change discipline

1. **Spec first** - If semantics change, update `ION_SPEC.md` before or alongside code.
2. **Minimal diff** - Match existing patterns in the module you're editing; don't refactor adjacent code.
3. **Test both paths** - Positive compile+run tests and negative compile-error tests where applicable.
4. **Rebuild release** - Integration harness defaults to `target/release/ion-compiler`.

## Common pitfalls

- References cannot escape functions, structs, enums, channels, or `spawn` - reject at type-check time.
- `extern "C"` calls require `unsafe` blocks.
- Multi-file mode: `--mode multi --output <name> <main.ion>` generates per-module `.c`/`.h`.
- Stdlib: `import "stdlib/io.ion" as io;` (resolved via `ion.toml` / `ION_STDLIB` / project `stdlib/`). Bare `import "io.ion"` also resolves when `io.ion` is on a search path.
- `ion build` (`ion-build` binary): project manifest at `ion.toml`; see ION_SPEC §10.1.
- Integration tests: add `tests/test_*.ion` plus one row in `tests/test_expectations.tsv` (see `ion-integration-tests` skill). Run `cd tests && ./test_runner.sh` to verify.
- Fn literals are capture-free only; references to outer bindings are rejected with `ClosureCapture`.
- LSP parses the **open buffer** in memory (lexer → parser with source for doc attachment), then `load_imports` which **fully `parse_module`s imported files from disk** (per-import errors are published). Parser/tc/import changes may need LSP updates (`ion-lsp-vscode` skill).
- Documentation: contiguous `//` lines immediately above a declaration attach to AST nodes; LSP hover shows signature plus prose. No `///` syntax.
