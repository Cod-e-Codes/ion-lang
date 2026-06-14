---
name: adding-ion-features
description: >-
  Implement new Ion language features or compiler fixes across lexer, parser,
  AST, type checker, IR, and C codegen. Use when adding syntax, types, builtins,
  diagnostics, codegen, or changing compiler behavior in src/, runtime/, stdlib/,
  or ION_SPEC.md. Also use for parser errors, type check errors, IR lowering, cgen output, module imports,
  Send checking, or ownership/borrow rules - even if the user says "make X work
  in Ion" without naming a compiler stage.
paths:
  - src/**
  - runtime/**
  - stdlib/**
  - ION_SPEC.md
---

# Adding Ion Compiler Features

Follow this pipeline when changing language behavior. Read `ion-lang` skill for repo context and [references/stage-checklist.md](references/stage-checklist.md) for per-stage details.

## Workflow

Copy and track progress:

```
- [ ] 1. Confirm semantics in ION_SPEC.md (update if normative behavior changes)
- [ ] 2. Lexer - new tokens/keywords in src/lexer/mod.rs (if needed)
- [ ] 3. AST - add/modify nodes in src/ast/mod.rs
- [ ] 4. Parser - parse new syntax in src/parser/mod.rs
- [ ] 5. Type checker - typing, ownership, Send in `src/tc/` (`mod.rs` plus `ownership.rs`, `builtins.rs`, `types.rs` as needed)
- [ ] 6. IR - lowering in src/ir/mod.rs
- [ ] 7. Codegen - C output in `src/cgen/` (`mod.rs`, `types.rs`, `builtins.rs`, `drop.rs`) (+ runtime/ if new runtime support)
- [ ] 8. Tests - integration test in tests/ (see ion-integration-tests skill)
- [ ] 9. LSP - update src/lsp/ if diagnostics/hover/completion affected
- [ ] 10. cargo test && cargo build --release && tests/test_runner.sh
```

For new syntax: **lexer → AST → parser** (tokens must exist before the parser can consume them; AST shapes should be defined before parser returns them). Skip stages that don't apply (e.g., parser-only bugfix may not touch IR).

**Note:** This skill auto-triggers on `src/**`, `runtime/**`, `stdlib/**`, and `ION_SPEC.md`. For `tests/` edits, also follow `ion-integration-tests`.

## Stage guidance

### Spec (`ION_SPEC.md`)

- Section 3: grammar (EBNF)
- Section 4-7: types, ownership, memory, concurrency
- Section 10.2: update known limitations if fixing tooling gaps

### AST types before parser logic

Define AST variants in `src/ast/mod.rs` before writing parser branches that construct them. The lexer still comes first when new tokens or keywords are involved.

### Parser (`src/parser/mod.rs`)

- Large file - locate similar constructs and mirror style
- `ParseError` for failures; avoid panics on user input
- Method calls are `Expr::MethodCall` (not desugared to `Expr::Call` at parse time) - handle both `Call` and `MethodCall` in tc, IR, and cgen
- Import **syntax** is parsed here; import **resolution** is in `src/compiler/mod.rs` (see below)

### Module compiler (`src/compiler/mod.rs`)

Touch this for imports, multi-file mode, or qualified names - not only when editing `import` syntax in the parser:

- `parse_module` - recursive lex+parse per file, cycle detection, module cache
- `register_imports` / `load_imports` / `ModuleExports` - builds `alias::item` maps for tc (CLI and LSP)
- `merge_modules` - single-file codegen merges imported ASTs into one program
- `resolve_import_path` - relative path resolution from importing file

### Type checker (`src/tc/`)

See `mod.rs`, `ownership.rs`, `builtins.rs`, `types.rs`. LSP uses `check_program_collecting` for multiple diagnostics; CLI uses `check_program` (first error only).

Critical checks to preserve. Patterns below match **CLI stderr** (`Type check error: UseAfterMove { ... }` via `{:?}`). The LSP formats the same errors differently (e.g. "Use after move: x") - see `ion-lsp-vscode` skill; do not use CLI grep patterns to validate LSP diagnostics.

| Check | CLI stderr pattern |
|-------|----------------|
| Use after move | `UseAfterMove` |
| Reference escape | `ReferenceEscape` |
| Non-Send in channel/spawn | messages containing `Send` |
| `if`/`while` non-bool | bool condition errors |
| Module visibility | `Cannot access non-public` |
| `unsafe` for extern | `must be inside an unsafe block` |

Add new `TypeCheckError` variants only when existing ones can't express the failure clearly.

### IR and codegen

- `IRBuilder::build` - keep lowering deterministic
- `cgen` - generated C must compile with `gcc` + `runtime/ion_runtime.c` + `-lpthread`
- Multi-file: test with `--mode multi` if imports or visibility involved

### Runtime (`runtime/`)

New builtins (channels, Vec, String, spawn) often need C runtime helpers. Keep headers in `ion_runtime.h` consistent with cgen calls.

## Debugging a failing stage

```bash
# See parser/tc errors
./target/release/ion-compiler path/to/test.ion

# Inspect generated C
./target/release/ion-compiler tests/test_foo.ion && cat tests/test_foo.c

# Single Rust test
cargo test lexer::
```

## Anti-patterns

- Implementing syntax only in codegen without tc validation
- Breaking single-file merge path while fixing multi-file (test both)
- Weakening no-escape or Send rules to make a test pass
- Large parser refactors bundled with feature work
- Forgetting to register new keywords in lexer **and** LSP keyword lists (`src/lsp/server.rs`, `ion-vscode/syntaxes/ion.tmLanguage.json`) if applicable; rebuild `ion-lsp` after compiler changes

## Examples

**New expression form:** lexer token (if needed) → AST variant → parser branch → `tc` typing → IR expr → cgen emit → `test_*.ion`

**New type rule:** `tc` only (+ spec) → negative test with `test_error` pattern

**New builtin:** tc type signature → IR/cgen → possibly `runtime/ion_runtime.c` → positive test
