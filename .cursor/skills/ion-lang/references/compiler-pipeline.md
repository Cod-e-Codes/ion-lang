# Compiler Pipeline

## Entry points

| Binary | Source | Role |
|--------|--------|------|
| `ion-compiler` | `src/main.rs` | CLI: parse → type-check → codegen |
| `ion-lsp` | `src/bin/ion-lsp.rs` | Language server for editor diagnostics |

## Stage responsibilities

### 1. Module resolution (`src/compiler/mod.rs`)

- Entry point: `Compiler::parse_module` - resolves imports, detects cycles, then **lexes and parses each file**
- Resolves `import "path.ion" as alias;` relative to importing file
- Caches parsed `ast::Program` per module; builds `ModuleExports` for qualified names (`alias::fn`)

### 2. Lexer (`src/lexer/mod.rs`)

- Tokenizes UTF-8 source
- Keywords, literals, operators, comments
- Has unit tests in-module

### 3. Parser (`src/parser/mod.rs`)

- Builds `ast::Program` from tokens
- `Ident::Ident(` disambiguation: import aliases and builtins (`Box`, `Vec`, `String`) become `Expr::Call`; local enum names stay `Expr::EnumLit`
- Largest source file - follow existing parsing helpers and error style (`ParseError`)

### 4. AST (`src/ast/mod.rs`)

- Node definitions for items, expressions, types, patterns
- Extend AST types before adding parser branches that build them (lexer first if new tokens are needed)

### 5. Type checker (`src/tc/`)

| File | Role |
|------|------|
| `mod.rs` | `TypeChecker`, `check_program`, `check_program_collecting`, main stmt/expr checking |
| `ownership.rs` | `check_expr_for_moves`, `is_copy_type` |
| `builtins.rs` | `check_builtin_call` |
| `types.rs` | `types_equal`, `type_to_string`, `resolve_type_name`, method type helpers |

- Ownership, moves, borrows, no-escape rule
- `Send` checking for channels and `spawn`
- Module visibility (`pub`), generics, method resolution
- Errors via `TypeCheckError` enum
- CLI: `check_program` returns first error; LSP: `check_program_collecting` gathers multiple independent errors

### 6. IR (`src/ir/mod.rs`)

- `IRBuilder::build` lowers AST to IR for codegen
- Used in both single-file (merged program) and multi-file modes

### 7. C codegen (`src/cgen/`)

| File | Role |
|------|------|
| `mod.rs` | `Codegen`, `generate`, stmt/expr emission |
| `types.rs` | `type_to_c_impl`, mangling, alias resolution |
| `builtins.rs` | `generate_builtin_call` |
| `drop.rs` | `emit_drop`, `type_needs_drop` |

- `Codegen::generate` - single merged `.c`
- `generate_module_source` / `generate_module_header` - multi-file `.c`/`.h`
- Runtime support in `runtime/ion_runtime.c` and `runtime/ion_runtime.h`

## Single vs multi-file (`src/main.rs`)

**Single (default):** merge all imported modules → one IR → one `.c` next to input.

**Multi (`--mode multi`):** per-module IR → `.c` + `.h` each → compile objects → link with runtime.

## LSP path (`src/lsp/server.rs`, `src/lsp/util.rs`)

Differs from the CLI for the **current file**: lexer → parser on buffer text (not reading the file from disk). Then `load_imports` runs `parse_module` per import on disk (per-import diagnostics; partial exports on failure), then `check_program_collecting_with_source` type-checks the merged program while seeding LSP symbols from the buffer AST only. Expression-level LSP data (types, references) is recorded only for functions in the open file so merged stdlib spans do not collide by line/column. See `ion-lsp-vscode` skill.
