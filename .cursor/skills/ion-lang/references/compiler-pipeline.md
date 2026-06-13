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

### 5. Type checker (`src/tc/mod.rs`)

- Ownership, moves, borrows, no-escape rule
- `Send` checking for channels and `spawn`
- Module visibility (`pub`), generics, method resolution
- Errors via `TypeCheckError` enum - match existing variants for consistency

### 6. IR (`src/ir/mod.rs`)

- `IRBuilder::build` lowers AST to IR for codegen
- Used in both single-file (merged program) and multi-file modes

### 7. C codegen (`src/cgen/mod.rs`)

- `Codegen::generate` - single merged `.c`
- `generate_module_source` / `generate_module_header` - multi-file `.c`/`.h`
- Runtime support in `runtime/ion_runtime.c` and `runtime/ion_runtime.h`

## Single vs multi-file (`src/main.rs`)

**Single (default):** merge all imported modules → one IR → one `.c` next to input.

**Multi (`--mode multi`):** per-module IR → `.c` + `.h` each → compile objects → link with runtime.

## LSP path (`src/lsp/server.rs`)

Differs from the CLI for the **current file**: lexer → parser on buffer text (not reading the file from disk). Then `register_imports` runs `parse_module` recursively for each imported file on disk to build `module_exports`, then the type checker runs on the buffer AST. See `ion-lsp-vscode` skill.
