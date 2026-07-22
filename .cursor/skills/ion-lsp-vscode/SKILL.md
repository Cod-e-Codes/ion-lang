---
name: ion-lsp-vscode
description: >-
  Develop the Ion LSP server and VS Code/Cursor extension - diagnostics, hover,
  completion, syntax highlighting. Use when working on ion-lsp, tower-lsp, LSP
  protocol, extension packaging, ion-vscode, TextMate grammar, or IDE features for
  Ion. Also use when fixing editor diagnostics, rebuilding the .vsix, or
  configuring ion.lspPath in workspace settings.
paths:
  - ion-vscode/**
  - src/lsp/**
  - src/bin/ion-lsp.rs
---

# Ion LSP & VS Code Extension

## Components

| Piece | Location |
|-------|----------|
| LSP server | `src/lsp/server.rs`, `src/lsp/util.rs`, `src/bin/ion-lsp.rs` |
| VS Code extension | `ion-vscode/` (TypeScript + TextMate grammar) |
| Compiler reuse | `lexer`, `parser`, `tc` on buffer; `load_imports` → `parse_module` for imports on disk |

## Build LSP

```bash
cargo build --release --bin ion-lsp
```

Binary: `target/release/ion-lsp` (`.exe` on Windows).

Stop the running LSP before rebuild if Windows reports "Access is denied".

## Build & install extension

```bash
cd ion-vscode
npm install
npm run compile
npx @vscode/vsce package --allow-missing-repository
```

Install:

```bash
cursor --install-extension ion-language-0.1.1.vsix
# Version comes from ion-vscode/package.json - adjust filename after `vsce package`
# or: code --install-extension ion-language-0.1.1.vsix
```

Workspace setting `.vscode/settings.json`:

```json
{
  "ion.lspPath": "${workspaceFolder}/target/release/ion-lsp.exe"
}
```

Adjust path for OS (no `.exe` on Linux/macOS).

## LSP architecture

`src/lsp/server.rs` implements `tower_lsp::LanguageServer`. In `check_file`:

1. On `did_open` / `did_change` - **lexer → parser** on the in-memory buffer (unsaved edits included; does not re-read the file from disk)
2. **`Compiler::load_imports`** - per-import `parse_module` on disk with `build::discover_import_config` (same stdlib search order as `ion-build`, including walk-up `stdlib/` and install-relative paths); failed imports publish diagnostics at the `import` span while successful imports still register exports
3. **Type-check** - `tc::TypeChecker::check_program_collecting_with_source` on merged program, symbols seeded from buffer AST (`check_program_collecting` for unit tests)
4. Publish diagnostics from lexer, parser, import resolution, or type-check errors
5. **Hover** - expression types, symbol docs, builtin signatures
6. **Completion** - prefix-filtered; context-aware for `alias::` and `Type.` / `expr.` (`int`, `i8`-`i64`, `u8`-`u64`, `uint` expose `MIN`/`MAX` via `BUILTIN_TYPE_MEMBERS` in `util.rs`)
7. **Go to definition** - variables, calls, methods, fields, variants, type aliases; cross-file via `module_paths`
8. **References**, **document symbols**, **signature help**, **semantic tokens**
9. **`did_change_watched_files`** - re-check open files whose import dependencies changed

Known limitations: built-in methods have signature hover but no go-to-definition target (ION_SPEC §10.2); type names in annotations have no goto (§10.3; no spans on `Type` nodes).

### CLI vs LSP error text

The CLI and `ion-build` print `Type check failed with N error(s):` followed by numbered lines from `tc::format_type_errors`. The LSP uses `check_program_collecting` and may show several type-check diagnostics in one publish (e.g. one per function). LSP formats errors as human-readable strings (e.g. "Use after move: x"). Do not use CLI grep patterns from `ion-integration-tests` to validate LSP diagnostics.

## Extension files

| File | Role |
|------|------|
| `ion-vscode/src/extension.ts` | Spawns LSP process, client config |
| `ion-vscode/syntaxes/ion.tmLanguage.json` | Syntax highlighting (keywords include `break`, `continue`) |
| `ion-vscode/language-configuration.json` | Brackets, comments |
| `ion-vscode/package.json` | Extension manifest, `ion.lspPath` setting |

## When compiler changes affect LSP

Parser or type checker changes often need no LSP code if error types already map to strings. Update LSP when adding:

- New keywords or builtins (`src/lsp/util.rs` lists)
- New diagnostic categories or positions
- Import resolution behavior
- New symbols to record in `tc::LspInfo`

After LSP changes:

```bash
cargo build --release --bin ion-lsp
# Reload VS Code/Cursor window to pick up new binary
```

After extension TS changes:

```bash
cd ion-vscode && npm run compile
```

Repackage `.vsix` only when shipping extension updates.

## Debugging

- LSP logs: check Cursor/VS Code Output panel → "Ion Language Server"
- Compare CLI vs LSP: run `ion-compiler` on same file if diagnostics differ
- Ensure `ion.lspPath` points at the binary you just built

## Anti-patterns

- Duplicating parse/type logic in extension - reuse Rust compiler crate
- Editing grammar for semantics that belong in parser/tc
- Forgetting to rebuild both `ion-lsp` and reload window after server changes
