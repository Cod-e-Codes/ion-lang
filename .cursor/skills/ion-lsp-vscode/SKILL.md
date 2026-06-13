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
| LSP server | `src/lsp/server.rs`, `src/bin/ion-lsp.rs` |
| VS Code extension | `ion-vscode/` (TypeScript + TextMate grammar) |
| Compiler reuse | `lexer`, `parser`, `tc` on buffer; `register_imports` → `parse_module` for imports on disk |

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
cursor --install-extension ion-language-0.1.0.vsix
# Version comes from ion-vscode/package.json - adjust filename after `vsce package`
# or: code --install-extension ion-language-0.1.0.vsix
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
2. **`Compiler::register_imports`** - for each import, **`parse_module` on disk** (full lex+parse of dependency tree) to populate `module_exports` for qualified names
3. **Type-check** - `tc::TypeChecker` on the buffer AST, with exports from step 2
4. Publish diagnostics from lexer, parser, or type-check errors
5. Hover - variable types at use sites and `let` binding identifiers; symbol docs at fn/struct/enum definitions
6. Completion - keywords, builtins, file symbols (no prefix filtering)
7. Go to definition - variables, function calls (`foo`, `mod::func`), user-defined methods; cross-file for imports

Known limitations (ION_SPEC §10.2): built-in methods (`Vec::push`, etc.) do not go to definition; completion has no prefix filtering.

### CLI vs LSP error text

The CLI prints `Type check error: UseAfterMove { ... }` (`{:?}` on `TypeCheckError`). The LSP formats the same errors as human-readable strings (e.g. "Use after move: x", "Reference escape: ..."). Do not use CLI grep patterns from `ion-integration-tests` to validate LSP diagnostics - check the editor or `src/lsp/server.rs` formatting.

## Extension files

| File | Role |
|------|------|
| `ion-vscode/src/extension.ts` | Spawns LSP process, client config |
| `ion-vscode/syntaxes/ion.tmLanguage.json` | Syntax highlighting (keywords include `break`, `continue`) |
| `ion-vscode/language-configuration.json` | Brackets, comments |
| `ion-vscode/package.json` | Extension manifest, `ion.lspPath` setting |

## When compiler changes affect LSP

Parser or type checker changes often need no LSP code if error types already map to strings. Update LSP when adding:

- New keywords (completion list)
- New builtins (completion/hover)
- New diagnostic categories or positions
- Import resolution behavior

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
