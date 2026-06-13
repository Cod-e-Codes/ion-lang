# Per-Stage Checklist

## Lexer (`src/lexer/mod.rs`)

- [ ] New keyword added to keyword map (ION_SPEC §2.2.2 lists reserved words)
- [ ] Token variant in public token enum
- [ ] `tokenize()` handles edge cases (strings, escapes, floats)
- [ ] Unit test in `#[cfg(test)]` module

## AST (`src/ast/mod.rs`)

- [ ] Types defined before parser branches that construct them (lexer first if new tokens needed)
- [ ] `Clone`/`Debug` derives consistent with siblings
- [ ] Span info preserved if used by LSP (check existing nodes)

## Parser (`src/parser/mod.rs`)

- [ ] Grammar matches ION_SPEC §3
- [ ] Recovery/consume patterns match neighboring rules
- [ ] `parse_type`, `parse_expr`, `parse_stmt` hierarchy respected
- [ ] Imports: `import "file.ion" as name;` syntax
- [ ] If imports/exports/multi-file behavior changes: update `src/compiler/mod.rs` (`parse_module`, `ModuleExports`, `merge_modules`)

## Type checker (`src/tc/`)

- `mod.rs` - `check_program`, stmt/expr checking
- `ownership.rs` - moves, `is_copy_type`
- `builtins.rs` - builtin calls
- `types.rs` - `types_equal`, `type_to_string`, `resolve_type_name`

- `for` loops desugar to `While` in TC; `loop_depth` is set via the synthetic `WhileStmt`, not directly in the `For` arm
- [ ] Move tracking updated for new bindings/expr forms
- [ ] Borrow rules for new lvalue/rvalue paths
- [ ] Generic instantiation consistent with existing generics
- [ ] Method calls resolve to known builtin or user fn

## IR (`src/ir/mod.rs`)

- [ ] New `Inst` or expr variants
- [ ] `IRBuilder` covers all AST paths for the feature

## Codegen (`src/cgen/`)

- `mod.rs` - main emission
- `types.rs` - C type mangling
- `builtins.rs` - builtin call codegen
- `drop.rs` - drop/defer paths

- [ ] Valid C99-ish output
- [ ] Includes and forward decls for multi-file headers
- [ ] Drop/defer/spawn/channel paths emit correct runtime calls

## LSP (`src/lsp/`)

- [ ] Parser errors surface in diagnostics
- [ ] New keywords in completion list if maintained manually
- [ ] Rebuild: `cargo build --release --bin ion-lsp`
