# Ion bug hotspots

## Type checker (`src/tc/`)

- **Use-after-move**: moved values in branches, loop bodies, struct field partial move
- **Reference escape**: `&` stored in struct, returned, sent on channel, captured by `spawn`
- **Send**: non-Send types on channels or in spawn closures; `Box` and channel element variance
- **MethodCall vs Call**: parser emits `Expr::MethodCall`; tc/IR/cgen must handle both
- **Module visibility**: `pub` vs private across `import`

CLI errors use `TypeCheckError` Debug form (`UseAfterMove { ... }`). LSP reformats them in `src/lsp/`.

## Codegen (`src/cgen/`)

- Drop order and `ion_drop_*` for moved fields
- **Match scrutinee move-out**: pattern payload bindings null `match_val_N.data.variant_*` fields when ownership transfers (`statement_match_payload_move_neutralizes_scrutinee`); whole-enum binding arms clear active variant payloads via `emit_match_scrutinee_whole_enum_moved_out` (`whole_enum_binding_neutralizes_scrutinee_payloads`). IR infers `enum_type` from the scrutinee when arms use binding/wildcard only (`infer_match_enum_name`).
- **Return unwind**: all function exits use `emit_function_return` (`ret_val`, `scope_emit_return_unwind`, `goto epilogue`), including diverging `return` inside rvalue `match` arms (`rvalue_match_divergent_return_unwinds_owned`). Value-producing rvalue arms still assign and `break` from the `switch`.
- `Box`, `Vec`, `String` layout vs `runtime/ion_runtime.h`
- Single-file merge (`merge_modules`) vs `--mode multi` divergences
- `extern "C"` calls only inside `unsafe` blocks in source; cgen must not strip guards

## Parser / compiler

- `register_imports` (LSP) vs full `parse_module` (CLI) import resolution
- Import cycles and duplicate symbol registration
- Keyword additions: lexer + parser + `src/lsp/util.rs` `KEYWORDS` + TextMate grammar

## Integration harness

- TSV empty columns: use manifest awk parsing rules; bash `read` collapses tabs
- `cgen` `must_match` is fixed-string grep; regex only in `must_not_match`
- Panic tests: some are codegen-only rows; runtime panic may need manual run

## Runtime

- Channel send/recv pairing, closed channel behavior
- `spawn` thread lifecycle and stack size
- Windows: `-lws2_32` for socket examples; pthread via MinGW
