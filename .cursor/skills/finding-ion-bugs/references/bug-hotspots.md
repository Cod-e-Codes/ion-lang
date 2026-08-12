# Ion bug hotspots

## Type checker (`src/tc/`)

- **Use-after-move**: moved values in branches, loop bodies, struct field partial move
- **Reference escape**: `&` stored in struct, returned, sent on channel, captured by `spawn`
- **Send**: non-Send types on channels or in spawn closures; `Box` and channel element variance
- **Recursive types**: `is_reference_containing` / `is_send` / `is_eq_type` / `type_needs_drop` need a visiting set; without one, `Box`/`Vec`/`Option<Box<…>>` self-reference stack-overflows at decl time. Representability (`InfiniteSize`) treats only `Box`/`Vec`/`RawPtr` as size boundaries — `Option<Node>` is still infinite size; `Option<Box<Node>>` is not. Do not “stop at Box” inside the no-escape walker or `Box<&T>` silently passes.
- **Match on `&GenericEnum`**: peel `Ref` before building the type-param subst map in `add_pattern_bindings` (see `test_match_ref_generic_enum_arith.ion`); bare `if let Type::Generic` misses `Ref { Generic { … } }` and leaves bindings as `&T`
- **`resolve_type_name` and `&Enum` params**: must recurse into `Ref` so `&Flag` becomes `Ref { Enum }` (parser stores enum names as `Struct`); otherwise calls get `expected &Flag, got &Flag` from Struct vs Enum mismatch
- **MethodCall vs Call**: parser emits `Expr::MethodCall`; tc/IR/cgen must handle both
- **Module visibility**: `pub` vs private across `import`

CLI errors use `TypeCheckError` Debug form (`UseAfterMove { ... }`). LSP reformats them in `src/lsp/`.

## Codegen (`src/cgen/`)

- Drop order and `ion_drop_*` for moved fields
- **Struct field move-out**: owned fields null after partial move on the next statement (`board.items = NULL`; deferred when the move is a call argument)
- **Vec::push lvalues**: struct variables and field paths use `&item`, not compound literal (`vec_push_struct_var_uses_address_of_lvalue`)
- **Enum emission order**: non-generic enums before structs in single-file C output
- **Tuple mangle**: `tuple_type_name` sanitizes `*` and brackets when names include `Vec` types
- **Match scrutinee move-out**: pattern payload bindings null `match_val_N.data.variant_*` fields when ownership transfers (`statement_match_payload_move_neutralizes_scrutinee`); whole-enum binding arms clear active variant payloads via `emit_match_scrutinee_whole_enum_moved_out` (`whole_enum_binding_neutralizes_scrutinee_payloads`). IR infers `enum_type` from the scrutinee when arms use binding/wildcard only (`infer_match_enum_name`).
- **Return unwind**: all function exits use `emit_function_return` (`ret_val`, `scope_emit_return_unwind`, `goto epilogue`), including diverging `return` inside rvalue `match` arms (`rvalue_match_divergent_return_unwinds_owned`). Value-producing rvalue arms still assign and `break` from the `switch`.
- `Box`, `Vec`, `String` layout vs `runtime/ion_runtime.h`
- **`Box::new` let initializer**: IR must type `StructLit` as `Struct(name)`, not default `Int`; cgen must prefer let `type_context` over a wrong Call `return_type` (`type_context.or(return_type)`, same as `Vec::new`). Nested `Box::new(var)` worked via `var_types`; direct `Box::new(Node { ... })` did not (see `test_box_new_struct_let*.ion`).
- **`Box::unwrap`**: copy `T` out, then `ion_box_free` the box pointer; do not drop `T` (nested heap in the payload would double-free). Move-mark the argument so scope-exit drop does not free again (`test_box_unwrap_same_scope.ion`). A `run` exit code cannot catch the leak; Linux CI leak-sanitizer step uses `detect_leaks=1` on unwrap tests.
- **`String::len`**: null-check the `String*` value, not `&local` (`-Waddress` under CI `-Werror`)
- **String literal call args**: parameters typed `String` need `ion_string_from_literal` at the call site, not only on `let s: String = "…"` (see `test_string_call_arg_literal.ion`)
- **Reborrowed field → `&Vec` / `&String` param**: non-copy `FieldAccess` through `&Struct` is already `&T` in Ion but loads as `T*` in C; user calls need `&(base->field)` so arity matches `T**` (see `test_ref_struct_field_to_ref_vec_param.ion`). Nested embedded paths use `.` after the first hop (`&(w->inner.data)`). Builtins keep bare field loads via `vec_ion_ptr_expr`.
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
