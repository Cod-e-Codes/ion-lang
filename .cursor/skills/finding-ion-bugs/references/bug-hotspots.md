# Ion bug hotspots

## Type checker (`src/tc/`)

- **Use-after-move**: moved values in branches, loop bodies, struct field partial move
- **Reference escape**: `&` stored in struct, returned, sent on channel, captured by `spawn`. Locals and params must also reject off-stack stores (`Box<&T>`, `Vec<&T>`); `is_reference_containing` on decls/returns is not enough (`test_box_ref_let_error.ion`). `Option<&T>` stack temporaries from `get_ref` stay legal.
- **Send**: non-Send types on channels or in spawn closures; `Box` and channel element variance
- **Recursive types**: `is_reference_containing` / `is_send` / `is_eq_type` / `type_needs_drop` need a visiting set; without one, `Box`/`Vec`/`Option<Box<…>>` self-reference stack-overflows at decl time. Representability (`InfiniteSize`) treats only `Box`/`Vec`/`RawPtr` as size boundaries — `Option<Node>` is still infinite size; `Option<Box<Node>>` is not. Do not “stop at Box” inside the no-escape walker or `Box<&T>` silently passes.
- **Generic enum `None`**: `Option::None` has no payload, so `T` is inferred only from `expr_expected` / return type. Unannotated `let empty = Option::None` must error with cannot-infer, not a later `Option<T>` vs `Option<Box<Node>>` mismatch (`test_option_none_unannotated_error.ion`). Same-expression `Node { next: Option::None }` is fine (`test_option_none_struct_field.ion`).
- **Match-arm result types**: `infer_block_result_type` reads recorded `TypeInfo` expr types plus control-flow shape (diverge vs value). It must not call `check_expr` again after `check_stmt` (`test_vec_get_putback_named.ion`).
- **Match on `&GenericEnum`**: peel `Ref` before building the type-param subst map in `add_pattern_bindings` (see `test_match_ref_generic_enum_arith.ion`); bare `if let Type::Generic` misses `Ref { Generic { … } }` and leaves bindings as `&T`
- **`resolve_type_name` and `&Enum` params**: must recurse into `Ref` so `&Flag` becomes `Ref { Enum }` (parser stores enum names as `Struct`); otherwise calls get `expected &Flag, got &Flag` from Struct vs Enum mismatch
- **`resolve_type_name` generic params**: must rewrite `Struct` to `Enum` inside `Generic` (and other composites). `Result<int, MyError>` vs `Result::Err(MyError::Bad)` otherwise prints `expected Result<int, MyError>, got Result<int, MyError>` (`test_result_custom_enum.ion`)
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
- **IR must use `TypeInfo`**: after a successful type-check, every lowered expression id has a canonical type. Missing id is a compiler bug, never a syntactic `int` fallback. One-off patches for this class shipped in 0.1.10 / 0.1.13 / 0.1.16; 0.1.17 makes the checker the source of truth (`let x = y`, `Box::new(StructLit)`, `Box::unwrap`, `Enum::Variant`). Tests: `test_unannotated_let_non_int.ion`, `test_box_new_struct_let*.ion`, `test_box_unwrap_struct_let.ion`, `test_enum_unannotated_let.ion`.
- **Generic match subst**: `substitute_types_in_expr` must substitute `Match.scrutinee_type` (not only the inner expr / `enum_type`). Leaving `Slot<T>` emits `Slot_T` (`examples/handle_table`).
- **`Box::unwrap`**: copy `T` out, then `ion_box_free` the box pointer; do not drop `T` (nested heap in the payload would double-free). Move-mark the argument so scope-exit drop does not free again (`test_box_unwrap_same_scope.ion`). A `run` exit code cannot catch the leak; Linux CI leak-sanitizer step uses `detect_leaks=1` on unwrap tests.
- **`String::len`**: null-check the `String*` value, not `&local` (`-Waddress` under CI `-Werror`)
- **`len` method routing**: `"len"` is a Vec, String, and Slice method. IR/cgen must classify the receiver first (`receiver_is_slice` / `receiver_is_string`) before the `vec_methods` table, or `s.len()` on `&[]T` becomes `Vec::len` (same class as the v0.1.1 `String::len` mis-route). `vec.len()` must still become `Vec::len` (`test_slice_len.ion`, `test_method_call_basic.ion`).
- **String literal call args**: parameters typed `String` need `ion_string_from_literal` at the call site, not only on `let s: String = "…"`. Pass compilation-wide `TypeInfo.function_params` into multi-file cgen (`println`, `io::println`, `io_println`) so imported callees see `String` (`test_string_call_arg_literal.ion`, `test_multi_fmt_io.ion`).
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
