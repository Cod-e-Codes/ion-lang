# Ion Compiler Tests

This directory contains test programs for the Ion compiler.

## Running Tests

Use the test harness script:

```bash
cd tests
chmod +x test_runner.sh
./test_runner.sh
```

Or from the project root:

```bash
cd tests && ./test_runner.sh
```

On Windows, use Git Bash, not WSL bash. WSL often cannot run `ion-compiler.exe`.

The harness defaults to `../target/release/ion-compiler`. Rebuild it after compiler changes:

```bash
cargo build --release
```

At startup the harness precompiles `runtime/ion_runtime.c` once to `.ion_test_runtime.o` (override with `RUNTIME_OBJ`) and links that object for every `run` test and `test_multifile`, instead of recompiling the runtime per test.

If the release build fails with "Access is denied", stop `ion-lsp` (or any running `ion-compiler` / `ion-lsp` process) and retry. You can also build only the compiler:

```bash
cargo build --release --bin ion-compiler
```

To run against the debug compiler instead:

```bash
COMPILER=../target/debug/ion-compiler.exe ./test_runner.sh
```

## Test Format

Each test is an Ion source file (`.ion`) that should:
1. Compile successfully to C
2. Generate C that compiles without errors
3. Produce an executable that runs and returns the expected exit code

## Test Categories

The test runner prints pass/fail counts when it finishes. Do not rely on hardcoded totals in documentation.

### Core language
- `test_basic.ion` - Basic function and return
- `test_arithmetic.ion` - Arithmetic operations
- `test_move_basic.ion` - Move semantics
- `test_ref_valid.ion` - Valid reference usage
- `test_double_mut_borrow_error.ion` - Second `&mut` on same variable (negative, `BorrowConflict`)
- `test_mut_shared_borrow_error.ion` - `&mut` while shared borrow active (negative)
- `test_move_while_borrowed_error.ion` - Move into call while `let r = &x` is active (negative)
- `test_copy_use_while_mut_borrowed_error.ion` - Copy-type read while `let r = &mut x` is active (negative)
- `test_copy_use_while_shared_borrowed_error.ion` - Copy-type read while `let r = &x` is active (negative)
- `test_assign_while_borrowed_error.ion` - Assignment while `let r = &x` is active (negative)
- `test_mut_borrow_block_ok.ion` - Mutable borrow ends with `if` branch scope (exit 62)
- `test_shared_borrow_ok.ion` - Multiple `&T` borrows allowed (exit 63)
- `test_nested_shared_borrow_ok.ion` - Outer shared borrow survives inner `if` scope (exit 64)
- `test_field_double_mut_borrow_error.ion` - Second `&mut` on disjoint fields of same owner (negative, `BorrowConflict`)
- `test_field_mut_borrow_blocks_owner_error.ion` - Field read while root owner is mut-borrowed (negative)
- `test_field_whole_mut_borrow_error.ion` - Whole-owner `&mut` while field `&mut` is active (negative)
- `test_field_shared_borrow_ok.ion` - Multiple shared field borrows on same owner (exit 65)
- `test_field_mut_borrow_scope_ok.ion` - Field mut borrow released by inner `if` scope (exit 66)
- `test_send_basic.ion` - Send/Send smoke test
- `test_defer_basic.ion` - Defer statements
- `test_defer_block.ion` - Block-scoped defer
- `test_scope_drop_block.ion` - Automatic Vec drop at block exit
- `test_struct_field_drop.ion` - Struct and enum field drops at block exit (nested String fields, enum payload)
- `test_struct_field_drop_vec.ion` - Struct field holding `Vec<int>` drop at block exit (exit 46)
- `test_struct_field_drop_box.ion` - Box field drop at block exit (exit 44)
- `test_struct_enum_empty_drop.ion` - Enum drop with Empty variant only (exit 45)
- `test_move_call_drop.ion` - No double-drop when String is moved into a function call
- `test_move_in_loop_ok.ion` - Borrowing a non-copy value across loop iterations (no move in body)
- `test_move_in_loop_copy.ion` - Fresh copy-type bindings each loop iteration (exit 30)
- `test_scope_drop_elif.ion` - Vec drop inside an else-if branch
- `test_channel_basic.ion` - Channel operations
- `test_spawn_basic.ion` - Spawn statements
- `test_spawn_channel.ion` - Cross-thread channel send/recv via spawn (channel handle drop at scope exit)
- `test_if_basic.ion` - If statements with else
- `test_if_no_else.ion` - If statements without else
- `test_if_elif.ion` - If with else-if chain
- `test_if_elif_no_else.ion` - Else-if without a final else
- `test_struct_basic.ion` - Struct declarations

### Enums, generics, and collections
- `test_enum_basic.ion` - Enum declarations and literals
- `test_enum_generic.ion` - Generic enum types
- `test_match_basic.ion` - Pattern matching
- `test_match_pattern_bindings.ion` - Pattern matching with bindings
- `test_match_complex.ion` - Complex pattern matching scenarios
- `test_while_basic.ion` - While loops
- `test_break_continue.ion` - `break` and `continue` in `while` and `for` loops
- `test_call_basic.ion` - Function calls
- `test_string_basic.ion` - String literals
- `test_string_from.ion` - String::from() function
- `test_string_new.ion` - String::new() function
- `test_string_push_str.ion` - String::push_str() function
- `test_string_push_byte.ion` - String::push_byte() function
- `test_string_eq.ion` - String `==` and `!=` value equality (exit 55)
- `test_generic_types.ion` - Generic type system
- `test_generic_struct.ion` - Generic struct types
- `test_box_basic.ion` - Box<T> heap allocation
- `test_box_ops.ion` - Box operations (new, unwrap)
- `test_vec_basic.ion` - Vec<T> dynamic arrays
- `test_vec_new.ion` - Vec::new() function
- `test_vec_push_pop.ion` - Vec push and pop operations
- `test_vec_get_set.ion` - Vec get and set operations
- `test_vec_capacity.ion` - Vec capacity management
- `test_vec_i32.ion` - `Vec<i32>` with annotated `Vec::new`, `i32` indices
- `test_vec_struct.ion` - `Vec` with struct elements, annotated `Vec::new`, and `for` iteration
- `test_vec_push_struct_var.ion` - `Vec::push` with a struct variable (address-of lvalue)
- `test_struct_field_move_vec.ion` - move a `Vec` out of a struct field without double-free
- `test_tuple_vec_int.ion` - tuple `(Vec<T>, int)` mangling and return
- `test_tuple_vec_int_epilogue.ion` - tuple return with loop body before epilogue `return`
- `test_tuple_fn_lit.ion` - fn literal returning a tuple; `ret_val` compound init
- `test_call_struct_field_move.ion` - struct field moved into a call argument without broken C

### Modules and FFI
- `test_module_basic.ion` - Module system and imports
- `test_module_visibility.ion` - Module visibility control (negative test)
- `test_ffi_basic.ion` - Foreign Function Interface (FFI)

### Arrays, slices, and unsafe
- `test_array_basic.ion` - Fixed-size arrays
- `test_array_literal.ion` - Array literals
- `test_array_indexing.ion` - Array indexing operations
- `test_index_i32.ion` - Array indexing with `i32` index variables
- `test_array_bounds_safe.ion` - Array bounds checking with valid indices (Safety Enhancement)
- `test_unsafe_array_indexing.ion` - Unsafe array indexing without bounds checking (Safety Enhancement)
- `test_slice_bounds_codegen.ion` - Slice bounds checking in generated C (codegen grep)
- `test_unsafe_slice_indexing.ion` - Unsafe slice indexing without bounds checking (codegen grep)
- `test_slice_bounds_panic.ion` - Slice out-of-bounds panic (harness: codegen grep only; manual run below)
- `test_slice_basic.ion` - Dynamically sized slices
- `test_slice_indexing.ion` - Slice indexing operations
- `test_array_to_slice_coercion.ion` - `&[T; N]` to `&[]T` at call sites (exit 10)
- `test_array_to_slice_let.ion` - `&[T; N]` to `&[]T` in let bindings (exit 11)
- `test_array_bounds_panic.ion` - Array out-of-bounds panic (harness: codegen grep only; manual run below)
- `test_unsafe_basic.ion` - Unsafe blocks
- `test_unsafe_extern_required.ion` - Unsafe requirement for extern calls (negative test)
- `test_multifile.ion` - Multi-file compilation
- `test_multi_struct.ion` - Multi-file module with private struct in library
- `test_multi_fmt_io.ion` - Multi-file link with both `fmt` and `io` stdlib modules

### Numeric types and aliases
- `test_bool_literal.ion` - Boolean literals (`true`, `false`)
- `test_bool_operations.ion` - Boolean type usage
- `test_bool_comparison.ion` - Comparison operators returning `bool`
- `test_if_bool_required.ion` - Negative test: `if` requires `bool` condition
- `test_float_literal.ion` - Floating-point literals (`.5`, `3.`, `1e9`, etc.)
- `test_float_arithmetic.ion` - Float arithmetic operations
- `test_float_promotion.ion` - Float type promotion rules
- `test_float_comparison.ion` - Float comparison operations
- `test_integer_types.ion` - All integer type declarations
- `test_integer_promotion.ion` - Integer type promotion rules
- `test_integer_signed_unsigned.ion` - Signed/unsigned integer mixing
- `test_type_alias_basic.ion` - Basic type aliases
- `test_type_alias_generic.ion` - Generic type aliases
- `test_type_alias_resolution.ion` - Type alias resolution in function signatures

### Method call syntax
- `test_method_call_basic.ion` - Basic method call syntax (`vec.push()`, `s.len()`)
- `test_method_call_mut.ion` - Mutable receiver method calls (`vec.pop()`, `vec.set()`)
- `test_method_call_generic.ion` - Generic method calls with type inference
- `test_method_call_chaining.ion` - Chained method calls (`vec.push().len()`)

### Split channels, struct variants, and for loops
- `test_channel_split.ion` - Split Channel API (`Sender<T>`, `Receiver<T>` types)
- `test_channel_string.ion` - `channel<String>` send/recv; IR recv uses `String` element type (exit 3)
- `test_channel_send_call_expr.ion` - `send(&tx, make())` with non-lvalue operand codegen (exit 7)
- `test_channel_send_field_call_expr.ion` - `send(&tx, make_pair().x)` temps field of call result (exit 11)
- `test_enum_struct_variant.ion` - Struct-style enum variants with named fields
- `test_for_loop.ion` - `for...in` loop syntax with Vec iteration

### Literals and bitwise operators
- `test_escape_sequences.ion` - Complete escape sequence support (`\r`, `\t`, `\0`, etc.)
- `test_array_init.ion` - Array initialization syntax (`[value; count]`)
- `test_bitwise_ops.ion` - Bitwise operators (`&`, `|`, `^`, `<<`, `>>`)

### Casting, comparison, and stdlib I/O
- `test_comparison_operators.ion` - Full comparison operators (`<=`, `>=`)
- `test_type_cast.ion` - Type casting with `as` keyword
- `test_array_assignment.ion` - Array element assignment (`arr[i] = value`)

### Ergonomics (literals, compound assign, loop)
- `test_hex_literals.ion` - Hex integer literals (`0xFF`)
- `test_bin_literals.ion` - Binary integer literals (`0b10101010`)
- `test_compound_assign.ion` - Compound assignment (`+=`)
- `test_for_compound_assign.ion` - `+=` inside a `for` loop body (exit 6)
- `test_loop_basic.ion` - Infinite `loop { }` with `break`

### Match expression types
- `test_match_result_type.ion` - `match` infers non-`int` result type (`bool` via `-> bool` helper, exit 88)
- `test_match_expr_rvalue.ion` - `match` as rvalue in `let` binding (exit 91)
- `test_match_arm_type_mismatch.ion` - Mismatched arm result types (negative)
- `test_match_arm_divergent_rvalue.ion` - Diverging arm mixed with value arm in rvalue `match` (negative)
- `test_match_arm_if_else_value_rvalue.ion` - `if`/`else` value branches unify in rvalue `match` (exit 80)
- `test_match_arm_if_else_mixed_rvalue.ion` - Mixed diverging and value paths within one rvalue arm (negative)

### if/else ownership merge
- `test_if_else_move_ok.ion` - Move in diverging branch; use after `if` (exit 60)
- `test_if_else_move_error.ion` - Move in fall-through branch; use after `if` (negative)

### Function types
- `test_fn_type_basic.ion` - Store named function in `fn(int) -> int` variable; call through pointer (exit 77)
- `test_fn_type_mismatch.ion` - Function signature mismatch when coercing to fn type (negative)
- `test_fn_literal_basic.ion` - Capture-free fn literal stored in `fn(int) -> int` and called (exit 12)
- `test_fn_literal_callback.ion` - Pass capture-free fn literal to `fn(int) -> int` parameter (exit 40)
- `test_fn_literal_return.ion` - Return capture-free fn literal from function (exit 6)
- `test_fn_literal_capture_error.ion` - Fn literal referencing outer binding (negative, `ClosureCapture`)
- `test_fn_literal_ref_capture_error.ion` - Fn literal referencing outer reference (negative, `ClosureCapture`)
- `test_doc_comments.ion` - Adjacent `//` doc comments attach to AST without affecting compile or runtime (exit 42)
- `test_tuple_basic.ion` - Tuple literals, `.0`/`.1` access, and destructuring (exit 81)

- `test_io_print_str.ion` - Safe I/O library: `print_str()` function
- `test_io_print.ion` - Safe I/O library: `print()` function for String
- `test_io_println.ion` - Safe I/O library: `println()` function for String

### Iteration, Guards, and Formatting Tests
- `test_for_array.ion` - `for...in` over fixed-size arrays
- `test_for_string.ion` - `for...in` over `String` (byte iteration)
- `test_match_guard.ion` - Match arms with `if` guards
- `test_generic_field_access.ion` - Field access on generic struct values
- `test_io_print_int.ion` - `io::print_int` decimal output
- `test_fmt_int_to_string.ion` - `fmt::int_to_string` decimal output (`0`, positive, negative, `int::MIN`)
- `test_fmt_println_int.ion` - `fmt::println_int` via stdlib merge; codegen uses `io_print_int` in `fmt_print_int`
- `test_fs_read.ion` - `fs::read_to_string_result` reads `fixtures/small.txt` (exit 80)

### `ion-build` smoke tests

The harness also runs `ion-build` (not via `test_expectations.tsv`):

- `build_hello/` - minimal `ion.toml` project; expects exit code 55
- `build_bad_main/` - invalid `main` path; expects `main file not found` on stderr

Set `ION_BUILD` to override the `ion-build` binary path (default `../target/release/ion-build`).

### Negative Tests (Error Cases)
- `test_move_error.ion` - Use-after-move errors
- `test_move_in_loop.ion` - Use-after-move when a non-copy value is moved inside a loop body
- `test_move_in_loop_for.ion` - Same rule for `for` loops (outer binding moved in body)
- `test_move_channel_error.ion` - Use-after-move on channel receivers
- `test_ref_return_error.ion` - Reference escape errors
- `test_ref_return_error2.ion` - Additional reference escape errors
- `test_channel_ref_error.ion` - Non-Send channel elements
- `test_send_ref_error.ion` - Non-Send send operations
- `test_spawn_ref_error.ion` - Non-Send spawn captures
- `test_spawn_borrow_error.ion` - Spawn capture while lasting borrow is active (negative)
- `test_spawn_move_error.ion` - Move errors in spawn blocks
- `test_struct_ref_error.ion` - Reference in struct fields
- `test_enum_ref_error.ion` - Reference in enum variants
- `test_module_visibility.ion` - Module visibility violations
- `test_unsafe_extern_required.ion` - Unsafe requirement for extern calls
- `test_if_bool_required.ion` - Boolean requirement for if conditions
- `test_break_continue_error.ion` - `break` outside of a loop (negative test)

### Manual panic tests (bounds)

`test_array_bounds_panic.ion` and `test_slice_bounds_panic.ion` call `ion_panic` and abort. The harness compiles them and greps generated C for the panic message; it does not run the binaries.

From `tests/` (Git Bash):

```bash
../target/release/ion-compiler test_array_bounds_panic.ion
gcc test_array_bounds_panic.c ../runtime/ion_runtime.c -o test_array_bounds_panic \
    -I. -I.. -I../runtime -lpthread -lws2_32
./test_array_bounds_panic
# Expect stderr: Ion panic: Array index out of bounds

../target/release/ion-compiler test_slice_bounds_panic.ion
gcc test_slice_bounds_panic.c ../runtime/ion_runtime.c -o test_slice_bounds_panic \
    -I. -I.. -I../runtime -lpthread -lws2_32
./test_slice_bounds_panic
# Expect stderr: Ion panic: Slice index out of bounds
```

## Adding Tests

1. Create a new `test_<feature>.ion` file in this directory
2. Add one line to `test_expectations.tsv` (tab-separated):

```
file	kind	exit	error_pattern	must_match	must_not_match
test_myfeature.ion	run	42
```

   Kinds: `run` (compile+run+exit code), `error` (compile must fail; `error_pattern` greps CLI stderr), `cgen` (`must_match` / optional `must_not_match` on generated `.c`).

   For `error` rows, a matching failure message is required; a wrong pattern is a harness failure (not a pass).

3. Document the test in this README under the appropriate category

Special cases (not in the manifest):

- `test_multifile.ion`: multi-file mode harness in `test_runner.sh`
- `test_array_bounds_panic.ion` / `test_slice_bounds_panic.ion`: codegen-only in manifest; runtime panic is manual (see below)

## Environment Variables

- `COMPILER`: Path to the ion-compiler binary (default: `../target/release/ion-compiler`)
- `ION_BUILD`: Path to the ion-build binary (default: `../target/release/ion-build`)
- `CC`: C compiler to use (default: `gcc`)
- `CFLAGS`: Extra C compiler flags for generated C and the precompiled runtime (default: empty). CI uses `-fsanitize=address,undefined` for sanitizer smoke and runs the full harness with `-Wall -Wextra -Werror` on Linux.
- `LDFLAGS`: Extra C linker flags for generated test executables (default: empty). Pair with `CFLAGS` for sanitizer runtime flags when needed.
- `RUNTIME_OBJ`: Path to the precompiled runtime object file (default: `.ion_test_runtime.o` in `tests/`). Rebuilt when `runtime/ion_runtime.c` is newer than the object.

Example:
```bash
COMPILER=../target/debug/ion-compiler ION_BUILD=../target/debug/ion-build CC=clang ./test_runner.sh
CFLAGS="-fsanitize=address,undefined -fno-omit-frame-pointer" LDFLAGS="-fsanitize=address,undefined" ./test_runner.sh
```

