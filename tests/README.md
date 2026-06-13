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
- `test_send_basic.ion` - Send/Send smoke test
- `test_defer_basic.ion` - Defer statements
- `test_defer_block.ion` - Block-scoped defer
- `test_scope_drop_block.ion` - Automatic Vec drop at block exit
- `test_struct_field_drop.ion` - Struct and enum field drops at block exit (nested String fields, enum payload)
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

### Modules and FFI
- `test_module_basic.ion` - Module system and imports
- `test_module_visibility.ion` - Module visibility control (negative test)
- `test_ffi_basic.ion` - Foreign Function Interface (FFI)

### Arrays, slices, and unsafe
- `test_array_basic.ion` - Fixed-size arrays
- `test_array_literal.ion` - Array literals
- `test_array_indexing.ion` - Array indexing operations
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
- `test_loop_basic.ion` - Infinite `loop { }` with `break`

### Match expression types
- `test_match_result_type.ion` - `match` infers non-`int` result type (`bool` via `-> bool` helper, exit 88)
- `test_match_arm_type_mismatch.ion` - Mismatched arm result types (negative)

### if/else ownership merge
- `test_if_else_move_ok.ion` - Move in diverging branch; use after `if` (exit 60)
- `test_if_else_move_error.ion` - Move in fall-through branch; use after `if` (negative)

### Function types
- `test_fn_type_basic.ion` - Store named function in `fn(int) -> int` variable; call through pointer (exit 77)
- `test_fn_type_mismatch.ion` - Function signature mismatch when coercing to fn type (negative)

- `test_io_print_str.ion` - Safe I/O library: `print_str()` function
- `test_io_print.ion` - Safe I/O library: `print()` function for String
- `test_io_println.ion` - Safe I/O library: `println()` function for String

### Iteration, Guards, and Formatting Tests
- `test_for_array.ion` - `for...in` over fixed-size arrays
- `test_for_string.ion` - `for...in` over `String` (byte iteration)
- `test_match_guard.ion` - Match arms with `if` guards
- `test_generic_field_access.ion` - Field access on generic struct values
- `test_io_print_int.ion` - `io::print_int` decimal output
- `test_fmt_int_to_string.ion` - `fmt::int_to_string` conversion
- `test_fmt_println_int.ion` - `fmt::println_int` via stdlib merge; codegen uses `io_print_int` in `fmt_print_int`

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

1. Create a new `.ion` file in this directory
2. Add a test case to `test_runner.sh`:
   ```bash
   test_file "test_name.ion" expected_exit_code
   ```
   
   For error tests (should fail to compile):
   ```bash
   test_error "test_name.ion" "expected_error_pattern"
   ```

## Environment Variables

- `COMPILER`: Path to the ion-compiler binary (default: `../target/release/ion-compiler`)
- `CC`: C compiler to use (default: `gcc`)

Example:
```bash
COMPILER=../target/debug/ion-compiler CC=clang ./test_runner.sh
```

