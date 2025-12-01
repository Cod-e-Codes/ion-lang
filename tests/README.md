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

## Test Format

Each test is an Ion source file (`.ion`) that should:
1. Compile successfully to C
2. Generate C that compiles without errors
3. Produce an executable that runs and returns the expected exit code

## Test Categories

### Phase 0 Tests
- `test_basic.ion` - Basic function and return
- `test_arithmetic.ion` - Arithmetic operations
- `test_move_basic.ion` - Move semantics
- `test_ref_valid.ion` - Valid reference usage
- `test_send_basic.ion` - Channel send/recv
- `test_send.ion` - Channel send operations
- `test_defer_basic.ion` - Defer statements
- `test_channel_basic.ion` - Channel operations
- `test_spawn_basic.ion` - Spawn statements
- `test_if_basic.ion` - If statements with else
- `test_if_no_else.ion` - If statements without else
- `test_struct_basic.ion` - Struct declarations

### Phase 1 Tests
- `test_enum_basic.ion` - Enum declarations and literals
- `test_enum_generic.ion` - Generic enum types
- `test_match_basic.ion` - Pattern matching
- `test_match_pattern_bindings.ion` - Pattern matching with bindings
- `test_match_complex.ion` - Complex pattern matching scenarios
- `test_while_basic.ion` - While loops
- `test_call_basic.ion` - Function calls
- `test_string_basic.ion` - String literals
- `test_string_from.ion` - String::from() function
- `test_string_new.ion` - String::new() function
- `test_string_push_str.ion` - String::push_str() function
- `test_generic_types.ion` - Generic type system
- `test_generic_struct.ion` - Generic struct types
- `test_box_basic.ion` - Box<T> heap allocation
- `test_box_ops.ion` - Box operations (new, unwrap)
- `test_vec_basic.ion` - Vec<T> dynamic arrays
- `test_vec_new.ion` - Vec::new() function
- `test_vec_push_pop.ion` - Vec push and pop operations
- `test_vec_get_set.ion` - Vec get and set operations
- `test_vec_capacity.ion` - Vec capacity management

### Phase 2 Tests
- `test_module_basic.ion` - Module system and imports
- `test_module_visibility.ion` - Module visibility control (negative test)
- `test_ffi_basic.ion` - Foreign Function Interface (FFI)

### Phase 3 Tests
- `test_array_basic.ion` - Fixed-size arrays
- `test_array_literal.ion` - Array literals
- `test_array_indexing.ion` - Array indexing operations
- `test_slice_basic.ion` - Dynamically sized slices
- `test_slice_indexing.ion` - Slice indexing operations
- `test_array_to_slice_coercion.ion` - Array-to-slice coercion
- `test_unsafe_basic.ion` - Unsafe blocks
- `test_unsafe_extern_required.ion` - Unsafe requirement for extern calls (negative test)
- `test_multifile.ion` - Multi-file compilation

### Phase 4 Tests
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

### Phase 5 Tests
- `test_method_call_basic.ion` - Basic method call syntax (`vec.push()`, `s.len()`)
- `test_method_call_mut.ion` - Mutable receiver method calls (`vec.pop()`, `vec.set()`)
- `test_method_call_generic.ion` - Generic method calls with type inference
- `test_method_call_chaining.ion` - Chained method calls (`vec.push().len()`)

### Phase 6 Tests
- `test_channel_split.ion` - Split Channel API (`Sender<T>`, `Receiver<T>` types)
- `test_enum_struct_variant.ion` - Struct-style enum variants with named fields
- `test_for_loop.ion` - `for...in` loop syntax with Vec iteration

### Phase 7 Tests
- `test_escape_sequences.ion` - Complete escape sequence support (`\r`, `\t`, `\0`, etc.)
- `test_array_init.ion` - Array initialization syntax (`[value; count]`)
- `test_bitwise_ops.ion` - Bitwise operators (`&`, `|`, `^`, `<<`, `>>`)

### Phase 8 Tests
- `test_comparison_operators.ion` - Full comparison operators (`<=`, `>=`)
- `test_type_cast.ion` - Type casting with `as` keyword
- `test_array_assignment.ion` - Array element assignment (`arr[i] = value`)

### Negative Tests (Error Cases)
- `test_move_error.ion` - Use-after-move errors
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
- `test_if_bool_required.ion` - Boolean requirement for if conditions (Phase 4)

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

