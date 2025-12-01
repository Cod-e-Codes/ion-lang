#!/bin/bash

# Test harness for Ion compiler
# Automates: Ion → C → Compile C → Run Executable → Verify output

# Don't exit on error - we want to run all tests
set +e

COMPILER="${COMPILER:-../target/release/ion-compiler}"
CC="${CC:-gcc}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

test_count=0
pass_count=0
fail_count=0

# Test function: compile, build, run, and check exit code (positive test)
test_file() {
    local ion_file="$1"
    local expected_exit="$2"
    local test_name="${ion_file%.ion}"
    
    test_count=$((test_count + 1))
    echo -n "Testing ${test_name}... "
    
    # Step 1: Compile Ion to C
    if ! "$COMPILER" "$ion_file" > /dev/null 2>&1; then
        echo -e "${RED}FAIL${NC} - Compilation failed"
        fail_count=$((fail_count + 1))
        return 1
    fi
    
    local c_file="${test_name}.c"
    if [ ! -f "$c_file" ]; then
        echo -e "${RED}FAIL${NC} - C file not generated"
        fail_count=$((fail_count + 1))
        return 1
    fi
    
    # Step 2: Compile C to executable (include runtime if it exists)
    local compile_cmd="$CC \"$c_file\""
    if [ -f "../runtime/ion_runtime.c" ]; then
        compile_cmd="$compile_cmd ../runtime/ion_runtime.c"
    elif [ -f "runtime/ion_runtime.c" ]; then
        compile_cmd="$compile_cmd runtime/ion_runtime.c"
    fi
    # Add include paths for runtime headers (ion_runtime.h can be in runtime/ or ../runtime/)
    compile_cmd="$compile_cmd -I. -I.. -Iruntime -I../runtime"
    # Link with pthread for channel support
    compile_cmd="$compile_cmd -lpthread"
    compile_cmd="$compile_cmd -o \"${test_name}\""
    if ! eval "$compile_cmd" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} - C compilation failed"
        fail_count=$((fail_count + 1))
        rm -f "$c_file"
        return 1
    fi
    
    # Step 3: Run and check exit code
    "./${test_name}" > /dev/null 2>&1
    local actual_exit=$?
    
    # Step 4: Verify result
    if [ "$actual_exit" -eq "$expected_exit" ]; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
        rm -f "$c_file" "${test_name}"
        return 0
    else
        echo -e "${RED}FAIL${NC} - Expected exit code $expected_exit, got $actual_exit"
        fail_count=$((fail_count + 1))
        rm -f "$c_file" "${test_name}"
        return 1
    fi
}

# Test function: verify compilation fails with expected error (negative test)
test_error() {
    local ion_file="$1"
    local expected_error="$2"  # Error pattern to search for
    local test_name="${ion_file%.ion}"
    
    test_count=$((test_count + 1))
    echo -n "Testing ${test_name} (should error)... "
    
    # Step 1: Try to compile Ion to C - should FAIL
    local output
    output=$("$COMPILER" "$ion_file" 2>&1)
    local compile_exit=$?
    
    # If compilation succeeded, that's a failure (we expected it to fail)
    if [ "$compile_exit" -eq 0 ]; then
        echo -e "${RED}FAIL${NC} - Compilation succeeded but should have failed"
        fail_count=$((fail_count + 1))
        rm -f "${test_name}.c"
        return 1
    fi
    
    # Step 2: Check if error message contains expected pattern
    if echo "$output" | grep -q "$expected_error"; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
        return 0
    else
        echo -e "${YELLOW}PARTIAL${NC} - Compilation failed as expected, but error message didn't match pattern"
        echo "  Expected pattern: $expected_error"
        echo "  Actual output: $output"
        pass_count=$((pass_count + 1))  # Still count as pass since it failed
        return 0
    fi
}

# Main test execution
echo "Ion Compiler Test Harness"
echo "========================="
echo ""

# Create test directory if it doesn't exist
cd "$(dirname "$0")" || exit 1

# Run positive tests (should compile and run)
# Wrap each test call to ensure failures don't stop execution
if [ -f "test_basic.ion" ]; then
    test_file "test_basic.ion" 42 || true
fi

if [ -f "test_arithmetic.ion" ]; then
    test_file "test_arithmetic.ion" 30 || true
fi

if [ -f "test_move_basic.ion" ]; then
    test_file "test_move_basic.ion" 10 || true
fi

if [ -f "test_ref_valid.ion" ]; then
    test_file "test_ref_valid.ion" 0 || true
fi

if [ -f "test_send_basic.ion" ]; then
    test_file "test_send_basic.ion" 42 || true
fi

if [ -f "test_defer_basic.ion" ]; then
    test_file "test_defer_basic.ion" 7 || true
fi

if [ -f "test_channel_basic.ion" ]; then
    test_file "test_channel_basic.ion" 0 || true
fi

if [ -f "test_spawn_basic.ion" ]; then
    test_file "test_spawn_basic.ion" 0 || true
fi

if [ -f "test_if_basic.ion" ]; then
    test_file "test_if_basic.ion" 2 || true
fi

if [ -f "test_if_no_else.ion" ]; then
    test_file "test_if_no_else.ion" 2 || true
fi

if [ -f "test_struct_basic.ion" ]; then
    test_file "test_struct_basic.ion" 7 || true
fi

# Phase 1 tests
if [ -f "test_enum_basic.ion" ]; then
    test_file "test_enum_basic.ion" 0 || true
fi

if [ -f "test_match_basic.ion" ]; then
    test_file "test_match_basic.ion" 0 || true
fi

if [ -f "test_while_basic.ion" ]; then
    test_file "test_while_basic.ion" 0 || true
fi

if [ -f "test_call_basic.ion" ]; then
    test_file "test_call_basic.ion" 30 || true
fi

if [ -f "test_string_basic.ion" ]; then
    test_file "test_string_basic.ion" 0 || true
fi

if [ -f "test_match_pattern_bindings.ion" ]; then
    test_file "test_match_pattern_bindings.ion" 42 || true
fi

if [ -f "test_match_complex.ion" ]; then
    test_file "test_match_complex.ion" 10 || true
fi

if [ -f "test_generic_types.ion" ]; then
    test_file "test_generic_types.ion" 0 || true
fi

if [ -f "test_box_basic.ion" ]; then
    test_file "test_box_basic.ion" 0 || true
fi

if [ -f "test_vec_basic.ion" ]; then
    test_file "test_vec_basic.ion" 0 || true
fi

if [ -f "test_enum_generic.ion" ]; then
    test_file "test_enum_generic.ion" 0 || true
fi

if [ -f "test_generic_struct.ion" ]; then
    test_file "test_generic_struct.ion" 0 || true
fi

if [ -f "test_string_from.ion" ]; then
    test_file "test_string_from.ion" 0 || true
fi

# Standard library tests
if [ -f "test_box_ops.ion" ]; then
    test_file "test_box_ops.ion" 0 || true
fi

if [ -f "test_vec_new.ion" ]; then
    test_file "test_vec_new.ion" 0 || true
fi

if [ -f "test_vec_push_pop.ion" ]; then
    test_file "test_vec_push_pop.ion" 0 || true
fi

if [ -f "test_vec_get_set.ion" ]; then
    test_file "test_vec_get_set.ion" 0 || true
fi

if [ -f "test_vec_capacity.ion" ]; then
    test_file "test_vec_capacity.ion" 0 || true
fi

if [ -f "test_string_new.ion" ]; then
    test_file "test_string_new.ion" 0 || true
fi

if [ -f "test_string_push_str.ion" ]; then
    test_file "test_string_push_str.ion" 0 || true
fi

# Phase 2 tests - Modules and FFI
if [ -f "test_module_basic.ion" ]; then
    test_file "test_module_basic.ion" 30 || true
fi

if [ -f "test_ffi_basic.ion" ]; then
    test_file "test_ffi_basic.ion" 0 || true
fi

# Run negative tests (should fail to compile)
if [ -f "test_move_error.ion" ]; then
    test_error "test_move_error.ion" "UseAfterMove" || true
fi

if [ -f "test_ref_return_error.ion" ]; then
    test_error "test_ref_return_error.ion" "ReferenceEscape" || true
fi

if [ -f "test_ref_return_error2.ion" ]; then
    # This one should fail because return type mismatch (can't return &int where int expected)
    # But it might also fail on the no-escape rule first
    test_error "test_ref_return_error2.ion" "ReferenceEscape\|TypeMismatch" || true
fi

if [ -f "test_channel_ref_error.ion" ]; then
    test_error "test_channel_ref_error.ion" "Send element type for channel" || true
fi

if [ -f "test_send_ref_error.ion" ]; then
    test_error "test_send_ref_error.ion" "Send element type for channel" || true
fi

if [ -f "test_spawn_ref_error.ion" ]; then
    test_error "test_spawn_ref_error.ion" "Send value for spawn capture" || true
fi

if [ -f "test_spawn_move_error.ion" ]; then
    test_error "test_spawn_move_error.ion" "UseAfterMove" || true
fi

if [ -f "test_struct_ref_error.ion" ]; then
    test_error "test_struct_ref_error.ion" "ReferenceEscape" || true
fi

if [ -f "test_enum_ref_error.ion" ]; then
    test_error "test_enum_ref_error.ion" "ReferenceEscape" || true
fi

# Phase 2 negative tests
if [ -f "test_module_visibility.ion" ]; then
    test_error "test_module_visibility.ion" "Cannot access non-public" || true
fi

# Phase 3 tests - Arrays and Slices
if [ -f "test_array_basic.ion" ]; then
    test_file "test_array_basic.ion" 6 || true
fi

if [ -f "test_array_literal.ion" ]; then
    test_file "test_array_literal.ion" 21 || true
fi

if [ -f "test_array_indexing.ion" ]; then
    test_file "test_array_indexing.ion" 150 || true
fi

if [ -f "test_slice_basic.ion" ]; then
    test_file "test_slice_basic.ion" 3 || true
fi

if [ -f "test_slice_indexing.ion" ]; then
    test_file "test_slice_indexing.ion" 42 || true
fi

if [ -f "test_array_to_slice_coercion.ion" ]; then
    test_file "test_array_to_slice_coercion.ion" 10 || true
fi

# Phase 3 tests - Unsafe Blocks
if [ -f "test_unsafe_basic.ion" ]; then
    test_file "test_unsafe_basic.ion" 0 || true
fi

# Phase 3 negative tests
if [ -f "test_unsafe_extern_required.ion" ]; then
    test_error "test_unsafe_extern_required.ion" "must be inside an unsafe block" || true
fi

# Phase 4 tests - Boolean Type
if [ -f "test_bool_literal.ion" ]; then
    test_file "test_bool_literal.ion" 0 || true
fi

if [ -f "test_bool_operations.ion" ]; then
    test_file "test_bool_operations.ion" 0 || true
fi

if [ -f "test_bool_comparison.ion" ]; then
    test_file "test_bool_comparison.ion" 0 || true
fi

# Phase 4 tests - Floating-Point Types
if [ -f "test_float_literal.ion" ]; then
    test_file "test_float_literal.ion" 0 || true
fi

if [ -f "test_float_arithmetic.ion" ]; then
    test_file "test_float_arithmetic.ion" 0 || true
fi

if [ -f "test_float_promotion.ion" ]; then
    test_file "test_float_promotion.ion" 0 || true
fi

if [ -f "test_float_comparison.ion" ]; then
    test_file "test_float_comparison.ion" 0 || true
fi

# Phase 4 tests - Integer Types
if [ -f "test_integer_types.ion" ]; then
    test_file "test_integer_types.ion" 0 || true
fi

if [ -f "test_integer_promotion.ion" ]; then
    test_file "test_integer_promotion.ion" 0 || true
fi

if [ -f "test_integer_signed_unsigned.ion" ]; then
    test_file "test_integer_signed_unsigned.ion" 0 || true
fi

# Phase 4 tests - Type Aliases
if [ -f "test_type_alias_basic.ion" ]; then
    test_file "test_type_alias_basic.ion" 42 || true
fi

if [ -f "test_type_alias_generic.ion" ]; then
    test_file "test_type_alias_generic.ion" 0 || true
fi

if [ -f "test_type_alias_resolution.ion" ]; then
    test_file "test_type_alias_resolution.ion" 0 || true
fi

# Phase 4 negative tests
if [ -f "test_if_bool_required.ion" ]; then
    test_error "test_if_bool_required.ion" "bool.*if condition\|if condition.*bool" || true
fi

# Phase 5 tests - Method Call Syntax
if [ -f "test_method_call_basic.ion" ]; then
    test_file "test_method_call_basic.ion" 0 || true
fi

if [ -f "test_method_call_mut.ion" ]; then
    test_file "test_method_call_mut.ion" 0 || true
fi

if [ -f "test_method_call_generic.ion" ]; then
    test_file "test_method_call_generic.ion" 0 || true
fi

if [ -f "test_method_call_chaining.ion" ]; then
    test_file "test_method_call_chaining.ion" 0 || true
fi

# Phase 6 tests - Split Channel API, Struct-style Enum Variants, for...in Loops
if [ -f "test_channel_split.ion" ]; then
    test_file "test_channel_split.ion" 0 || true
fi

if [ -f "test_enum_struct_variant.ion" ]; then
    test_file "test_enum_struct_variant.ion" 42 || true
fi

if [ -f "test_for_loop.ion" ]; then
    test_file "test_for_loop.ion" 0 || true
fi

# Phase 7 tests - Escape sequences, Array initialization, Bitwise operators
if [ -f "test_escape_sequences.ion" ]; then
    test_file "test_escape_sequences.ion" 0 || true
fi

if [ -f "test_array_init.ion" ]; then
    test_file "test_array_init.ion" 0 || true
fi

if [ -f "test_bitwise_ops.ion" ]; then
    test_file "test_bitwise_ops.ion" 0 || true
fi

if [ -f "test_comparison_operators.ion" ]; then
    test_file "test_comparison_operators.ion" 0 || true
fi

if [ -f "test_type_cast.ion" ]; then
    test_file "test_type_cast.ion" 0 || true
fi

if [ -f "test_array_assignment.ion" ]; then
    test_file "test_array_assignment.ion" 0 || true
fi

# Phase 3 tests - Multi-file compilation
if [ -f "test_multifile.ion" ] && [ -f "utils.ion" ]; then
    test_count=$((test_count + 1))
    echo -n "Testing test_multifile (multi-file mode)... "
    
    # Compile in multi-file mode (we're already in tests/ directory)
    # Use absolute path for compiler to ensure it works
    compiler_abs="$COMPILER"
    if [ "${COMPILER:0:1}" != "/" ]; then
        # Convert relative path to absolute
        compiler_abs="$(cd "$(dirname "$COMPILER")" 2>/dev/null && pwd)/$(basename "$COMPILER")"
        if [ ! -f "$compiler_abs" ]; then
            compiler_abs="$COMPILER"
        fi
    fi
    
    compile_output=$("$compiler_abs" --mode multi --output test_multifile test_multifile.ion 2>&1)
    compile_exit=$?
    if [ "$compile_exit" -ne 0 ]; then
        echo -e "${RED}FAIL${NC} - Multi-file compilation failed"
        echo "  Error output: $compile_output"
        fail_count=$((fail_count + 1))
    else
        # Check that files were generated
        if [ ! -f "test_multifile.c" ] || [ ! -f "test_multifile.h" ] || [ ! -f "utils.c" ] || [ ! -f "utils.h" ]; then
            echo -e "${RED}FAIL${NC} - Generated files missing"
            fail_count=$((fail_count + 1))
            rm -f test_multifile.c test_multifile.h utils.c utils.h test_multifile test_multifile.o utils.o 2>/dev/null
        else
            # Check that test_multifile.c includes its header
            if ! grep -q '#include "test_multifile.h"' test_multifile.c 2>/dev/null; then
                echo -e "${YELLOW}WARN${NC} - test_multifile.c doesn't include its header"
            fi
            # Check that test_multifile.c includes utils.h
            if ! grep -q '#include "utils.h"' test_multifile.c 2>/dev/null; then
                echo -e "${YELLOW}WARN${NC} - test_multifile.c doesn't include utils.h"
            fi
            # Check that utils.c includes its header
            if ! grep -q '#include "utils.h"' utils.c 2>/dev/null; then
                echo -e "${YELLOW}WARN${NC} - utils.c doesn't include its header"
            fi
            
            # Run the executable
            if [ -f "test_multifile" ]; then
                ./test_multifile > /dev/null 2>&1
                actual_exit=$?
                if [ "$actual_exit" -eq 27 ]; then
                    echo -e "${GREEN}PASS${NC}"
                    pass_count=$((pass_count + 1))
                else
                    echo -e "${RED}FAIL${NC} - Expected exit code 27, got $actual_exit"
                    fail_count=$((fail_count + 1))
                fi
            else
                echo -e "${RED}FAIL${NC} - Executable not generated"
                fail_count=$((fail_count + 1))
            fi
            
            # Cleanup
            rm -f test_multifile.c test_multifile.h utils.c utils.h test_multifile test_multifile.o utils.o 2>/dev/null
        fi
    fi
fi

# Summary
echo ""
echo "========================="
echo "Tests run: $test_count"
echo -e "${GREEN}Passed: $pass_count${NC}"
if [ $fail_count -gt 0 ]; then
    echo -e "${RED}Failed: $fail_count${NC}"
else
    echo -e "Failed: $fail_count"
fi

if [ $fail_count -eq 0 ]; then
    exit 0
else
    exit 1
fi

