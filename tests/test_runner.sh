#!/bin/bash

# Test harness for Ion compiler
# Automates: Ion -> C -> Compile C -> Run Executable -> Verify output

set +e

COMPILER="${COMPILER:-../target/release/ion-compiler}"
if [ -f "${COMPILER}.exe" ] && [ ! -x "$COMPILER" ]; then
    COMPILER="${COMPILER}.exe"
fi
ION_BUILD="${ION_BUILD:-../target/release/ion-build}"
if [ -f "${ION_BUILD}.exe" ] && [ ! -x "$ION_BUILD" ]; then
    ION_BUILD="${ION_BUILD}.exe"
fi
CC="${CC:-gcc}"
RUNTIME_OBJ="${RUNTIME_OBJ:-.ion_test_runtime.o}"

EXE_SUFFIX=""
is_windows_host() {
    command -v uname >/dev/null 2>&1 || return 1
    case "$(uname -s)" in
        MINGW*|MSYS*|CYGWIN*) return 0 ;;
        *) return 1 ;;
    esac
}
if is_windows_host; then
    EXE_SUFFIX=".exe"
fi

resolve_runtime_src() {
    if [ -f "../runtime/ion_runtime.c" ]; then
        printf '%s' "../runtime/ion_runtime.c"
    elif [ -f "runtime/ion_runtime.c" ]; then
        printf '%s' "runtime/ion_runtime.c"
    fi
}

c_include_flags() {
    printf '%s' "-I. -I.. -Iruntime -I../runtime"
}

c_link_libs() {
    local libs="-lpthread"
    if is_windows_host; then
        libs="$libs -lws2_32"
    fi
    printf '%s' "$libs"
}

precompile_runtime() {
    local runtime_src
    runtime_src="$(resolve_runtime_src)"
    if [ -z "$runtime_src" ]; then
        echo -e "${RED}ERROR${NC} - ion_runtime.c not found"
        exit 1
    fi
    if [ -f "$RUNTIME_OBJ" ] && [ ! "$runtime_src" -nt "$RUNTIME_OBJ" ]; then
        return 0
    fi
    if ! $CC -c "$runtime_src" $(c_include_flags) -o "$RUNTIME_OBJ" 2>/dev/null; then
        echo -e "${RED}ERROR${NC} - Failed to precompile runtime ($runtime_src)"
        exit 1
    fi
}

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

test_count=0
pass_count=0
fail_count=0

test_file() {
    local ion_file="$1"
    local expected_exit="$2"
    local test_name="${ion_file%.ion}"

    test_count=$((test_count + 1))
    echo -n "Testing ${test_name}... "

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

    local compile_cmd="$CC \"$c_file\" \"$RUNTIME_OBJ\" $(c_include_flags) $(c_link_libs) -o \"${test_name}${EXE_SUFFIX}\""
    if ! eval "$compile_cmd" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} - C compilation failed"
        fail_count=$((fail_count + 1))
        rm -f "$c_file"
        return 1
    fi

    "./${test_name}${EXE_SUFFIX}" > /dev/null 2>&1
    local actual_exit=$?

    if [ "$actual_exit" -eq "$expected_exit" ]; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
        rm -f "$c_file" "${test_name}" "${test_name}${EXE_SUFFIX}"
        return 0
    else
        echo -e "${RED}FAIL${NC} - Expected exit code $expected_exit, got $actual_exit"
        fail_count=$((fail_count + 1))
        rm -f "$c_file" "${test_name}" "${test_name}${EXE_SUFFIX}"
        return 1
    fi
}

test_error() {
    local ion_file="$1"
    local expected_error="$2"
    local test_name="${ion_file%.ion}"

    test_count=$((test_count + 1))
    echo -n "Testing ${test_name} (should error)... "

    local output
    output=$("$COMPILER" "$ion_file" 2>&1)
    local compile_exit=$?

    if [ "$compile_exit" -eq 0 ]; then
        echo -e "${RED}FAIL${NC} - Compilation succeeded but should have failed"
        fail_count=$((fail_count + 1))
        rm -f "${test_name}.c"
        return 1
    fi

    if echo "$output" | grep -q "$expected_error"; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
        return 0
    else
        echo -e "${YELLOW}PARTIAL${NC} - Compilation failed as expected, but error message didn't match pattern"
        echo "  Expected pattern: $expected_error"
        echo "  Actual output: $output"
        fail_count=$((fail_count + 1))
        return 1
    fi
}

test_cgen_grep() {
    local ion_file="$1"
    local must_match="$2"
    local must_not_match="$3"
    local test_name="${ion_file%.ion}"

    test_count=$((test_count + 1))
    echo -n "Testing ${test_name} (codegen)... "

    if [ -z "$must_match" ]; then
        echo -e "${RED}FAIL${NC} - cgen test requires non-empty must_match"
        fail_count=$((fail_count + 1))
        return 1
    fi

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

    if ! grep -Fq "$must_match" "$c_file" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} - Generated C missing pattern: $must_match"
        fail_count=$((fail_count + 1))
        rm -f "$c_file"
        return 1
    fi

    if [ -n "$must_not_match" ] && grep -q "$must_not_match" "$c_file" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} - Generated C contains forbidden pattern: $must_not_match"
        fail_count=$((fail_count + 1))
        rm -f "$c_file"
        return 1
    fi

    echo -e "${GREEN}PASS${NC}"
    pass_count=$((pass_count + 1))
    rm -f "$c_file"
    return 0
}

run_multi_test() {
    local main_ion="$1"
    local expected_exit="$2"
    local output_name="${3:-${main_ion%.ion}}"

    test_count=$((test_count + 1))
    echo -n "Testing ${output_name} (multi-file mode)... "

    local compiler_abs="$COMPILER"
    if [ "${COMPILER:0:1}" != "/" ]; then
        compiler_abs="$(cd "$(dirname "$COMPILER")" 2>/dev/null && pwd)/$(basename "$COMPILER")"
        if [ ! -f "$compiler_abs" ]; then
            compiler_abs="$COMPILER"
        fi
    fi

    local compile_output
    compile_output=$("$compiler_abs" --mode multi --output "$output_name" "$main_ion" 2>&1)
    local compile_exit=$?
    if [ "$compile_exit" -ne 0 ]; then
        echo -e "${RED}FAIL${NC} - Multi-file compilation failed"
        echo "  Error output: $compile_output"
        fail_count=$((fail_count + 1))
        return 1
    fi

    if [ ! -f "${output_name}${EXE_SUFFIX}" ]; then
        echo -e "${RED}FAIL${NC} - Executable not generated"
        fail_count=$((fail_count + 1))
        return 1
    fi

    "./${output_name}${EXE_SUFFIX}" > /dev/null 2>&1
    local actual_exit=$?
    if [ "$actual_exit" -eq "$expected_exit" ]; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
    else
        echo -e "${RED}FAIL${NC} - Expected exit code $expected_exit, got $actual_exit"
        fail_count=$((fail_count + 1))
    fi

    rm -f "${output_name}${EXE_SUFFIX}" "${output_name}.c" "${output_name}.h" "${output_name}.o" 2>/dev/null
    rm -f utils.c utils.h utils.o struct_lib.c struct_lib.h struct_lib.o 2>/dev/null
    rm -f fmt.c fmt.h fmt.o io.c io.h io.o 2>/dev/null
    return 0
}

test_multifile() {
    if [ ! -f "test_multifile.ion" ] || [ ! -f "utils.ion" ]; then
        return 0
    fi
    run_multi_test "test_multifile.ion" 27 "test_multifile"
}

test_multi_struct() {
    if [ ! -f "test_multi_struct.ion" ] || [ ! -f "struct_lib.ion" ]; then
        return 0
    fi
    run_multi_test "test_multi_struct.ion" 30 "test_multi_struct"
}

test_multi_fmt_io() {
    if [ ! -f "test_multi_fmt_io.ion" ]; then
        return 0
    fi
    run_multi_test "test_multi_fmt_io.ion" 42 "test_multi_fmt_io"
}

test_ion_build() {
    if [ ! -f "build_hello/ion.toml" ]; then
        return 0
    fi

    test_count=$((test_count + 1))
    echo -n "Testing ion-build (build_hello)... "

    local build_output
    build_output=$(cd build_hello && "$ION_BUILD" build 2>&1)
    local build_exit=$?
    if [ "$build_exit" -ne 0 ]; then
        echo -e "${RED}FAIL${NC} - ion-build failed"
        echo "  Error output: $build_output"
        fail_count=$((fail_count + 1))
        return 1
    fi

    if [ ! -f "build_hello/out/build_hello${EXE_SUFFIX}" ]; then
        echo -e "${RED}FAIL${NC} - Executable not generated in build_hello/out/"
        fail_count=$((fail_count + 1))
        return 1
    fi

    "build_hello/out/build_hello${EXE_SUFFIX}" > /dev/null 2>&1
    local actual_exit=$?
    if [ "$actual_exit" -eq 55 ]; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
    else
        echo -e "${RED}FAIL${NC} - Expected exit code 55, got $actual_exit"
        fail_count=$((fail_count + 1))
    fi

    rm -rf build_hello/out 2>/dev/null
    return 0
}

test_ion_build_bad_main() {
    if [ ! -f "build_bad_main/ion.toml" ]; then
        return 0
    fi

    test_count=$((test_count + 1))
    echo -n "Testing ion-build (bad main)... "

    local build_output
    build_output=$(cd build_bad_main && "$ION_BUILD" build 2>&1)
    local build_exit=$?
    if [ "$build_exit" -eq 0 ]; then
        echo -e "${RED}FAIL${NC} - ion-build should fail for missing main"
        fail_count=$((fail_count + 1))
        return 1
    fi

    if echo "$build_output" | grep -q "main file not found"; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
    else
        echo -e "${YELLOW}PARTIAL${NC} - Build failed but pattern not matched"
        echo "  Error output: $build_output"
        pass_count=$((pass_count + 1))
    fi
    return 0
}

strip_cr() {
    printf '%s' "$1" | tr -d '\r'
}

resolve_error_pattern() {
    local pattern="$1"
    local exit_code="$2"
    local must_match="$3"
    local must_not_match="$4"

    if [ -n "$pattern" ]; then
        printf '%s' "$pattern"
        return 0
    fi
    if [ -n "$must_match" ]; then
        printf '%s' "$must_match"
        return 0
    fi
    if [ -n "$must_not_match" ]; then
        printf '%s' "$must_not_match"
        return 0
    fi
    if [ -n "$exit_code" ]; then
        printf '%s' "$exit_code"
        return 0
    fi
}

# Field separator for manifest rows (ASCII RS). Tabs cannot be used here because
# bash read collapses consecutive IFS delimiters even when IFS is only tab.
MANIFEST_FS=$'\036'

verify_harness_tsv_parser() {
    echo -n "Harness self-check (TSV cgen field parsing)... "
    local parsed
    parsed=$(
        awk -F '\t' '{
            printf "%s%s%s%s%s%s%s%s%s%s%s",
                $1, "'"$MANIFEST_FS"'", $2, "'"$MANIFEST_FS"'", $3,
                "'"$MANIFEST_FS"'", $4, "'"$MANIFEST_FS"'", $5,
                "'"$MANIFEST_FS"'", $6
        }' <<'EOF'
test_struct_field_drop_vec.ion	cgen			ion_vec_free((ion_vec_t*)(h.items))	
EOF
    )
    local must_match
    IFS="$MANIFEST_FS" read -r _file _kind _exit _error must_match _must_not <<< "$parsed"
    if [ "$must_match" != "ion_vec_free((ion_vec_t*)(h.items))" ]; then
        echo -e "${RED}FAIL${NC}"
        echo "  Expected must_match in field 5, got: '$must_match'"
        exit 1
    fi
    echo -e "${GREEN}PASS${NC}"
}

verify_harness_cgen_empty_pattern() {
    echo -n "Harness self-check (cgen rejects empty must_match)... "
    local tmp_ion="test_harness_selfcheck_cgen.ion"
    printf '%s\n' 'fn main() -> int { return 0; }' > "$tmp_ion"
    if "$COMPILER" "$tmp_ion" > /dev/null 2>&1; then
        local saved_test_count=$test_count
        local saved_pass_count=$pass_count
        local saved_fail_count=$fail_count
        test_cgen_grep "$tmp_ion" "" "" > /dev/null 2>&1
        local rc=$?
        test_count=$saved_test_count
        pass_count=$saved_pass_count
        fail_count=$saved_fail_count
        rm -f "$tmp_ion" "${tmp_ion%.ion}.c"
        if [ "$rc" -eq 0 ]; then
            echo -e "${RED}FAIL${NC} - empty must_match should fail"
            exit 1
        fi
        echo -e "${GREEN}PASS${NC}"
    else
        rm -f "$tmp_ion"
        echo -e "${RED}FAIL${NC} - could not compile self-check fixture"
        exit 1
    fi
}

run_manifest() {
    local manifest="${1:-test_expectations.tsv}"
    if [ ! -f "$manifest" ]; then
        echo -e "${RED}ERROR${NC} - Missing manifest: $manifest"
        fail_count=$((fail_count + 1))
        return 1
    fi

    while IFS="$MANIFEST_FS" read -r file kind exit_code error_pattern must_match must_not_match || [ -n "$file" ]; do
        file="$(strip_cr "$file")"
        kind="$(strip_cr "$kind")"
        exit_code="$(strip_cr "$exit_code")"
        error_pattern="$(strip_cr "$error_pattern")"
        must_match="$(strip_cr "$must_match")"
        must_not_match="$(strip_cr "$must_not_match")"

        [ "$file" = "file" ] && continue
        [ -z "$file" ] && continue
        [ ! -f "$file" ] && continue

        case "$kind" in
            run)
                test_file "$file" "$exit_code" || true
                ;;
            error)
                test_error "$file" "$(resolve_error_pattern "$error_pattern" "$exit_code" "$must_match" "$must_not_match")" || true
                ;;
            cgen)
                test_cgen_grep "$file" "$must_match" "$must_not_match" || true
                ;;
            *)
                echo -e "${YELLOW}WARN${NC} - Unknown manifest kind '$kind' for $file"
                ;;
        esac
    done < <(
        awk -F '\t' -v fs="$MANIFEST_FS" 'NR > 1 && $1 != "" {
            printf "%s%s%s%s%s%s%s%s%s%s%s\n",
                $1, fs, $2, fs, $3, fs, $4, fs, $5, fs, $6
        }' "$manifest" | tr -d '\r'
    )
}

echo "Ion Compiler Test Harness"
echo "========================="
echo ""

cd "$(dirname "$0")" || exit 1

precompile_runtime

verify_harness_tsv_parser
verify_harness_cgen_empty_pattern
echo ""

run_manifest "test_expectations.tsv"
test_multifile || true
test_multi_struct || true
test_multi_fmt_io || true
test_ion_build || true
test_ion_build_bad_main || true

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
