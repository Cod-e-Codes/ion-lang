#!/bin/bash

# Test harness for Ion compiler
# Automates: Ion -> C -> Compile C -> Run Executable -> Verify output

set +e

COMPILER="${COMPILER:-../target/release/ion-compiler}"
if [ -f "${COMPILER}.exe" ] && [ ! -x "$COMPILER" ]; then
    COMPILER="${COMPILER}.exe"
fi
CC="${CC:-gcc}"

EXE_SUFFIX=""
if command -v uname >/dev/null 2>&1; then
    case "$(uname -s)" in
        MINGW*|MSYS*|CYGWIN*) EXE_SUFFIX=".exe" ;;
    esac
fi

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

    local compile_cmd="$CC \"$c_file\""
    if [ -f "../runtime/ion_runtime.c" ]; then
        compile_cmd="$compile_cmd ../runtime/ion_runtime.c"
    elif [ -f "runtime/ion_runtime.c" ]; then
        compile_cmd="$compile_cmd runtime/ion_runtime.c"
    fi
    compile_cmd="$compile_cmd -I. -I.. -Iruntime -I../runtime -lpthread"
    if command -v uname >/dev/null 2>&1; then
        case "$(uname -s)" in
            MINGW*|MSYS*|CYGWIN*) compile_cmd="$compile_cmd -lws2_32" ;;
        esac
    fi
    compile_cmd="$compile_cmd -o \"${test_name}${EXE_SUFFIX}\""
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

    if ! grep -q "$must_match" "$c_file" 2>/dev/null; then
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

test_multifile() {
    if [ ! -f "test_multifile.ion" ] || [ ! -f "utils.ion" ]; then
        return 0
    fi

    test_count=$((test_count + 1))
    echo -n "Testing test_multifile (multi-file mode)... "

    local compiler_abs="$COMPILER"
    if [ "${COMPILER:0:1}" != "/" ]; then
        compiler_abs="$(cd "$(dirname "$COMPILER")" 2>/dev/null && pwd)/$(basename "$COMPILER")"
        if [ ! -f "$compiler_abs" ]; then
            compiler_abs="$COMPILER"
        fi
    fi

    local compile_output
    compile_output=$("$compiler_abs" --mode multi --output test_multifile test_multifile.ion 2>&1)
    local compile_exit=$?
    if [ "$compile_exit" -ne 0 ]; then
        echo -e "${RED}FAIL${NC} - Multi-file compilation failed"
        echo "  Error output: $compile_output"
        fail_count=$((fail_count + 1))
        return 1
    fi

    if [ ! -f "test_multifile.c" ] || [ ! -f "test_multifile.h" ] || [ ! -f "utils.c" ] || [ ! -f "utils.h" ]; then
        echo -e "${RED}FAIL${NC} - Generated files missing"
        fail_count=$((fail_count + 1))
        rm -f test_multifile.c test_multifile.h utils.c utils.h test_multifile test_multifile.o utils.o 2>/dev/null
        return 1
    fi

    local multifile_cc="$CC test_multifile.c utils.c -I. -I.. -Iruntime -I../runtime ../runtime/ion_runtime.c -lpthread"
    if command -v uname >/dev/null 2>&1; then
        case "$(uname -s)" in
            MINGW*|MSYS*|CYGWIN*) multifile_cc="$multifile_cc -lws2_32" ;;
        esac
    fi
    multifile_cc="$multifile_cc -o test_multifile${EXE_SUFFIX}"
    if ! eval "$multifile_cc" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} - C compilation failed"
        fail_count=$((fail_count + 1))
        return 1
    fi

    if [ ! -f "test_multifile${EXE_SUFFIX}" ]; then
        echo -e "${RED}FAIL${NC} - Executable not generated"
        fail_count=$((fail_count + 1))
        rm -f test_multifile.c test_multifile.h utils.c utils.h test_multifile test_multifile${EXE_SUFFIX} test_multifile.o utils.o 2>/dev/null
        return 1
    fi

    ./test_multifile${EXE_SUFFIX} > /dev/null 2>&1
    local actual_exit=$?
    if [ "$actual_exit" -eq 27 ]; then
        echo -e "${GREEN}PASS${NC}"
        pass_count=$((pass_count + 1))
    else
        echo -e "${RED}FAIL${NC} - Expected exit code 27, got $actual_exit"
        fail_count=$((fail_count + 1))
    fi

    rm -f test_multifile.c test_multifile.h utils.c utils.h test_multifile test_multifile${EXE_SUFFIX} test_multifile.o utils.o 2>/dev/null
}

run_manifest() {
    local manifest="${1:-test_expectations.tsv}"
    if [ ! -f "$manifest" ]; then
        echo -e "${RED}ERROR${NC} - Missing manifest: $manifest"
        fail_count=$((fail_count + 1))
        return 1
    fi

    while IFS=$'\t' read -r file kind exit_code error_pattern must_match must_not_match; do
        [ "$file" = "file" ] && continue
        [ -z "$file" ] && continue
        [ ! -f "$file" ] && continue

        case "$kind" in
            run)
                test_file "$file" "$exit_code" || true
                ;;
            error)
                test_error "$file" "$error_pattern" || true
                ;;
            cgen)
                test_cgen_grep "$file" "$must_match" "$must_not_match" || true
                ;;
            *)
                echo -e "${YELLOW}WARN${NC} - Unknown manifest kind '$kind' for $file"
                ;;
        esac
    done < "$manifest"
}

echo "Ion Compiler Test Harness"
echo "========================="
echo ""

cd "$(dirname "$0")" || exit 1

run_manifest "test_expectations.tsv"
test_multifile || true

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
