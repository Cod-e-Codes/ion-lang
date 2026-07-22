---
name: ion-integration-tests
description: >-
  Write and run Ion integration tests - .ion programs verified via test_runner.sh.
  Use when adding tests, fixing test failures, checking exit codes, writing negative
  compile-error tests, or validating compiler output in tests/. Also use when the
  user mentions test_runner, integration tests, test_*.ion, or wants to verify Ion
  programs compile and run correctly.
paths:
  - tests/**
  - examples/**
---

# Ion Integration Tests

End-to-end tests: Ion → C → gcc → run executable → assert exit code (or assert compile failure).

The harness runs tests under `tests/` only. Files in `examples/` are documented demos; build and run them with `ion-build` from each example directory (see [README.md](../../../README.md#example-programs)).

**Example output policy:** Each demo is `examples/<name>/` with `ion.toml`. Generated `.c` and executables go under that directory's `target/` (not committed). `text_summary/` also ships `sample.txt`.

To build examples, use `ion-build` (see README). Manual `gcc` + `runtime/ion_runtime.c` is for the test harness and advanced debugging only.

## Run tests

From project root (Git Bash on Windows):

```bash
cargo build --release --bin ion-compiler --bin ion-build
cd tests && ./test_runner.sh
```

From PowerShell (repo root; requires Git Bash):

```powershell
cargo build --release --bin ion-compiler --bin ion-build
& "${env:ProgramFiles}\Git\bin\bash.exe" -lc 'cd tests && ./test_runner.sh'
```

The harness primarily calls `ion-compiler` for `run`/`error`/`cgen` rows. It also runs `ion-build` smoke tests (`tests/build_hello/`, `tests/build_bad_main/`) via the `ION_BUILD` env var (defaults to `../target/release/ion-build`).

Environment overrides:

```bash
COMPILER=../target/debug/ion-compiler ION_BUILD=../target/debug/ion-build CC=clang ./test_runner.sh
RUNTIME_OBJ=/tmp/ion_runtime.o ./test_runner.sh
CFLAGS="-Wall -Wextra -Werror" RUNTIME_OBJ=.ion_test_runtime_werror.o ./test_runner.sh
```

Local full-harness `-Werror` matches the Linux CI warning-clean job.

```bash
CFLAGS="-fsanitize=address,undefined -fno-omit-frame-pointer" LDFLAGS="-fsanitize=address,undefined" ./test_runner.sh
```

**Harness link step:** On startup, `test_runner.sh` precompiles `runtime/ion_runtime.c` once to `RUNTIME_OBJ` (default `.ion_test_runtime.o`) and links that object for each `run` test and `test_multifile`. This path uses `ion-compiler` and `gcc` directly, not `ion-build`. Example programs under `examples/` use `ion-build` per README.

**Windows:** Use Git Bash, not WSL. Rebuild release after compiler changes. Stop `ion-lsp` if build fails with "Access is denied".

**Stale binaries:** `test_runner.sh` defaults to `../target/release/ion-compiler`. Rebuild after codegen changes and confirm that path is the binary you just built. Agent shells may set `CARGO_TARGET_DIR` outside the repo; pin it to the repo `target/` when verifying locally, or override with `COMPILER=` / `ION_BUILD=`.

**Method calls on reference parameters:** `test_method_call_ref_param.ion` covers `out.push()` / `vec.get()` style calls on `&T` and `&mut T` parameters (distinct from static `Vec::push(out, …)` and from method calls on local variables).

**String literal as owned `String` argument:** `test_string_call_arg_literal.ion` covers `fn f(s: String)` called with a string literal (cgen must emit `ion_string_from_literal`, not a raw C string).

## Manifest (`test_expectations.tsv`)

Tab-separated columns:

| Column | `run` | `error` | `cgen` |
|--------|-------|---------|--------|
| `file` | `.ion` path | same | same |
| `kind` | `run` | `error` | `cgen` |
| `exit` | expected exit code | empty | empty |
| `error_pattern` | empty | grep on CLI stderr | empty |
| `must_match` | empty | empty | required literal substring in `.c` (`grep -Fq`) |
| `must_not_match` | empty | empty | optional forbidden regex in `.c` (`grep -q`) |

`test_runner.sh` loops the manifest and calls `test_file`, `test_error`, or `test_cgen_grep`. Special cases stay explicit: `test_multifile` (multi-file mode), panic tests (codegen-only rows; manual runtime documented in [tests/README.md](../../../tests/README.md)).

### Manifest parsing and `cgen` grep

Authors still write normal tab-separated rows. The harness reads them with `awk -F '\t'` and an internal record separator so **empty columns are preserved** (bash `read` with tab IFS collapses consecutive tabs and breaks `cgen` rows).

On startup, two self-checks run before the manifest:

1. TSV field 5 (`must_match`) is parsed correctly for a sample `cgen` row
2. Empty `must_match` is rejected (would otherwise false-pass via `grep -q ""`)

**`cgen` pattern rules:**

| Column | grep mode | Write patterns as |
|--------|-----------|-------------------|
| `must_match` | Fixed string (`grep -Fq`) | Literal C text. Use `s->data[0]`, not `s->data\[0\]`. Parentheses are fine: `ion_vec_free((ion_vec_t*)(h.items))` |
| `must_not_match` | Basic regex (`grep -q`) | Regex when needed, e.g. `^[[:space:]]+print_int\(` on `test_fmt_println_int.ion` |

Empty `must_match` on a `cgen` row is a harness **FAIL**, not a skip.

## Add a positive test

1. Create `tests/test_<feature>.ion` with `fn main() -> int { ... }`
2. Return a distinct integer as exit code (e.g. `return 42;`)
3. Add one line to `tests/test_expectations.tsv`:

```
test_myfeature.ion	run	42
```

4. Document in `tests/README.md` under the appropriate category

## Add a negative test

Program should **fail to compile**. Add to manifest:

```
test_myfeature_error.ion	error		UseAfterMove
```

The harness greps **compiler CLI stderr** for the pattern (not LSP diagnostic text). Use stable substrings from `ion-compiler` output:

```bash
../target/release/ion-compiler test_myfeature_error.ion 2>&1
```

| Scenario | CLI stderr pattern |
|----------|-----------------|
| Use after move | `UseAfterMove` |
| Reference escape | `ReferenceEscape` |
| Non-Send channel | `Send element type for channel` |
| Non-Send spawn | `Send value for spawn capture` |
| Non-public import | `Cannot access non-public` |
| Extern without unsafe | `must be inside an unsafe block` |
| Non-bool if | `bool.*if condition\|if condition.*bool` |
| Fn literal capture | `ClosureCapture` |

### `test_error` PARTIAL pass trap

If compilation fails but the grep pattern does **not** match, the harness prints **PARTIAL** and increments `fail_count`. A wrong pattern is a harness failure - always verify the pattern against actual CLI output and confirm the harness prints **PASS**, not PARTIAL.

## Test file conventions

- Prefix: `test_*.ion`
- Helper modules for multi-file: e.g. `utils.ion` alongside `test_multifile.ion`
- Keep programs minimal - one behavior per file
- Use comments to explain intent for negative tests

## Multi-file test

`test_multifile.ion` + `utils.ion`: harness block `test_multifile` uses `--mode multi --output test_multifile`, expects exit code 27.

## Examples

**Positive** (`test_basic.ion`):

```ion
fn main() -> int {
    return 42;
}
```

**Negative** (`test_move_error.ion`):

```ion
fn main() -> int {
    let x = Box::new(10);
    let y = x;
    let z = x;  // use after move (Box is not copy)
    return Box::unwrap(z);
}
```

## What integration tests do NOT cover

- Rust unit tests - use `cargo test` for lexer/parser unit tests
- stdout output - harness only checks process exit code
- `test_array_bounds_panic.ion` / `test_slice_bounds_panic.ion` - panic/abort at runtime; harness checks generated C only (manual run in tests/README.md)

## After adding tests

```bash
cargo build --release --bin ion-compiler
cd tests && ./test_runner.sh
```

Update `tests/README.md` catalog entry. Verify with `./test_runner.sh`; do not hardcode pass totals in docs.
