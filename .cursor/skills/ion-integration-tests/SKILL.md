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

The harness runs tests under `tests/` only. Files in `examples/` are documented demos - compile and run them manually (see [README.md](../../../README.md)); they are not in the integration manifest.

## Run tests

From project root (Git Bash on Windows):

```bash
cargo build --release --bin ion-compiler
cd tests && ./test_runner.sh
```

Environment overrides:

```bash
COMPILER=../target/debug/ion-compiler CC=clang ./test_runner.sh
```

**Windows:** Use Git Bash, not WSL. Rebuild release after compiler changes. Stop `ion-lsp` if build fails with "Access is denied".

## Manifest (`test_expectations.tsv`)

Tab-separated columns:

| Column | `run` | `error` | `cgen` |
|--------|-------|---------|--------|
| `file` | `.ion` path | same | same |
| `kind` | `run` | `error` | `cgen` |
| `exit` | expected exit code | empty | empty |
| `error_pattern` | empty | grep on CLI stderr | empty |
| `must_match` | empty | empty | required substring in `.c` |
| `must_not_match` | empty | empty | optional forbidden substring |

`test_runner.sh` loops the manifest and calls `test_file`, `test_error`, or `test_cgen_grep`. Special cases stay explicit: `test_multifile` (multi-file mode), panic tests (codegen-only rows; manual runtime documented in [tests/README.md](../../../tests/README.md)).

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

### `test_error` PARTIAL pass trap

If compilation fails but the grep pattern does **not** match, the harness prints **PARTIAL** and still increments `pass_count`. A wrong pattern looks like a pass - always verify the pattern against actual CLI output and confirm the harness prints **PASS**, not PARTIAL.

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
