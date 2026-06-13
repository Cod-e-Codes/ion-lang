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

The harness runs tests under `tests/` only. Files in `examples/` are documented demos - compile and run them manually (see [README.md](../../../README.md)); they are not registered in `test_runner.sh`.

## Tests are not auto-discovered

Creating `tests/test_foo.ion` does **nothing** until you add a matching `if [ -f "test_foo.ion" ]; then ... fi` block in `tests/test_runner.sh`. Every new test requires both the `.ion` file **and** harness registration. Update `tests/README.md` too.

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

## Add a positive test

1. Create `tests/test_<feature>.ion` with `fn main() -> int { ... }`
2. Return a distinct integer as exit code (e.g. `return 42;`)
3. Register in `tests/test_runner.sh`:

```bash
if [ -f "test_myfeature.ion" ]; then
    test_file "test_myfeature.ion" 42 || true
fi
```

4. Document in `tests/README.md` under the appropriate category

## Add a negative test

Program should **fail to compile**. Register:

```bash
if [ -f "test_myfeature_error.ion" ]; then
    test_error "test_myfeature_error.ion" "ExpectedSubstring" || true
fi
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

Special-case in `test_runner.sh` (see `test_multifile` block): uses `--mode multi --output test_multifile test_multifile.ion`, checks `.c`/`.h` generation, compiles objects, expects exit code 27.

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
- `test_array_bounds_panic.ion` - panic/abort; skipped by harness (manual only)

## After adding tests

```bash
cargo build --release --bin ion-compiler
cd tests && ./test_runner.sh
```

Update `tests/README.md` catalog entry. Do not hardcode pass/fail totals in docs.
