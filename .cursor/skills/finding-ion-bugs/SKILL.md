---
name: finding-ion-bugs
description: >-
  Systematically find and reproduce bugs in the ion-lang compiler, runtime, and
  tests. Use when debugging failures, hunting regressions, investigating wrong
  codegen, type-check mistakes, test_runner failures, clippy warnings, or when
  the user asks to find bugs, triage issues, or review changes for defects.
  Also use before filing GitHub issues or after a feature change breaks tests.
---

# Finding Bugs in ion-lang

Read `ion-lang` skill first for build commands and pipeline layout. This skill is a defect-hunting workflow, not a feature guide.

## Triage order

Run cheapest checks first; stop when you have a minimal repro.

```
- [ ] 1. Identify symptom (compile error, wrong runtime, test failure, LSP drift)
- [ ] 2. Reproduce with smallest input (.ion file or `cargo test` filter)
- [ ] 3. Localize stage: lexer → parser → tc → IR → cgen → runtime → harness
- [ ] 4. Bisect: git log / recent diff if regression
- [ ] 5. Fix + add test (see ion-integration-tests or adding-ion-features)
- [ ] 6. Full verify: cargo test && cargo clippy && test_runner.sh
```

## Reproduce

**Single file (compiler CLI):**

```bash
cargo build --release --bin ion-compiler
./target/release/ion-compiler path/to/repro.ion
# Inspect generated C
cat path/to/repro.c
```

**Rust unit tests:**

```bash
cargo test                    # all
cargo test lexer::            # one module
cargo test test_name -- --nocapture
```

**Integration harness (Git Bash on Windows):**

```bash
cargo build --release --bin ion-compiler --bin ion-build
cd tests && ./test_runner.sh
# One test
cd tests && ./test_runner.sh test_foo.ion
```

**Lint:**

```bash
cargo clippy -- -D warnings
```

**Windows:** Git Bash for `test_runner.sh`. Stop `ion-lsp` / `ion-compiler` if rebuild hits "Access is denied".

## Localize by symptom

| Symptom | First place to inspect | Quick check |
|---------|------------------------|-------------|
| Parse error / bad AST | `src/parser/mod.rs`, `src/lexer/mod.rs` | `./target/release/ion-compiler file.ion` |
| Type / ownership / Send | `src/tc/` (`ownership.rs`, `mod.rs`) | CLI stderr: `UseAfterMove`, `ReferenceEscape`, `Send` |
| Wrong or missing C | `src/cgen/`, `src/ir/mod.rs` | Compare `.c` output; `cgen` rows in `test_expectations.tsv` |
| Link / runtime crash | `runtime/ion_runtime.c`, generated calls | `gcc` link line from README |
| Import / multi-file | `src/compiler/mod.rs` | `--mode multi` |
| `ion-build` / manifest failure | `src/build/` (`manifest.rs`, `driver.rs`, `paths.rs`) | `tests/build_hello/`, `tests/build_bad_main/` |
| Test harness false pass/fail | `tests/test_runner.sh`, TSV row | Empty `must_match` on `cgen` rows fails harness self-check |
| LSP wrong, CLI correct | `src/lsp/` | LSP parses buffer + disk imports; formats errors differently than CLI |

Stage-specific checklists: [references/bug-hotspots.md](references/bug-hotspots.md).

## Regression bisect

```bash
git log --oneline -20
git bisect start
git bisect bad HEAD
git bisect good <known-good-commit>
# rebuild + repro each step
cargo build --release --bin ion-compiler --bin ion-build && cd tests && ./test_runner.sh test_foo.ion
```

## Change review for bugs

When the user wants a review of local changes (not interactive debugging):

1. Launch one `bugbot` subagent (`readonly: true`) with repo path and `Diff: branch changes` (or `uncommitted changes`). Follow the user-level `review-bugbot` skill for prompt shape.
2. Summarize findings by severity; do not auto-fix unless asked.

For security-sensitive changes (unsafe, extern, runtime memory), offer `security-review` subagent if the user asks.

## Minimal repro for integration tests

**Positive run test:** smallest `.ion` that exits wrong code.

**Negative compile test:** smallest program that should fail with a specific stderr pattern:

```bash
./target/release/ion-compiler tests/test_foo.ion 2>&1 | grep -F 'expected fragment'
```

**Codegen test:** add `cgen` row with `must_match` literal substring from expected `.c`.

## Report format

When reporting bugs to the user or an issue:

```markdown
## Summary
One sentence.

## Repro
Commands and file contents.

## Expected / Actual

## Stage
lexer | parser | tc | ir | cgen | runtime | tests | lsp

## Likely cause
Hypothesis with file:line if known.
```

## Anti-patterns

- Fixing tests by weakening ownership or Send rules
- Skipping `test_runner.sh` after compiler changes
- Debugging LSP using CLI stderr grep patterns (formats differ)
- Large refactors while hunting a single bug
- Claiming a fix without a repro test
