# Contributing to Ion

## Prerequisites

- Rust stable (`rust-toolchain.toml` pins the channel)
- GCC or Clang (MinGW on Windows for generated C)
- Git Bash on Windows for integration tests

## Build

```bash
cargo build --release --bin ion-compiler
```

Install the compiler into your Cargo bin directory:

```bash
cargo install --path . --bin ion-compiler
```

## Test

Unit tests:

```bash
cargo test
```

Integration tests (from Git Bash):

```bash
cd tests && ./test_runner.sh
```

Lint:

```bash
cargo clippy -- -D warnings
```

Format check:

```bash
cargo fmt --check
```

## Pull requests

- Keep diffs focused; match existing style in the module you touch.
- Add or update integration tests in `tests/test_*.ion` and `tests/test_expectations.tsv` when behavior changes.
- Update `ION_SPEC.md` when language or stdlib semantics change.
- CI must pass (see `.github/workflows/ci.yml`).
