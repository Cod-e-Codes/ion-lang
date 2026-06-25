# Contributing to Ion

## Prerequisites

- Rust 1.96.0 (`rust-toolchain.toml` pins the version; run `rustup update` if `rustc --version` is older)
- GCC or Clang (MinGW on Windows for generated C)
- Git Bash on Windows for integration tests

## Build

```bash
cargo build --release --bin ion-compiler --bin ion-build --bin ion-lsp
```

Install CLI tools into your Cargo bin directory:

```bash
cargo install --path . --bin ion-compiler --bin ion-build
```

`ion-lsp` is built with the release command above; point the editor extension at `target/release/ion-lsp` (see README IDE Support).

CI (`.github/workflows/ci.yml`) builds all three binaries on Linux, runs integration tests, ASan/UBSan smoke on generated C, and the full integration harness under `-Wall -Wextra -Werror` on Linux. The Windows job builds `ion-compiler` and `ion-build` only. Build `ion-lsp` locally for IDE work.

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
