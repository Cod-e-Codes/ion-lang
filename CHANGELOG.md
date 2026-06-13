# Changelog

## [Unreleased]

### Release engineering

- Pin Rust toolchain via `rust-toolchain.toml` and commit `Cargo.lock`.
- Add GitHub Actions CI (unit tests, clippy, release build, integration tests on Linux and Windows).
- Declare `ion-compiler` and `ion-lsp` binaries explicitly in `Cargo.toml`.

### Correctness

- Fix match expression rvalue codegen when used in `let` bindings (assign arm value to target variable).
- Emit `Vec`/`slice`/`tuple` typedefs before struct definitions so struct fields holding `Vec<T>` get correct drop codegen.

### CLI

- Add `--version` flag.
- Print parser and type checker errors with line numbers via `Display` (stable error substrings preserved for integration tests).
- Type-check merged program (main plus imports) in one pass.

### Stdlib

- Add `stdlib/result.ion` with generic `Result<T, E>`.
- Replace `fs::read_to_string` with `fs::read_to_string_result` returning `ReadResult` (`Ok(String)` / `Err(int)` codes).

### Tests

- Treat harness `PARTIAL` error-pattern mismatches as failures.
- Add `test_match_expr_rvalue.ion`, `test_struct_field_drop_vec.ion`.

### Docs

- Update README, CONTRIBUTING, tests README, and agent skill notes.
