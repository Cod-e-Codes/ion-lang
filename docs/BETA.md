# Ion beta readiness contract

Ion is currently a `0.x` language and toolchain. This document defines what the
project means by a future beta release: a narrow, documented compatibility
surface rather than a promise that every experimental feature is complete.

## Beta compatibility window

For a beta tag, Ion should keep programs written against the stable beta subset
compiling for the rest of the beta line unless a release note explicitly calls
out a breaking change. Breaking changes require at least one of:

- a new minor version while Ion remains `0.x`;
- a migration note in `CHANGELOG.md`;
- an unstable-feature escape hatch documented in this file or `ION_SPEC.md`.

## Stable beta subset

The stable beta subset is the part of Ion that examples, CI, and the standard
library may rely on without an unstable marker:

- ownership, moves, drops, no-escape `&T` / `&mut T`, and borrow conflicts;
- functions, structs, enums, type aliases, modules, imports, and visibility;
- primitive numeric and boolean types, arrays, slices, `String`, `Vec<T>`,
  `Box<T>`, and `Result<T, E>`;
- `if`, `while`, `for`, `loop`, `break`, `continue`, `match`, guards, and
  `defer`;
- FFI through `extern "C"` and `unsafe`;
- channels and `spawn` with structural `Send`;
- `ion-build` projects with `ion.toml` fields documented in `ION_SPEC.md`.

## Unstable or constrained in beta

The following features may change shape before 1.0:

- tuples beyond flat two-field values;
- capture-free function literals and function-pointer coercions;
- generic ergonomics with optional built-in trait bounds (`Copy`, `Eq`, `Send`);
- byte-oriented string iteration;
- conservative move analysis in `while` and `for` loops;
- generated C layout details not covered by `docs/ABI.md`.

## Compatibility policy

- The compiler, bundled standard library, runtime C ABI, and `ion-build`
  manifest format are versioned together.
- Public stdlib import paths such as `stdlib/io.ion`, `stdlib/fmt.ion`,
  `stdlib/fs.ion`, and `stdlib/result.ion` should not be removed or renamed in a
  patch release.
- Additions are allowed in patch releases. Breaking language, stdlib, runtime,
  or manifest changes require a minor version while Ion is `0.x`.
- Unsupported behavior inside `unsafe` remains outside the compatibility
  guarantee.

## Platform support

Beta support is limited to platforms exercised in CI:

- Linux with GCC or Clang-compatible C tooling.
- Windows with MinGW GCC through Git Bash.

macOS and MSVC are best-effort until CI covers them.
