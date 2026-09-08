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
- `if`, `while`, `for`, `loop`, `break`, `continue`, `match`, `select`, guards, and
  `defer`;
- FFI through `extern "C"` and `unsafe`;
- channels, `try_send` / `try_recv`, `select`, `spawn` / `JoinHandle` / `join`, and structural `Send`;
- owned `File` streaming I/O and `stdlib/fs.ion` whole-file read;
- `ion-build` projects with `ion.toml` fields documented in `ION_SPEC.md`.

0.1.24 is a breaking 0.x channel change: `recv` returns `Option<T>`, `send` returns `SendResult<T>`, `clone_sender` makes MPSC real, and `channel<T>(cap)` sets capacity. Last sender drop unblocks `recv` with `None`. See CHANGELOG 0.1.24.

0.2.0 is a breaking 0.x contract and language-surface change: wrapping arithmetic, abort panics, UTF-8 `String`, match ownership join, `Vec::set` as `SetResult`, joinable `spawn`, `select`, nested tuples, and owned `File`. See CHANGELOG 0.2.0.

## Unstable or constrained in beta

The following features may change shape before 1.0:

- capture-free function literals and function-pointer coercions;
- generic ergonomics with optional built-in trait bounds (`Copy`, `Eq`, `Send`);
- byte-oriented string iteration;
- AST-structured loop ownership joins without a full CFG (see ION_SPEC §5.2);
- generated C layout details not covered by `docs/ABI.md`.

## Compatibility policy

- The compiler, bundled standard library, runtime C ABI, and `ion-build`
  manifest format are versioned together.
- Public stdlib import paths such as `stdlib/io.ion`, `stdlib/fmt.ion`,
  `stdlib/fs.ion`, `stdlib/result.ion`, and `stdlib/handle.ion` should not be
  removed or renamed in a patch release.
- Additions are allowed in patch releases. Breaking language, stdlib, runtime,
  or manifest changes require a minor version while Ion is `0.x`.
- Unsupported behavior inside `unsafe` remains outside the compatibility
  guarantee.

## Platform support

Beta support is limited to platforms exercised in CI:

- Linux with GCC or Clang-compatible C tooling.
- Windows with MinGW GCC through Git Bash.
- macOS (GitHub `macos-14`) with Clang-compatible C tooling.

MSVC is best-effort until CI covers it.
