# Ion Compiler

A source-to-source transpiler from Ion to C. Ion is transpiled to human-readable C, then compiled with a C compiler.

## What is Ion?

Ion is a systems programming language with:
- Move-only ownership, no GC
- Stack-only, no-escape references (`&T`, `&mut T`)
- Channels-only concurrency with OS threads and structural `Send` checking
- C backend: Ion is transpiled to human-readable C, then compiled with a C compiler

## Language Overview

Brief summary; see [ION_SPEC.md](ION_SPEC.md) for the full language reference.

- **Ownership**: move by default; single owner per value; use-after-move is a compile error
- **Borrowing**: `&T` and `&mut T` are stack-local; references cannot escape the function (no return, no struct fields, no channels, no `spawn`)
- **Types**: primitives, structs, enums (tuple and struct variants), generics, `[T; N]`, `[]T`, `Box<T>`, `Vec<T>`, `String`
- **Control flow**: `if`/`while` (bool conditions), `loop { }`, `for x in expr` over `Vec<T>`, `[T; N]`, or `String` (bytes as `u8`), `match` with guards, `defer`
- **Concurrency**: `channel<T>()` returns `(Sender<T>, Receiver<T>)`; `send(&tx, v)` and `recv(&mut rx)`; `spawn { ... }` with structural `Send`
- **FFI**: `extern "C"` blocks, raw pointers `*T`, calls require `unsafe`
- **Stdlib**: `stdlib/io.ion`, `stdlib/fmt.ion`, `stdlib/fs.ion`, and `stdlib/result.ion`

Known limitations: [ION_SPEC.md section 10.3](ION_SPEC.md#103-known-limitations).

## Documentation

| Resource | Contents |
|----------|----------|
| [ION_SPEC.md](ION_SPEC.md) | Language semantics, grammar, stdlib contracts |
| [tests/README.md](tests/README.md) | Integration test catalog |
| [examples/](examples/) | Runnable example programs |

## Building

### Prerequisites

- Rust 1.96.0 (see `rust-toolchain.toml`; `rustup update` if your toolchain is older)
- A C compiler: GCC or Clang (on Windows, use MinGW GCC for generated C; MSVC is not the primary target)
- Git Bash on Windows for `tests/test_runner.sh`

### Build the Compiler

```bash
cargo build --release
```

This builds `ion-compiler`, `ion-build`, and `ion-lsp`. Install into your Cargo bin directory:

```bash
cargo install --path . --bin ion-compiler --bin ion-build
```

Binaries: `target/release/ion-compiler`, `target/release/ion-build`, `target/release/ion-lsp` (`.exe` on Windows).

## IDE Support

### VS Code Extension

Ion has a VS Code / Cursor extension that provides:
- Syntax highlighting
- Real-time diagnostics (syntax and type errors)
- Hover: variable types at use sites and `let` binding identifiers; symbol signatures and attached `//` doc prose at definitions and qualified imports
- Completion: keywords, builtins, and file symbols
- Go to definition: variables, function calls, and user-defined methods; imported `mod::func` opens the module file

Limitations: built-in methods (`Vec::push`, etc.) do not go to definition; completion has no prefix filtering. Full list in [ION_SPEC.md section 10.3](ION_SPEC.md#103-known-limitations).

**Installation:**

1. Build the LSP server:
   ```bash
   cargo build --release --bin ion-lsp
   ```

2. Install the extension from the `ion-vscode` directory:
   ```bash
   cd ion-vscode
   npm install
   npm run compile
   npx @vscode/vsce package --allow-missing-repository
   code --install-extension ion-language-0.1.0.vsix
   ```
   On Cursor, use `cursor --install-extension ion-language-0.1.0.vsix` instead of `code`.

3. Workspace settings (`.vscode/settings.json`) point `ion.lspPath` at `target/release/ion-lsp.exe`.

**Packaging (local install, no marketplace publish):**

```bash
cd ion-vscode
npm install
npm run compile
npx @vscode/vsce package --allow-missing-repository
cursor --install-extension ion-language-0.1.0.vsix
```

## Usage

### Quick Start

From the repository root (uses root `ion.toml`):

```powershell
cargo build --release --bin ion-build
.\target\release\ion-build.exe build
.\target\hello_world.exe
```

`ion build` transpiles, compiles generated C, links the runtime, and writes the executable under `target/` by default.

Source: [examples/hello_world_safe.ion](examples/hello_world_safe.ion). For a minimal FFI example without stdlib, see [examples/hello_world.ion](examples/hello_world.ion).

### Advanced: Manual Transpile and Link

For codegen inspection, integration test workflows, or debugging generated C, use `ion-compiler` directly (see [ION_SPEC.md section 10.1](ION_SPEC.md#101-project-build-ion-build)):

```bash
./target/release/ion-compiler examples/hello_world.ion
gcc examples/hello_world.c runtime/ion_runtime.c -o hello_world \
    -I. -I.. -Iruntime -I../runtime -lpthread
./hello_world
```

The integration harness (`tests/test_runner.sh`) calls `ion-compiler` and `gcc` this way. Application development should use `ion-build` instead.

### Project manifests (`ion.toml`)

`ion-build` discovers `ion.toml` by walking up from the current directory. Required fields: `name`, `main`, `output`. Common optional fields: `mode` (`single` or `multi`), `out_dir` (default `target`), `cflags`, `ldflags`, `stdlib_paths`, `emit_in_source`.

Root [ion.toml](ion.toml) builds [examples/hello_world_safe.ion](examples/hello_world_safe.ion). Per-example manifests live under [examples/](examples/) (for example `spawn_channel.toml`, `http_server.toml`, `examples/data_lib/ion.toml`). Use `--manifest path` when the file is not named `ion.toml`:

```powershell
cd examples
..\target\release\ion-build.exe build --manifest http_server.toml
.\target\http_server.exe
```

FFI programs that rename C symbols (for example `recv_sys`) set compile-time `-D` flags in `cflags`, not `ldflags`. See [examples/http_server.toml](examples/http_server.toml).

### Multi-file projects

Multi-file mode (`mode = "multi"` in `ion.toml`) transpiles each module to `.c`/`.h`, compiles objects, and links one executable. Artifacts default to `out_dir` (usually `target/`), not the source tree.

```powershell
cd examples\data_lib
..\..\target\release\ion-build.exe build
.\target\data_lib.exe
```

For manual multi-file codegen in the source directory (no `ion-build`), use `ion-compiler --mode multi --output NAME main.ion`. See [examples/data_lib/README.md](examples/data_lib/README.md).

## Example Programs

Top-level `examples/*.ion` files each have a checked-in merged `examples/*.c` codegen snapshot. Regenerate after compiler codegen changes (from repo root, relative paths keep portable banners):

```bash
for f in examples/*.ion; do ./target/release/ion-compiler "$f"; done
./target/release/ion-compiler examples/text_summary/text_summary.ion
```

Committed `.c` snapshots are for review and `ion-compiler` regression; building and running examples uses `ion-build` and the manifests below.

**Build and run** (from `examples/` unless noted):

```powershell
# Single-file example with a manifest in examples/
..\target\release\ion-build.exe build --manifest spawn_channel.toml
.\target\spawn_channel.exe

# http_server (FFI cflags in http_server.toml)
..\target\release\ion-build.exe build --manifest http_server.toml

# text_summary (has its own ion.toml; needs sample.txt in that directory)
cd text_summary
..\..\target\release\ion-build.exe build
.\target\text_summary.exe
```

On Linux or macOS, use `./target/release/ion-build` and drop `.exe`. Windows channel/spawn builds add `-lws2_32` automatically.

| File | Manifest | What it demonstrates |
|------|----------|---------------------|
| [examples/hello_world.ion](examples/hello_world.ion) | (use `ion-compiler` only; no stdlib) | Minimal FFI `write()` to stdout |
| [examples/hello_world_safe.ion](examples/hello_world_safe.ion) | [hello_world_safe.toml](examples/hello_world_safe.toml) or root [ion.toml](ion.toml) | stdlib `io` module |
| [examples/spawn_channel.ion](examples/spawn_channel.ion) | [spawn_channel.toml](examples/spawn_channel.toml) | `spawn` with cross-thread channels |
| [examples/http_server.ion](examples/http_server.ion) | [http_server.toml](examples/http_server.toml) | Sockets, FFI, concurrent clients via `spawn` |
| [examples/showcase.ion](examples/showcase.ion) | [showcase.toml](examples/showcase.toml) | Mixed language features |
| [examples/access_log.ion](examples/access_log.ion) | [access_log.toml](examples/access_log.toml) | Log parsing, spawn, channels, fmt/io |
| [examples/minimal.ion](examples/minimal.ion) | (transpile-only with `ion-compiler`) | Smallest valid program |
| [examples/channel_worker.ion](examples/channel_worker.ion) | [channel_worker.toml](examples/channel_worker.toml) | Channel worker |
| [examples/text_summary/text_summary.ion](examples/text_summary/text_summary.ion) | [text_summary/ion.toml](examples/text_summary/ion.toml) | `fs` read, string iteration, counts |
| [examples/data_lib/main.ion](examples/data_lib/main.ion) | [data_lib/ion.toml](examples/data_lib/ion.toml) | Multi-module library; see [data_lib/README.md](examples/data_lib/README.md) |

`examples/data_lib/` keeps only `.ion` sources; `ion-build` writes `.c`, `.h`, and the executable under `examples/data_lib/target/` (not committed).

## Project Structure

```
.
├── src/
│   ├── bin/        # ion-build, ion-lsp
│   ├── lexer/      # Tokenizer
│   ├── parser/     # AST construction
│   ├── ast/        # AST node definitions
│   ├── compiler/   # Module resolution, import paths
│   ├── build/      # ion.toml, ion-build driver, C toolchain
│   ├── tc/         # Type checker (safety, visibility, qualified names)
│   ├── ir/         # Intermediate representation
│   ├── cgen/       # C code generator
│   └── lsp/        # Language server
├── runtime/        # C runtime sources and headers
├── stdlib/         # Standard library modules
├── ion-vscode/     # VS Code / Cursor extension
├── examples/       # Example Ion programs
└── tests/          # Test programs
```

## Development

### Run Tests

Unit tests:

```bash
cargo test
```

Integration tests (use Git Bash on Windows, not WSL):

```bash
cd tests && ./test_runner.sh
```

The runner loads `tests/test_expectations.tsv` (exit codes, error patterns, codegen checks) and runs each entry. Add a new positive test with one `.ion` file plus one manifest line. See [tests/README.md](tests/README.md).

### Linting

```bash
cargo clippy -- -D warnings
```

## License

MIT
