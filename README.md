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
- **Control flow**: `if`/`while` (bool conditions), `for x in expr` over `Vec<T>`, `[T; N]`, or `String` (bytes as `u8`), `match` with guards, `defer`
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

For codegen inspection or debugging, use `ion-compiler` directly:

```bash
./target/release/ion-compiler examples/hello_world.ion
gcc examples/hello_world.c runtime/ion_runtime.c -o hello_world \
    -I. -I.. -Iruntime -I../runtime -lpthread
./hello_world
```

### Single-File Mode (Default)

**Step 1: Compile Ion to C**

```bash
./target/release/ion-compiler input.ion
```

This generates `input.c` in the same directory as the source file.

**Step 2: Compile C to Executable**

The generated C code requires the Ion runtime library:

```bash
gcc input.c runtime/ion_runtime.c -o input -I. -I.. -Iruntime -I../runtime -lpthread
```

**Required flags:**
- `input.c` - generated C file
- `runtime/ion_runtime.c` - Ion runtime (when running from project root)
- `-I. -I.. -Iruntime -I../runtime` - include paths for runtime headers
- `-lpthread` - pthread library (required for channels and spawn)
- `-o input` - output executable name

**Step 3: Run**

```bash
./input
```

### FFI with POSIX Functions

If your Ion program uses FFI names that conflict with Ion keywords (`recv`, `send`), add `-D` flags when linking:

```bash
gcc http_server.c runtime/ion_runtime.c -o http_server \
    -I. -I.. -Iruntime -I../runtime -lpthread \
    -Drecv_sys=recv -Dsend_sys=send -Dclose=closesocket -lws2_32
```

On Linux or macOS, omit `-lws2_32` and `-Dclose=closesocket`. `ion_net_init()` is a no-op outside Windows.

### Multi-File Mode

```bash
./target/release/ion-compiler --mode multi --output myprogram main.ion
```

This parses imported modules, generates `.c` and `.h` per module, compiles object files, and links with the runtime. The `--output` flag sets the executable name (defaults to the main module name). Multi-file mode handles compilation and linking; you do not need to run `gcc` manually.

## Example Programs

Top-level `examples/*.ion` files each have a checked-in merged `examples/*.c` codegen snapshot. Regenerate after compiler codegen changes:

```bash
for f in examples/*.ion; do ./target/release/ion-compiler "$f"; done
./target/release/ion-compiler examples/text_summary/text_summary.ion
```

Top-level single-file examples (including `channel_worker.ion`) commit a merged `.c` snapshot next to the `.ion`. The `text_summary/` subdirectory also commits one `.c` (it needs `sample.txt`). Multi-file `examples/data_lib/` keeps only `.ion` sources; see [examples/data_lib/README.md](examples/data_lib/README.md) for build output (`.c`/`.h` generated in place, not committed).

Compile and run any single-file example:

```bash
./target/release/ion-compiler examples/spawn_channel.ion
gcc examples/spawn_channel.c runtime/ion_runtime.c -o spawn_channel \
    -I. -Iruntime -lpthread -lws2_32   # omit -lws2_32 on Linux/macOS
./spawn_channel
```

For `http_server.ion`, add `-Drecv_sys=recv -Dsend_sys=send` when linking.

| File | What it demonstrates |
|------|---------------------|
| [examples/hello_world.ion](examples/hello_world.ion) | Minimal FFI `write()` to stdout |
| [examples/hello_world_safe.ion](examples/hello_world_safe.ion) | stdlib `io` module |
| [examples/spawn_channel.ion](examples/spawn_channel.ion) | `spawn` with cross-thread channels |
| [examples/http_server.ion](examples/http_server.ion) | Sockets, FFI, concurrent clients via `spawn` |
| [examples/showcase.ion](examples/showcase.ion) | Mixed language features: tuples, `+=`, `push_byte`, spawn/channels, capture-free fn literals |
| [examples/access_log.ion](examples/access_log.ion) | Log parsing, `loop`/`break`, match guards, spawn, channels, fmt/io |
| [examples/minimal.ion](examples/minimal.ion) | Smallest valid program |
| [examples/channel_worker.ion](examples/channel_worker.ion) | Channel worker: `spawn` sums jobs from a channel |
| [examples/text_summary/text_summary.ion](examples/text_summary/text_summary.ion) | `fs` file read, string iteration, line/word/byte counts |
| [examples/data_lib/main.ion](examples/data_lib/main.ion) | Multi-module library (`catalog.ion`); see [data_lib/README.md](examples/data_lib/README.md) |

Build `data_lib` (multi-file):

```powershell
cd examples\data_lib
..\..\target\release\ion-build.exe build
.\target\data_lib.exe
```

Or with `ion-compiler` directly (emits `.c`/`.h` in the current directory):

```bash
cd examples/data_lib
../../target/release/ion-compiler --mode multi --output data_lib main.ion
./data_lib
```

## Project Structure

```
.
├── src/
│   ├── lexer/      # Tokenizer
│   ├── parser/     # AST construction
│   ├── ast/        # AST node definitions
│   ├── compiler/   # Compilation driver (module resolution, cycle detection)
│   ├── tc/         # Type Checker (safety checks, visibility, qualified names)
│   ├── ir/         # Intermediate representation
│   └── cgen/       # C code generator
├── runtime/        # C runtime headers
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
