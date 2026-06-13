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
- **Stdlib**: `stdlib/io.ion` and `stdlib/fmt.ion` for safe stdout output

Known limitations: [ION_SPEC.md section 10.2](ION_SPEC.md#102-known-limitations).

## Documentation

| Resource | Contents |
|----------|----------|
| [ION_SPEC.md](ION_SPEC.md) | Language semantics, grammar, stdlib contracts |
| [tests/README.md](tests/README.md) | Integration test catalog |
| [examples/](examples/) | Runnable example programs |

## Building

### Prerequisites

- Rust toolchain (stable or nightly)
- A C compiler (GCC, Clang, or MSVC)

### Build the Compiler

```bash
cargo build --release
```

The binary will be at `target/release/ion-compiler` (or `target/release/ion-compiler.exe` on Windows).

## IDE Support

### VS Code Extension

Ion has a VS Code / Cursor extension that provides:
- Syntax highlighting
- Real-time diagnostics (syntax and type errors)
- Hover: variable types at use sites; symbol docs at definitions
- Completion: keywords, builtins, and file symbols
- Go to definition: variables only (same file)

Limitations: no hover on `let` bindings; function calls do not go to definition; completion has no prefix filtering. Full list in [ION_SPEC.md section 10.2](ION_SPEC.md#102-known-limitations).

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

## Usage

### Quick Start

```bash
# Compile Ion to C
./target/release/ion-compiler examples/hello_world.ion

# Compile C to executable (from project root)
gcc examples/hello_world.c runtime/ion_runtime.c -o hello_world \
    -I. -I.. -Iruntime -I../runtime -lpthread

# Run
./hello_world
```

Source: [examples/hello_world.ion](examples/hello_world.ion). For stdlib-based I/O, see [examples/hello_world_safe.ion](examples/hello_world_safe.ion).

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

Compile and run any example with the same pattern as the quick start above. For `http_server.ion`, add `-Drecv_sys=recv -Dsend_sys=send` when linking.

| File | What it demonstrates |
|------|---------------------|
| [examples/hello_world.ion](examples/hello_world.ion) | Minimal FFI `write()` to stdout |
| [examples/hello_world_safe.ion](examples/hello_world_safe.ion) | stdlib `io` module |
| [examples/spawn_channel.ion](examples/spawn_channel.ion) | `spawn` with cross-thread channels |
| [examples/http_server.ion](examples/http_server.ion) | Sockets, FFI, concurrent clients via `spawn` |
| [examples/showcase.ion](examples/showcase.ion) | Mixed language features (includes spawn/channels) |
| [examples/access_log.ion](examples/access_log.ion) | Log parsing, match guards, spawn, channels, fmt/io |
| [examples/minimal.ion](examples/minimal.ion) | Smallest valid program |

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

Integration tests:

```bash
./tests/test_runner.sh
```

The runner compiles Ion programs and checks exit codes or compile errors. See [tests/README.md](tests/README.md) for the test catalog.

### Linting

```bash
cargo clippy
```

## License

MIT
