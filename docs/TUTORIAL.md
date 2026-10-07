# A first Ion program

This writes one file, builds it with `ion-build`, and runs it. The program is the same shape as [examples/hello_world_safe/hello_world_safe.ion](../examples/hello_world_safe/hello_world_safe.ion).

## Source

Create a directory and `hello.ion`:

```ion
import "stdlib/io.ion" as io;

fn main() -> int {
    io::println("Hello, World!");
    return 0;
}
```

`main` returns `int`. `0` is success. `io::println` takes an owned `String`. The string literal is copied into that parameter.

## Manifest

`ion-build` reads `ion.toml` in the same directory:

```toml
name = "hello"
main = "hello.ion"
output = "hello"
mode = "single"
out_dir = "target"
```

`stdlib/` and `runtime/` must be in a parent directory (the Ion repo, or an unpacked release). Run the commands below from the directory that contains `ion.toml`.

## Build and run

Bash:

```bash
ion-build build
./target/hello
```

PowerShell:

```powershell
ion-build.exe build
.\target\hello.exe
```

From a clone, before `ion-build` is on `PATH`:

```bash
cargo build --release --bin ion-build
./target/release/ion-build build
./target/hello_world
```

```powershell
cargo build --release --bin ion-build
.\target\release\ion-build.exe build
.\target\hello_world.exe
```

Those last two commands use the repository root `ion.toml`, which builds `examples/hello_world_safe` and writes `target/hello_world`.

## Enums you declare yourself

There is no prelude. Import `stdlib/option.ion` for `Option` and `stdlib/result.ion` for `Result`, or declare those enums in the file. Do not declare `Option` again in a file that imports `option.ion`.

`SendResult`, `TrySendResult`, `TryRecvResult`, and `SetResult` are not standard-library exports. Declare them in the program when you use channels or `Vec::set`. The shapes are in [ION_SPEC.md](../ION_SPEC.md) Section 7.2 and Section 8.2.

Copy-paste patterns for moves, `Vec`, and channels are in [verified-patterns.md](verified-patterns.md).
