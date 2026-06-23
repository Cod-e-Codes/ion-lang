# data_lib

Multi-module example: `catalog.ion` (library) and `main.ion` (entry). Uses `--mode multi`, which generates `.c`, `.h`, and `.o` files in this directory at build time. Those artifacts are not committed; only the `.ion` sources are.

```bash
cd examples/data_lib
../../target/release/ion-compiler --mode multi --output data_lib main.ion
./data_lib    # data_lib.exe on Windows
```

Expected output:

```
catalog report
lines: 3
units: 17
total cents: 5140
```
