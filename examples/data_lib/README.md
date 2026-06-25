# data_lib

Multi-module example: `catalog.ion` (library) and `main.ion` (entry).

```powershell
cd examples\data_lib
..\..\target\release\ion-build.exe build
.\target\data_lib.exe
```

Expected output:

```
catalog report
lines: 3
units: 17
total cents: 5140
```

Build artifacts land in `examples/data_lib/target/` by default (not committed).

For manual multi-file codegen in the source directory:

```bash
../../target/release/ion-compiler --mode multi --output data_lib main.ion
```
