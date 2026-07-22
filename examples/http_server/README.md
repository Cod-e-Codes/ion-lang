# http_server

Sockets FFI demo on port 8080. Accepts clients in a spawned thread; type `quit` on stdin to close the listen socket and exit.

```powershell
cd examples\http_server
..\..\target\release\ion-build.exe build
.\target\http_server.exe
```

In another terminal: `curl http://127.0.0.1:8080/`

`ion.toml` maps `recv`/`send` to `recv_sys`/`send_sys` via `cflags`. On Windows only, `cflags_windows` maps `close` to `closesocket` for Winsock.

Build artifacts land in `target/` (not committed).
