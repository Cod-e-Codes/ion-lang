# todo_demo

Interactive todo list: `Vec<Todo>`, struct + `String`, move-only rebuild updates, stdin commands.

## Build and run

From repo root (after `cargo build --release --bin ion-build`):

```powershell
cd examples\todo_demo
..\..\target\release\ion-build.exe build
.\target\todo_demo.exe
```

## Commands

At the `todo>` prompt:

| Command | Action |
|---------|--------|
| `help` | Show command list |
| `list` | Print all todos |
| `add <label>` | Add a todo (auto-numbered id) |
| `done <id>` | Mark a todo done |
| `remove <id>` | Delete a todo |
| `quit` | Exit |

Example session:

```
todo> add buy milk
added
todo> add walk the dog
added
todo> list
1: buy milk
2: walk the dog
todo> done 1
marked done
todo> remove 2
removed
todo> list
1: buy milk [done]
todo> quit
```

Build artifacts land in `examples/todo_demo/target/` (not committed).
