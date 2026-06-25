# Verified Ion Patterns

Copy patterns from here or from linked test files. Do not add constructs not shown in ION_SPEC.md or this repo.

## Types

Primitives: `int`, `bool`, `f32`, `f64`, `i8`..`i64`, `u8`..`u64`, `*T`.

Collections: `Vec<T>`, `String`, `Box<T>`, `[T; N]`, `[]T` (slice).

Channels: `Sender<T>`, `Receiver<T>` from `channel<T>()`.

Function types (named functions only): `let f: fn(int) -> int = add;`

## Struct literal

```ion
let p: Point = Point { x: 1, y: 2 };
```

## Enum variants

Tuple: `Option::Some(42)`, `Option::None`.

Struct: `Status::Ok { value: 10 }`.

Match:

```ion
match opt {
    Option::Some(v) => { return v; }
    Option::None => { return 0; }
};
```

Guard: `Option::Some(v) if v > 0 => { ... }`

## Vec and String

```ion
let mut v: Vec<int> = Vec::new();
v.push(1);
let n: int = v.len();

let mut s: String = String::new();
s.push_str("hi");
let lit: String = "hello";
```

`Vec::get` / `Vec::pop` return `Option<T>`. `Vec::set(&mut v, index, value)` returns `int` (0 = ok).

## Box

```ion
let b: Box<int> = Box::new(42);
let x: int = Box::unwrap(b);
```

## Arrays and slices

```ion
let arr: [int; 3] = [1, 2, 3];
let fill: [u8; 4] = [0; 4];
let slice: []int = arr;  // array to slice coercion
let elem: int = arr[0];  // bounds checked unless unsafe
```

## Borrowing (same function only)

```ion
fn bump(x: &mut int) {
    *x = *x + 1;
}

fn read_len(v: &Vec<int>) -> int {
    return v.len();
}
```

While `&mut s` is active, do not move or assign `s` or other fields of the same root owner.

## defer

```ion
defer expr;  // runs at scope exit, LIFO order
```

See [tests/test_defer_basic.ion](../../../../tests/test_defer_basic.ion).

## Compound assignment

`sum += v` desugars to `sum = sum + v` for supported `+` types.

## Type cast

`let b: u8 = (48 + digit) as u8;`

## Module layout

**math.ion** (library):

```ion
pub fn add(a: int, b: int) -> int {
    return a + b;
}
```

**main.ion**:

```ion
import "math.ion" as math;

fn main() -> int {
    return math::add(1, 2);
}
```

Non-`pub` items are file-private. Qualified names use `alias::item`.

Multi-file build (`ion-build` with `mode = "multi"` in `ion.toml`):

```powershell
cd examples\data_lib
..\..\target\release\ion-build.exe build
```

Manual codegen only (emits `.c`/`.h` in cwd):

```bash
./target/release/ion-compiler --mode multi --output app path/to/main.ion
```

Multi-file mode prefixes each module's C symbols (`io_print_int`, `fmt_print_int`) so stdlib modules can link together. The compiler walks up from the current directory to find `runtime/`. Spawn captures move by value even for `int`; use a helper function if the same constant is needed after `spawn` (see `examples/channel_worker.ion`).

## Channel send expressions

`send(&tx, make())` is valid ([tests/test_channel_send_call_expr.ion](../../../../tests/test_channel_send_call_expr.ion)).

Expression form `tx<-value` exists in grammar; prefer `send(&tx, value)` to match tests and examples.

## if / ownership merge

A move in an `if` branch that always `return`s does not block use after the `if`. Two fall-through branches must agree on whether a binding is still valid.

Do not move a non-copy binding inside a `while` or `for` body (next iteration would need it again).

## extern block

```ion
extern "C" {
    fn write(fd: int, buf: *u8, count: int) -> int;
}

fn main() -> int {
    unsafe {
        let _n: int = write(1, "ok\n", 3);
    }
    return 0;
}
```

## Rejected patterns (compile errors)

Do not write these expecting them to work:

```ion
// return reference
fn bad() -> &int { ... }

// reference in struct
struct S { r: &int; }

// channel of references
channel<&int>()

// spawn captures reference
spawn { use_ref(&x); };

// use after move
let v = Vec::new();
let w = v;
let _ = v.len();
```

See [tests/test_ref_return_error2.ion](../../../../tests/test_ref_return_error2.ion), [tests/test_spawn_ref_error.ion](../../../../tests/test_spawn_ref_error.ion), and ION_SPEC.md section 9.4.

## Design alternatives (no borrowed returns)

When another language would return `&T`:

- Return an owned value or `Option<T>` by move
- Return an index or handle, caller re-indexes locally
- Pass a callback that receives `&T` inside the callee's stack frame

See ION_SPEC.md section 12 (non-normative).
