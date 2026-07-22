# Verified Ion Patterns

**Canonical idiom reference** for agents and authors. [ION_SPEC.md](../../../../ION_SPEC.md) §12 indexes this file; keep long examples here, not in the spec. Every `ion` block should match a pattern in `tests/` or `examples/` (linked inline). When semantics change, update this file and tests together.

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
fn owned(s: String) -> int { return s.len(); }
// owned("hello") is valid; literals coerce to owned String at call sites too.
```

`Vec::get` / `Vec::pop` return `Option<T>` and **move** the element out. For read-only scans use `Vec::get_ref(&v, i)` which returns `Option<&T>` (local temporary only; cannot return or store). In `match Option::Some(x)`, `x` is `&T`: copy types bind by value; structs and enums with non-copy fields bind as a pointer. Inner `match x` on `&Enum` dispatches variants without deref. `Vec::set(&mut v, index, value)` returns `int` (0 = ok). Method syntax (`v.push(x)`, `v.get_ref(i)`) desugars to `Vec::` builtins with correct receiver borrows.

## Struct field mutation

```ion
struct VM { ip: int; stack: Vec<int>; }

fn step(vm: &mut VM) {
    vm.stack.push(1);
    vm.ip += 1;
}
```

Field paths are valid assignment targets on owned structs and `&mut` parameters.

## Ownership move

```ion
struct Packet {
    id: i32;
    data: Vec<u8>;
}

fn make_packet(data: Vec<u8>) -> Packet {
    Packet { id: 1, data: data }
}

fn main() -> int {
    let buf: Vec<u8> = Vec::new();
    let pkt = make_packet(buf);
    // buf is invalid after the move; pkt owns the data
    return 0;
}
```

See [tests/test_move_basic.ion](../../../../tests/test_move_basic.ion).

## Index and handle search

When another language would return `&T`, return an index (`int`) or handle and re-index locally. Read-only scans use `Vec::get_ref` (does not hollow the vector). See [tests/test_vec_search_index_ok.ion](../../../../tests/test_vec_search_index_ok.ion) and [tests/test_vec_get_ref_scan.ion](../../../../tests/test_vec_get_ref_scan.ion).

```ion
enum Option<T> {
    Some(T);
    None;
}

struct Customer {
    id: int;
    active: int;
}

fn find_customer_index(customers: &Vec<Customer>, target_id: int) -> int {
    let mut i: int = 0;
    let len: int = Vec::len(customers);
    while i < len {
        match Vec::get_ref(customers, i) {
            Option::Some(c) => {
                if c.id == target_id {
                    return i;
                }
            }
            Option::None => {
                return -1;
            }
        };
        i = i + 1;
    }
    return -1;
}

fn main() -> int {
    let mut customers: Vec<Customer> = Vec::new();
    let idx: int = find_customer_index(&customers, 42);
    if idx >= 0 {
        match Vec::get(&customers, idx) {
            Option::Some(c) => {
                let id: int = c.id;
                Vec::set(
                    &mut customers,
                    idx,
                    Customer {
                        id: id,
                        active: 1,
                    },
                );
            }
            Option::None => {}
        };
    }
    return 0;
}
```

## Mutating Vec elements

`Vec::get` moves the element out. Copy fields, rebuild the struct, and `Vec::set` it back ([tests/test_vec_get_putback.ion](../../../../tests/test_vec_get_putback.ion)). Helpers can take `&mut T` on an owned local:

```ion
fn mark_active(c: &mut Customer) {
    c.active = 1;
}
```

## Comparing borrowed structs

Read fields through `&Struct` parameters. Ion does not allow reference fields in structs.

```ion
struct Customer {
    id: int;
    score: int;
}

fn compare(a: &Customer, b: &Customer) -> int {
    if a.score != b.score {
        return a.score - b.score;
    }
    return a.id - b.id;
}
```

## Concurrency and ownership transfer

Move owned values into `spawn` and channels; no shared mutable state across threads. See [examples/spawn_channel/spawn_channel.ion](../../../../examples/spawn_channel/spawn_channel.ion) and [examples/channel_worker/channel_worker.ion](../../../../examples/channel_worker/channel_worker.ion).

```ion
struct Job {
    data: Vec<u8>;
}

fn worker(rx: Receiver<Job>) {
    let mut rx_mut: Receiver<Job> = rx;
    loop {
        let job: Job = recv(&mut rx_mut);
        // use job.data
    }
}

fn main() -> int {
    let (tx, rx): (Sender<Job>, Receiver<Job>) = channel<Job>();

    spawn {
        worker(rx);
    };

    let job = Job { data: Vec::new() };
    send(&tx, job);
    return 0;
}
```

## Owned API boundaries

At module exports and public functions, prefer owned results (`Vec<T>`, `String`, structs with owned fields) over borrowed views. Use `&T`, `&str`, and local `Option<&T>` from `Vec::get_ref` inside a single function only.

## VM dispatch loop

See [tests/test_vm_execute.ion](../../../../tests/test_vm_execute.ion): `match vm.code.get_ref(vm.ip)` then inner `match op` on `&Op`, field updates, and `break` inside `match` within `loop`.

## Box

```ion
let b: Box<int> = Box::new(42);
let x: int = Box::unwrap(b);
```

## Arrays and slices

```ion
let arr: [int; 3] = [1, 2, 3];
let fill: [u8; 4] = [0; 4];
let s: &[]int = &arr;  // array-to-slice coercion via borrow
let elem: int = arr[0];  // bounds checked unless unsafe
```

## Borrowing (same function only)

```ion
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

Multi-file mode prefixes each module's C symbols (`io_print_int`, `fmt_print_int`) so stdlib modules can link together. The compiler walks up from the current directory to find `runtime/`. Spawn captures move by value even for `int`; use a helper function if the same constant is needed after `spawn` (see `examples/channel_worker/channel_worker.ion`).

## Channel send expressions

`send(&tx, make())` is valid ([tests/test_channel_send_call_expr.ion](../../../../tests/test_channel_send_call_expr.ion)). Use `send(&tx, value)` and `recv(&mut rx)` (see [examples/spawn_channel/spawn_channel.ion](../../../../examples/spawn_channel/spawn_channel.ion)).

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
let v: Vec<int> = Vec::new();
let w = v;
let _ = v.len();
```

See [tests/test_ref_return_error2.ion](../../../../tests/test_ref_return_error2.ion), [tests/test_spawn_ref_error.ion](../../../../tests/test_spawn_ref_error.ion), and ION_SPEC.md §9.4.

## Design alternatives (no borrowed returns)

When another language would return `&T`:

- Return an owned value or `Option<T>` by move
- Return an index or handle; caller re-indexes locally (see **Index and handle search** above)
- Call a helper with `&mut T` on an owned local after `Vec::get` / before `Vec::set`
