# Ion Compiler

A source-to-source transpiler from Ion to C. Ion is transpiled to human-readable C, then compiled with a C compiler.

## What is Ion?

Ion is a systems programming language with:
- Move-only ownership, no GC
- Stack-only, no-escape references (`&T`, `&mut T`)
- Channels-only concurrency with OS threads and structural `Send` checking
- C backend: Ion is transpiled to human-readable C, then compiled with a C compiler

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

Ion has a VS Code extension that provides:
- Syntax highlighting
- Real-time diagnostics (syntax and type errors)

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
   npx vsce package
   code --install-extension ion-language-0.1.0.vsix
   ```

## Usage

### Single-File Mode (Default)

**Step 1: Compile Ion to C**

```bash
./target/release/ion-compiler input.ion
```

This generates `input.c` in the same directory as the source file.

**Step 2: Compile C to Executable**

The generated C code requires the Ion runtime library. Compile with:

```bash
gcc input.c runtime/ion_runtime.c -o input -I. -I.. -Iruntime -I../runtime -lpthread
```

**Required flags:**
- `input.c` - Your generated C file
- `runtime/ion_runtime.c` - Ion runtime library (when running from project root)
- `-I. -I.. -Iruntime -I../runtime` - Include paths for runtime headers
- `-lpthread` - Link pthread library (required for channel support)
- `-o input` - Output executable name

**Step 3: Run**

```bash
./input
```

**Complete Example:**

```bash
# Compile Ion to C
./target/release/ion-compiler hello.ion

# Compile C to executable
gcc hello.c runtime/ion_runtime.c -o hello -I. -I.. -Iruntime -I../runtime -lpthread

# Run
./hello
```

### Special Cases: FFI with POSIX Functions

If your Ion program uses FFI functions that conflict with Ion keywords (like `recv` and `send`), you'll need to add `-D` flags to map them:

```bash
# Example: HTTP server using recv_sys and send_sys
gcc http_server.c runtime/ion_runtime.c -o http_server \
    -I. -I.. -Iruntime -I../runtime -lpthread \
    -Drecv_sys=recv -Dsend_sys=send
```

The `-D` flags create preprocessor macros that map your Ion function names to the actual POSIX function names.

### Multi-File Mode

Compile multiple Ion files with separate `.c` and `.h` generation:

```bash
./target/release/ion-compiler --mode multi --output myprogram main.ion
```

This will:
1. Parse all imported modules recursively
2. Generate `.c` and `.h` files for each module
3. Compile each `.c` file to a `.o` object file
4. Link all object files with the Ion runtime into an executable

The `--output` flag specifies the name of the final executable (defaults to the main module name).

**Note:** Multi-file mode automatically handles compilation and linking, so you don't need to manually run `gcc`.

## Language Features

### Types

#### Primitive Types
- **Integers**: `int` (signed, platform-dependent), `i8`, `i16`, `i32`, `i64`, `u16`, `u32`, `u64`, `uint` (unsigned int)
- **Floating-point**: `f32` (32-bit float), `f64` (64-bit double)
- **Boolean**: `bool` with literals `true` and `false`
- **Type promotion**: Automatic coercion between compatible numeric types (e.g., `i8` promotes to `i64`, `f32` promotes to `f64`)

#### Type Aliases
- Simple type aliases: `type Name = T;`
- Generic type aliases: `type Result<T> = Option<T>;` with parameter substitution

#### Composite Types
- **Structs**: `struct Point { x: int; y: int; }` with field access (`.field`)
- **Enums**: Tagged unions with tuple-style or struct-style variants
  - Tuple variants: `enum Option { Some(int); None; }`
  - Struct variants: `enum Result { Ok { value: int }; Err { message: String }; }`
- **Arrays**: Fixed-size arrays `[T; N]` (e.g., `[int; 5]`)
- **Slices**: Dynamically sized slices `[]T` (e.g., `[]int`)
- **References**: `&T` and `&mut T` with no-escape rule
- **Raw pointers**: `*T` for FFI (distinct from safe references)

#### Generics
- Monomorphization-based generics for `struct`, `enum`, and `fn`
- Examples: `Vec<int>`, `Box<Point>`, `Option<T>`

### Control Flow

- **Conditionals**: `if expr { ... } else { ... }` (condition must be `bool`)
- **Loops**: 
  - `while expr { ... }` (condition must be `bool`)
  - `for identifier in expr { ... }` (iterates over `Vec<T>`, `String`, or `[T; N]`)
- **Pattern Matching**: `match expr { pattern => block, ... }` with exhaustiveness checking
  - Supports enum variant patterns: `Option::Some(_)`, `Option::None`
  - Supports struct variant patterns: `Result::Ok { value: v }`, `Result::Err { message: _m }`
  - Supports wildcard patterns: `_`
  - Supports binding patterns: `x`
  - Supports nested patterns in variant payloads
- **Defer**: `defer expr;` at function scope for cleanup

### Operators

- **Arithmetic**: `+`, `-`, `*`, `/`, `%` (modulo, Phase 8)
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=` (Phase 8)
- **Logical**: `&&`, `||`, `!`
- **Bitwise** (Phase 7): `&` (AND), `|` (OR), `^` (XOR), `<<` (left shift), `>>` (right shift)
  - Operands must be integer types
  - Shift amount must be unsigned integer
  - Standard precedence: shifts before bitwise AND/XOR/OR
  - Critical for network programming and binary protocols

### Method Call Syntax (Phase 5)

Ion supports method call syntax as syntactic sugar for calling functions where the first argument is implicitly the receiver:

- **Syntax**: `expr.method_name(args...)` desugars to `Type::method_name(expr, args...)`
- **Automatic borrowing**: The compiler automatically creates `&mut` or `&` references as needed based on the function signature
- **Built-in methods**: Works with `Vec<T>`, `String`, and other built-in types
- **Generic support**: Fully supports generic methods with type inference
- **Backward compatible**: Qualified function call syntax (`Vec::push(&mut vec, 10)`) still works

Example:
```ion
let mut vec: Vec<int> = Vec::new();
vec.push(10);           // Method syntax (preferred)
Vec::push(&mut vec, 20); // Qualified syntax (still valid)

let len: int = vec.len();
match vec.pop() {
    Option::Some(value) => { /* ... */ }
    Option::None => {}
};
```

### Memory Management

#### Stack Allocation
- `let` bindings with optional type annotations
- Move semantics: values are moved on assignment and return
- References: `&T` and `&mut T` with a **no-escape** rule (cannot be returned, stored in longer-lived data, sent over channels, or captured by `spawn`)

#### Heap Allocation
- **Box<T>**: Single-owner heap allocation
  - `Box::new(value)` - allocate and move value to heap
  - `Box::unwrap(box)` - move value out of box
- **Vec<T>**: Dynamic arrays
  - `Vec::new()` - create empty vector (or `vec.new()` with method syntax)
  - `Vec::with_capacity(n)` - create with initial capacity
  - `vec.push(value)` - append element (method syntax) or `Vec::push(&mut vec, value)`
  - `vec.pop()` - remove and return last element (method syntax) or `Vec::pop(&mut vec)`
  - `vec.len()` - get length (method syntax) or `Vec::len(&vec)`
  - `Vec::capacity(&vec)` - get capacity
  - `vec.get(index)` - get element by index (returns `Option<T>`, method syntax) or `Vec::get(&vec, index)`
  - `vec.set(index, value)` - set element by index (method syntax) or `Vec::set(&mut vec, index, value)`
- **String**: Heap-allocated UTF-8 strings
  - `String::new()` - create empty string (or `s.new()` with method syntax)
  - `String::from("...")` - create from string literal
  - `s.push_str("...")` - append string (method syntax) or `String::push_str(&mut s, "...")`
  - `s.len()` - get byte length (method syntax) or `String::len(&s)`
  - String has `data` (*u8) and `len` (int) fields accessible via `.` syntax

#### Standard Library

Ion includes a safe standard library for common operations:

- **I/O Module** (`stdlib/io.ion`):
  - `io::print(s: String)` - Print string to stdout
  - `io::println(s: String)` - Print string with newline to stdout
  - `io::print_str(s: *u8, len: int)` - Print raw string with length validation
  - `io::print_int(n: int)` - Print integer (TODO: complete implementation)
  - All I/O functions use safe wrappers around POSIX `write()` syscall
  - Example: `import "stdlib/io.ion" as io; io::println(String::from("Hello!"));`

#### Arrays and Slices
- Fixed-size arrays: `[T; N]` syntax (e.g., `[int; 5]`)
- Dynamically sized slices: `[]T` syntax (e.g., `[]int`)
- Array literals: `[1, 2, 3]` syntax (explicit element listing)
- **Array initialization**: `[value; count]` syntax for repeated values (Phase 7)
  - Example: `let buffer: [u8; 128] = [0; 128];`
  - Works in variable declarations, struct fields, and function returns
- **Array bounds checking** (Safety Enhancement):
  - Indexing `arr[i]` performs runtime bounds checking by default
  - If `i < 0` or `i >= array_length`, the program panics with "Array index out of bounds"
  - Bounds checking can be disabled in `unsafe` blocks for performance-critical code
  - Example: `unsafe { let x = arr[i]; }` skips bounds checking
- **Array element assignment** (Phase 8): `arr[i] = value` syntax for mutating array elements
  - Example: `let mut arr: [int; 5] = [0; 5]; arr[0] = 10;`
  - Requires mutable array variables (`let mut`)
  - Subject to bounds checking (panics on out-of-bounds access)
- Implicit array-to-slice coercion: `&[T; N]` automatically coerces to `&[]T` in function calls

### Modules and Visibility

- **Module System**: Multi-file programs with `import "file.ion" as name;`
  - Relative import paths (`./file.ion`, `../file.ion`) with mandatory `.ion` extension
  - Import cycle detection
  - Qualified name resolution: `mod::item` syntax for accessing items from imported modules
- **Visibility Control**: `pub` keyword for functions, structs, and enums
  - Non-public items are only accessible within the same file
  - Public items can be accessed from other modules via qualified names

### Foreign Function Interface (FFI)

- `extern "C" { fn name(...) -> ...; }` blocks for declaring C functions
- Raw pointer types `*T` for FFI (distinct from safe references `&T`)
- Automatic string literal to `*u8` conversion for extern function calls
- C function prototypes are emitted in generated code
- Variadic functions: `fn printf(format: *u8, ...) -> int;` syntax
- Extern function calls require `unsafe` blocks

### Safety and Unsafe Code

- **Static Safety Checks**:
  - Undefined variables, use-after-move
  - No-escape for references (including struct fields)
  - Structural `Send` for channels and `spawn`
  - **Array bounds checking**: Runtime checks on array indexing by default
- **Explicit Unsafe Blocks**: `unsafe { ... }` blocks for low-level operations
  - Raw pointer dereferencing (`*ptr`) only allowed in unsafe blocks
  - Raw pointer arithmetic (`ptr + offset`) only allowed in unsafe blocks
  - Extern function calls require unsafe context
  - **Array bounds checking disabled**: `arr[i]` skips runtime checks in unsafe blocks
  - Core safety (no-escape, Send checking) still enforced even in unsafe
- **Runtime Panic**: Out-of-bounds array access triggers `ion_panic()` which prints an error message and aborts the program

### Concurrency

- **Channels**: Split channel API with `Sender<T>` and `Receiver<T>` types (Phase 6)
  - `let (tx, rx) = channel<T>();` - create a channel, returns `(Sender<T>, Receiver<T>)` tuple
  - `send(&tx, value)` - send value over channel (requires `&Sender<T>`)
  - `recv(&mut rx)` - receive value from channel (requires `&mut Receiver<T>`)
  - Both `Sender<T>` and `Receiver<T>` are move-only types
  - Tuple destructuring: `let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();`
- **Spawn**: `spawn { ... };` with structural `Send` checking
  - Checks that captured values are `Send`
  - Moves captured values out of the parent scope
- Structural `Send` checking ensures type safety for concurrent operations

### Compilation Modes

- **Single-file mode** (default): generates one `.c` file
- **Multi-file mode** (`--mode multi`): generates separate `.c` and `.h` files per module
  - Header generation for `pub` items (functions, structs, enums)
  - Automatic object file compilation (`.c` → `.o`) and linking orchestration
  - Proper include path handling for cross-module dependencies

### Phase 7 Features (Complete)

- **Array Initialization Syntax**: `[value; count]` for initializing arrays with repeated values
  - Example: `let buffer: [u8; 128] = [0; 128];`
  - Works in variable declarations, struct fields, and function returns
  - Supports type coercion (e.g., `int` to `u8` for array elements)
- **Bitwise Operators**: Full support for bitwise operations
  - `&` (AND), `|` (OR), `^` (XOR), `<<` (left shift), `>>` (right shift)
  - Standard operator precedence: shifts before bitwise AND/XOR/OR
  - Requires integer operands; shift amount must be unsigned
  - Critical for network programming, binary protocols, and low-level systems code
- **Complete Escape Sequence Support**: All standard C escape sequences
  - `\r` (carriage return), `\t` (tab), `\0` (null terminator)
  - `\n` (newline), `\\` (backslash), `\"` (double quote), `\'` (single quote)
  - Essential for HTTP protocol implementation (`\r\n` line endings)

### Phase 8 Features (Complete)

- **Type Casting**: `expr as Type` syntax for explicit type conversions between numeric types
  - Example: `let u8_val: u8 = int_val as u8;`
  - Only allows casts between numeric types (integers and floats)
- **Array Element Assignment**: `arr[i] = value` syntax for mutating array elements
  - Example: `let mut arr: [int; 5] = [0; 5]; arr[0] = 10;`
  - Requires mutable array variables
- **Full Comparison Operators**: `<=` and `>=` operators for complete relational comparisons
  - Example: `if a <= b { ... }` and `if b >= a { ... }`

### Not Yet Supported

- Trait bounds on generics
- Pattern matching guards
- Raw pointer dereferencing in Ion code (raw pointers are pass-through only, except in unsafe blocks)

## Examples

### Hello World

A simple "Hello, World!" program:

```ion
// hello_world.ion
extern "C" {
    fn write(fd: int, buf: *u8, count: int) -> int;
}

fn main() -> int {
    unsafe {
        // Write to stdout (fd = 1)
        // String literals are automatically converted to *u8 when passed to extern functions
        let _result: int = write(1, "Hello, World!\n", 14);  // 14 = length of "Hello, World!\n"
    }
    
    return 0;
}
```

**Compile and run:**

```bash
# Compile Ion to C
./target/release/ion-compiler examples/hello_world.ion

# Compile C to executable (from project root)
gcc examples/hello_world.c runtime/ion_runtime.c -o hello_world \
    -I. -I.. -Iruntime -I../runtime -lpthread

# Run
./hello_world
```

**Output:**
```
Hello, World!
```

### HTTP Server (FFI and Systems Programming)

A complete HTTP server demonstrating FFI, socket programming, and network I/O:

```ion
extern "C" {
    fn socket(domain: int, sock_type: int, protocol: int) -> int;
    fn bind(sockfd: &int, addr: &u8, addrlen: int) -> int;
    fn listen(sockfd: &int, backlog: int) -> int;
    fn accept(sockfd: &int, addr: &mut u8, addrlen: &mut int) -> int;
    fn recv_sys(sockfd: &int, buf: &mut u8, len: int, flags: int) -> int;
    fn send_sys(sockfd: &int, buf: *u8, len: int, flags: int) -> int;
    fn close(fd: &int) -> int;
    fn htons(hostshort: u16) -> u16;
}

struct SockAddrInBytes {
    data: [u8; 16];
}

fn create_sockaddr_in(port: u16) -> SockAddrInBytes {
    // Create socket address structure
    // ...
}

fn handle_client(client_fd: int) -> int {
    let mut buffer: [u8; 128] = [...];
    unsafe {
        let received: int = recv_sys(&client_fd, &mut buffer[0], 128, 0);
        // Send HTTP response
        let _sent: int = send_sys(&client_fd, "HTTP/1.1 200 OK\n", 16, 0);
        close(&client_fd);
    }
    return 0;
}

fn main() -> int {
    unsafe {
        let server_fd: int = socket(2, 1, 0);
        // ... bind, listen, accept loop
        while true {
            let client_fd: int = accept(&server_fd, &mut client_addr[0], &mut addrlen);
            let _result: int = handle_client(client_fd);
        }
    }
    return 0;
}
```

**Compile and run:**

```bash
# Step 1: Compile Ion to C
./target/release/ion-compiler examples/http_server.ion

# Step 2: Compile C to executable with FFI flags
gcc examples/http_server.c runtime/ion_runtime.c -o http_server \
    -I. -I.. -Iruntime -I../runtime -lpthread \
    -Drecv_sys=recv -Dsend_sys=send

# Step 3: Run the server
./http_server

# In another terminal, test the server:
curl http://localhost:8080
```

**Note:** The `-Drecv_sys=recv -Dsend_sys=send` flags are required because `recv` and `send` are keywords in Ion, so the example uses `recv_sys` and `send_sys` which need to be mapped to the actual POSIX functions.

This example demonstrates:
- FFI for POSIX socket APIs
- Struct definitions with array fields
- Network programming and HTTP protocol handling
- Real systems programming in Ion

**Compile and run:**

```bash
# Step 1: Compile Ion to C
./target/release/ion-compiler examples/http_server.ion

# Step 2: Compile C to executable with FFI flags
gcc examples/http_server.c runtime/ion_runtime.c -o http_server \
    -I. -I.. -Iruntime -I../runtime -lpthread \
    -Drecv_sys=recv -Dsend_sys=send

# Step 3: Run the server
./http_server

# In another terminal, test the server:
curl http://localhost:8080
```

**Note:** The `-Drecv_sys=recv -Dsend_sys=send` flags are required because `recv` and `send` are keywords in Ion, so the example uses `recv_sys` and `send_sys` which need to be mapped to the actual POSIX functions.

See `examples/http_server.ion` for the complete implementation.

### Basic Struct and Control Flow

```ion
struct Point {
    x: int;
    y: int;
}

fn main() -> int {
    let p: Point = Point { x: 10, y: 20 };
    if p.x > 0 {
        return p.x + p.y;
    } else {
        return 0;
    }
}
```

### Enums and Pattern Matching

```ion
// Tuple-style enum variants
enum Option {
    Some(int);
    None;
}

// Struct-style enum variants
enum Result {
    Ok { value: int };
    Err { message: String };
}

fn main() -> int {
    // Tuple variant
    let x: Option = Option::Some(42);
    match x {
        Option::Some(value) => {
            return value;
        },
        Option::None => {
            return 0;
        },
    };
    
    // Struct variant
    let result: Result = Result::Ok { value: 100 };
    match result {
        Result::Ok { value: v } => {
            return v;
        },
        Result::Err { message: _m } => {
            return 0;
        },
    };
}
```

### Generic Types

```ion
// Generic struct
struct Point<T> {
    x: T;
    y: T;
}

// Generic enum
enum Option<T> {
    Some(T);
    None;
}

fn main() -> int {
    let p: Point<int> = Point { x: 10, y: 20 };
    let opt: Option<int> = Option::Some(42);
    return 0;
}
```

### Function Calls and Loops

```ion
fn add(x: int, y: int) -> int {
    return x + y;
}

fn main() -> int {
    // While loop
    let mut i: int = 0;
    while i < 10 {
        i = i + 1;
    }
    
    // Comparison operators (Phase 8)
    let a: int = 10;
    let b: int = 20;
    if a <= b {  // true
        // ...
    }
    if b >= a {  // true
        // ...
    }
    
    // For loop
    let mut vec: Vec<int> = Vec::new();
    vec.push(10);
    vec.push(20);
    vec.push(30);
    
    for x in vec {
        // x is bound to each element: 10, 20, 30
        let _sum = add(x, 0);
    }
    
    let result: int = add(10, 20);
    return result;
}
```

### Type System

```ion
// Boolean type
fn main() -> int {
    let flag: bool = true;
    if flag {
        return 1;
    }
    return 0;
}

// Floating-point types
fn main() -> int {
    let x: f32 = 3.14;
    let y: f64 = 1.5e-3;
    let sum: f64 = x + y;  // f32 promotes to f64
    return 0;
}

// Additional integer types
fn main() -> int {
    let small: i8 = 42;
    let large: i64 = 1000;
    let result: i64 = small + large;  // i8 promotes to i64
    return 0;
}

// Type casting (Phase 8)
fn main() -> int {
    let a: int = 10;
    let b: u8 = a as u8;  // int to u8
    
    let c: f32 = 3.14;
    let d: int = c as int;  // f32 to int (truncates to 3)
    
    let e: u64 = 10000000000;
    let f: i64 = e as i64;  // u64 to i64
    
    return 0;
}

// Type aliases
type MyInt = int;
type MyFloat = f64;

fn add(a: MyInt, b: MyFloat) -> MyFloat {
    return a + b;  // int promotes to f64
}

// Generic type aliases
enum Option<T> {
    Some(T);
    None;
}

type Result<T> = Option<T>;

fn main() -> int {
    let x: Result<int> = Option::Some(42);
    return 0;
}
```

### Arrays and Slices

```ion
fn main() -> int {
    // Fixed-size array
    let arr: [int; 5] = [10, 20, 30, 40, 50];
    
    // Array indexing
    let first: int = arr[0];  // 10
    let third: int = arr[2];  // 30
    
    // Array literals
    let numbers: [int; 3] = [1, 2, 3];
    
    // Array initialization with repeated values (Phase 7)
    let zeros: [int; 10] = [0; 10];
    let ones: [u8; 5] = [1; 5];
    
    // Array element assignment (Phase 8)
    let mut buffer: [u8; 128] = [0; 128];
    buffer[0] = 65;  // 'A'
    buffer[1] = 66;  // 'B'
    buffer[2] = 67;  // 'C'
    
    let mut arr2: [int; 5] = [0; 5];
    arr2[0] = 10;
    arr2[1] = 20;
    
    return arr[0] + arr[1];  // Returns 30
}
```

### Bitwise Operations (Phase 7)

```ion
fn main() -> int {
    // Network byte order extraction
    let port: u16 = 8080;
    let shift8: u16 = 8;
    let mask: u16 = 255;  // 0xFF
    
    let high_byte: u16 = port >> shift8;   // High byte
    let low_byte: u16 = port & mask;      // Low byte
    
    // Basic bitwise operations
    let a: u8 = 170;  // 0xAA
    let b: u8 = 240;  // 0xF0
    let and_result: u8 = a & b;  // 160 (0xA0)
    let or_result: u8 = a | b;   // 250 (0xFA)
    let xor_result: u8 = a ^ b;  // 90 (0x5A)
    
    return 0;
}
```

### Escape Sequences (Phase 7)

```ion
fn main() -> int {
    // HTTP response with proper line endings
    let response: String = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n";
    
    // Tab-separated values
    let tsv: String = "name\tage\tcity\n";
    
    // Null-terminated strings
    let c_string: String = "Hello\0World";
    
    return 0;
}
```

### Modules and Imports

```ion
// math.ion
pub fn add(a: int, b: int) -> int {
    return a + b;
}

fn helper(x: int) -> int {
    return x;  // Not public - only accessible in this file
}

pub fn multiply(a: int, b: int) -> int {
    return a * b;
}
```

```ion
// main.ion
import "math.ion" as math;

fn main() -> int {
    let result: int = math::add(10, 20);  // Qualified name
    return result;  // Returns 30
}
```

### Foreign Function Interface and Unsafe Blocks

```ion
extern "C" {
    fn write(fd: int, buf: *u8, count: int) -> int;
}

fn main() -> int {
    // String literals are automatically converted to *u8 for extern functions
    // Extern calls require unsafe blocks
    unsafe {
        let _result: int = write(1, "Hello from unsafe\n", 18);
    }
    return 0;
}
```

### Vec and String

```ion
enum Option<T> {
    Some(T);
    None;
}

fn main() -> int {
    // Vec operations with method call syntax (Phase 5)
    let mut v: Vec<int> = Vec::new();
    v.push(10);
    v.push(20);
    
    match v.pop() {
        Option::Some(value) => {
            // value is 20
        }
        Option::None => {}
    };
    
    // String operations with method call syntax (Phase 5)
    let mut s: String = String::new();
    s.push_str("Hello");
    s.push_str(", Ion!");
    let len: int = s.len();
    
    return 0;
}
```

**Note**: Both method call syntax (`vec.push(10)`) and qualified function call syntax (`Vec::push(&mut vec, 10)`) are supported. Method syntax is preferred for better readability.

### For Loops

```ion
fn main() -> int {
    let mut vec: Vec<int> = Vec::new();
    vec.push(10);
    vec.push(20);
    vec.push(30);
    
    // Iterate over vector elements
    for x in vec {
        // x is bound to each element: 10, then 20, then 30
        let _sum = x + 1;
    }
    
    return 0;
}
```

For loops work with `Vec<T>`, `String`, and `[T; N]` types. They are desugared to while loops with index-based iteration internally.

### Split Channel API

```ion
// Create a channel with tuple destructuring
let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();

// Send a value (requires &Sender<T>)
send(&tx, 42);

// Receive a value (requires &mut Receiver<T>)
let mut rx_mut = rx;
let value = recv(&mut rx_mut);

if value == 42 {
    return 0;
} else {
    return 1;
}
```

The split channel API provides separate `Sender<T>` and `Receiver<T>` types that can be moved independently, enabling better concurrency patterns where different threads own different ends of the channel.

### Compile and Run

```bash
# Step 1: Compile Ion to C
./target/release/ion-compiler example.ion

# Step 2: Compile C to executable
gcc example.c runtime/ion_runtime.c -o example \
    -I. -I.. -Iruntime -I../runtime -lpthread

# Step 3: Run
./example
echo $?  # prints exit code
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

Run the unit tests:
```bash
cargo test
```

Run the integration test suite:
```bash
./tests/test_runner.sh
```

**Current test status: 79/79 tests passing**

The test runner compiles Ion programs and verifies they produce the expected exit codes or error messages.

### Linting

```bash
cargo clippy
```

## License

MIT
