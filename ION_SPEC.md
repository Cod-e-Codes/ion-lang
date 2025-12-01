## Ion Language Specification (Phase 8)

### 1. Introduction

Ion is a systems programming language designed for:

- **Ownership and borrowing** with **move-only semantics** and **no-escape references**
- **Memory safety without garbage collection**
- **Message-passing parallelism** via **typed, bounded channels** and OS threads
- **Simple syntax** and predictable performance
- A minimal, C-oriented runtime and ABI

This document defines the core semantics of Ion for Phase 8. Phase 8 extends Phase 7 with type casting, array element assignment, and complete comparison operators. Phase 7 introduced array initialization syntax, bitwise operators, and complete escape sequence support. Phase 6 extended Phase 5 with struct-style enum variants and for loops. Phase 5 introduced method call syntax (Uniform Function Call Syntax) for improved ergonomics. Phase 4 introduced a complete type system including boolean, floating-point, additional integer types, and type aliases. This specification is intended to be:

- Precise enough to guide an implementation (parser, type checker, and C backend)
- Strict about memory and concurrency safety

#### 1.1 Non-Negotiable Constraints

The following constraints are core to Ion’s identity:

- **Move-only, single ownership**
- **No-escape borrowing** (references are stack-local and lexical)
- **Channels-only concurrency**, no shared mutable state across threads
- **No GC**, explicit heap allocation via `Box<T>`
- **C backend first**, with C-compatible layout by default

Violating these constraints is a **compile-time error**, not undefined behavior.

#### 1.2 The No-Escape Rule

Ion references have the forms `&T` (shared) and `&mut T` (exclusive). They are **lexically scoped**, **stack-only views** into owned data. The compiler enforces the **no-escape rule**:

> A reference (`&T` or `&mut T`) may not outlive the lexical scope in which it is created.

Concretely, references **MUST NOT**:

- Be returned from functions
- Be stored in struct fields or enum variants
- Be captured by closures that may escape their defining scope
- Be moved into channels
- Cross thread boundaries (e.g., passed to `spawn`, stored in `Send` values)

Any construct that would require reasoning about reference lifetimes beyond the current function body is rejected at compile time.  This implies that APIs which conceptually return “references” (for example, `Option<&T>`) must instead be expressed in terms of owned values, indices/handles, or callback-style access.

#### 1.3 Ownership Rules (Summary)

1. **Single ownership**: Every value has exactly one owner at a time.
2. **Move by default**: Assignment, passing as arguments, and returning values **moves** ownership.
3. **Lexical borrows**: `&T` and `&mut T` borrows are restricted to the current function’s stack frame.
4. **Channel transfers**: Sending a value through a channel moves ownership into the channel.
5. **No self-referential types**: Types cannot contain references to themselves or into their own fields.

The type checker tracks moves and borrows to prevent use-after-move and use-after-free.

#### 1.4 Concurrency Model (Summary)

- **OS threads only**: `spawn { ... }` creates a native operating system thread.
- **Channels-only communication**: Threads communicate through typed, bounded MPSC channels.
- **No shared mutable state across threads**: Values cross threads only by move, via channels or `spawn` arguments.
- **`Send` property**: Only `Send` types may cross thread boundaries.

#### 1.5 Memory Model (Summary)

- **Stack-first**: Local variables live on the stack by default.
- **Explicit heap**: `Box<T>` allocates `T` on the heap, with deterministic destruction when the box’s owner goes out of scope.
- **No hidden allocations**: All heap allocation is explicit in Ion code.
- **Deterministic destruction**: Values are destroyed at scope end; `defer` ensures deterministic cleanup actions.

### 2. Lexical Structure

#### 2.1 Characters and Source Files

An Ion source file is a sequence of Unicode code points encoded as UTF-8. Identifiers are restricted to ASCII letters, digits, and `_` in this draft for simplicity.

Line terminators are `\n` (LF) or `\r\n` (CRLF). For the purposes of this specification, a **line** ends at a line terminator or at end-of-file.

#### 2.2 Tokens

Lexical analysis divides the input into a sequence of tokens:

- **Identifiers**
- **Keywords**
- **Literals**
- **Operators and delimiters**
- **Comments**

Whitespace and comments are ignored except as separators.

##### 2.2.1 Identifiers

Identifiers name variables, types, functions, and modules.

- The first character must be `A–Z`, `a–z`, or `_`.
- Subsequent characters may be `A–Z`, `a–z`, `0–9`, or `_`.
- Identifiers are case-sensitive.

Examples: `main`, `Packet`, `_tmp1`, `read_file`.

##### 2.2.2 Keywords

The following keywords are reserved and cannot be used as identifiers:

`fn`, `let`, `mut`, `struct`, `enum`, `type`, `box`, `if`, `else`, `while`, `match`, `defer`, `return`, `spawn`, `channel`, `true`, `false`, `nil`, `import`, `as`, `pub`, `extern`, `unsafe`.

Note: The `as` keyword is used both for module aliases (`import "file.ion" as name;`) and for type casting (`expr as Type`).

This list is intentionally small; future additions must justify their complexity.

#### 2.3 Unsafe Blocks and Safety Boundaries

The `unsafe` keyword marks code blocks where Ion's safety guarantees are suspended:

- **Array bounds checking** is disabled for array indexing within `unsafe` blocks
- **Raw pointer dereferencing** is only permitted within `unsafe` blocks  
- **FFI calls** to `extern "C"` functions must be wrapped in `unsafe` blocks

##### Unsafe Contract

Code within `unsafe` blocks must manually uphold the following invariants:

1. **No out-of-bounds array access**: All array indices must be valid
2. **No null pointer dereference**: All pointer dereferences must be to valid memory
3. **No data races**: Concurrent access to shared mutable state is prohibited

Violating these invariants results in **undefined behavior**.

##### Standard Library Safety Boundary

The Ion standard library provides safe wrappers around unsafe FFI operations. User code should:

- Use standard library functions (e.g., `io::print_str`) instead of direct FFI calls
- Only use `unsafe` blocks for performance-critical code with proven correctness
- Document all `unsafe` blocks with safety justifications

**Example: Safe I/O**

```ion
// Safe wrapper (recommended)
import "stdlib/io.ion" as io;

fn main() -> int {
    io::print_str("Hello, World!\n", 14);
    return 0;
}
```

```ion
// Direct FFI (requires unsafe)
extern "C" {
    fn write(fd: int, buf: *u8, count: int) -> int;
}

fn main() -> int {
    unsafe {
        let _result: int = write(1, "Hello, World!\n", 14);
    }
    return 0;
}
```


##### 2.2.3 Literals

Ion supports:

- **Integer literals** (e.g., `0`, `42`) – implemented
- **String literals**: `"..."` with complete escape sequence support (Phase 7)
  - `\r` (carriage return), `\n` (newline), `\t` (tab), `\0` (null)
  - `\\` (backslash), `\"` (double quote), `\'` (single quote)
  - Can be assigned to `String` type
- **Floating-point literals** (e.g., `3.14`, `1e9`, `.5`, `3.`) – implemented (Phase 4)
- **Boolean literals**: `true`, `false` – implemented (Phase 4)

The exact numeric literal grammar is given in the EBNF (Section 3.1).

##### 2.2.4 Operators and Delimiters

Ion uses the following operators:

- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=` (Phase 8)
- Logical: `&&`, `||`, `!`
- Bitwise (Phase 7): `&` (AND), `|` (OR), `^` (XOR), `<<` (left shift), `>>` (right shift)
- Assignment: `=`
- Type casting (Phase 8): `as` keyword for explicit type conversions
- Field access: `.`
- Address-of / borrow: `&` (borrow shared) and `&mut` (borrow exclusive)
- Channel: `<-` (send and receive in expressions)

Delimiters:

- Parentheses: `(`, `)`
- Braces: `{`, `}`
- Brackets: `[`, `]`
- Comma: `,`
- Semicolon: `;`
- Colon: `:`
- Double colon: `::`
- Arrow: `->`

##### 2.2.5 Comments and Whitespace

- Line comment: `//` to end of line.
- Block comments are not defined in Phase 0 (may be added later).

Whitespace (spaces, tabs, newlines) separates tokens but is otherwise insignificant, except in string literals.

### 3. Grammar (EBNF)

This section gives a high-level grammar for Ion, using EBNF-like notation:

- `a | b` means choice.
- `x?` means optional.
- `x*` means zero or more repetitions.
- `x+` means one or more repetitions.
- Terminals are in quotes; non-terminals are in plain text.

This grammar is intentionally simplified and is not tied to a particular parsing strategy.

#### 3.1 Lexical Grammar (Fragments)

The grammar for identifiers and literals is sketched below. Real-world implementations may accept a superset for numeric literals.

```ebnf
letter        = "A"…"Z" | "a"…"z" | "_" ;
digit         = "0"…"9" ;
identifier    = letter , { letter | digit } ;

int_lit       = decimal_lit | hex_lit | bin_lit ;
decimal_lit   = digit , { digit } ;
hex_lit       = "0x" , hex_digit , { hex_digit } ;
bin_lit       = "0b" , bin_digit , { bin_digit } ;
hex_digit     = digit | "A"…"F" | "a"…"f" ;
bin_digit     = "0" | "1" ;

float_lit     = decimal_lit , "." , decimal_lit , [ exponent ] 
              | decimal_lit , exponent ;
exponent      = ( "e" | "E" ) , [ "+" | "-" ] , decimal_lit ;

bool_lit      = "true" | "false" ;

string_lit    = "\"" , { string_char } , "\"" ;
string_char   = /* any character except " and newline, with backslash escapes */ ;
                /* Supported escapes: \r, \n, \t, \0, \\, \", \' */
```

#### 3.2 Modules and Declarations

```ebnf
file             = { import_decl | top_decl } ;

import_decl      = "import" , string_lit , [ "as" , identifier ] , ";" ;

top_decl         = struct_decl
                 | enum_decl
                 | type_alias
                 | fn_decl ;

struct_decl      = "struct" , identifier , struct_body ;

struct_body      = "{" , { field_decl } , "}" ;
field_decl       = identifier , ":" , type_expr , ";" ;

enum_decl        = "enum" , identifier , enum_body ;
enum_body        = "{" , { enum_variant } , "}" ;
enum_variant     = identifier , [ "(" , variant_fields? , ")" | "{" , named_fields? , "}" ] , ";" ;
variant_fields   = variant_field , { "," , variant_field } ;
variant_field    = type_expr ;
named_fields     = named_field , { "," , named_field } ;
named_field      = identifier , ":" , type_expr ;

type_alias       = "type" , identifier , "=" , type_expr , ";" ;

fn_decl          = "fn" , identifier , type_params? , "(" , params? , ")" ,
                   return_type? , block ;

type_params      = "<" , type_param , { "," , type_param } , ">" ;
type_param       = identifier ;  (* Phase 0: no bounds *)

params           = param , { "," , param } ;
param            = identifier , ":" , type_expr ;

return_type      = "->" , type_expr ;
```

In Phase 2, visibility is controlled by the `pub` keyword. Top-level declarations (functions, structs, enums) may be prefixed with `pub` to make them accessible from other modules. Non-public items are only accessible within the same file. Public items can be accessed from other modules via qualified names (`mod::item`).

#### 3.3 Types

```ebnf
type_expr        = func_type
                 | union_type ;

func_type        = "fn" , "(" , type_list? , ")" , "->" , type_expr ;

type_list        = type_expr , { "," , type_expr } ;

union_type       = primary_type , { "|" , primary_type } ;

primary_type     = named_type
                 | box_type
                 | channel_type
                 | array_type
                 | slice_type
                 | generic_type
                 | "(" , type_expr , ")" ;

array_type       = "[" , type_expr , ";" , int_lit , "]" ;
slice_type       = "[" , "]" , type_expr ;

named_type       = identifier , [ "::" , identifier ] ;

box_type         = "Box" , "<" , type_expr , ">" ;

channel_type     = "channel" , "<" , type_expr , ">" ;

generic_type     = identifier , "<" , type_expr , { "," , type_expr } , ">" ;
```

Note: Union types (`A | B`) are reserved for future use and may be restricted or removed; enums are the primary sum type mechanism in Phase 0.

#### 3.4 Statements and Blocks

```ebnf
block            = "{" , { stmt } , "}" ;

stmt             = let_stmt
                 | expr_stmt
                 | if_stmt
                 | while_stmt
                 | for_stmt
                 | match_stmt
                 | return_stmt
                 | defer_stmt
                 | spawn_stmt
                 | unsafe_stmt
                 | ";" ;

unsafe_stmt      = "unsafe" , block ;

let_stmt         = "let" , identifier , [ ":" , type_expr ] ,
                   [ "=" , expr ] , ";" ;

expr_stmt        = expr , ";" ;

if_stmt          = "if" , expr , block , [ "else" , ( block | if_stmt ) ] ;

while_stmt       = "while" , expr , block ;

for_stmt         = "for" , identifier , "in" , expr , block , ";" ;

match_stmt       = "match" , expr , "{" , { match_arm } , "}" ;
match_arm        = pattern , "=>" , block ;

return_stmt      = "return" , [ expr ] , ";" ;

defer_stmt       = "defer" , expr , ";" ;

spawn_stmt       = "spawn" , block , ";" ;
```

#### 3.5 Expressions

The expression grammar is presented in precedence levels (from lowest to highest):

```ebnf
expr             = assign_expr ;

assign_expr      = ( identifier | index_expr ) , [ "=" , assign_expr ]
                 | logical_or_expr ;

logical_or_expr  = logical_and_expr , { "||" , logical_and_expr } ;
logical_and_expr = equality_expr , { "&&" , equality_expr } ;

equality_expr    = relational_expr , { ( "==" | "!=" ) , relational_expr } ;

relational_expr  = bitwise_or_expr ,
                   { ( "<" | ">" | "<=" | ">=" ) , bitwise_or_expr } ;

additive_expr    = multiplicative_expr ,
                   { ( "+" | "-" ) , multiplicative_expr } ;

multiplicative_expr =
                   unary_expr ,
                   { ( "*" | "/" | "%" ) , unary_expr } ;

shift_expr       = additive_expr ,
                   { ( "<<" | ">>" ) , additive_expr } ;

bitwise_and_expr = shift_expr ,
                   { "&" , shift_expr } ;

bitwise_xor_expr  = bitwise_and_expr ,
                   { "^" , bitwise_and_expr } ;

bitwise_or_expr   = bitwise_xor_expr ,
                   { "|" , bitwise_xor_expr } ;

unary_expr       = ( "!" | "-" | "&" | "&mut" ) , unary_expr
                 | recv_expr ;

recv_expr        = chan_expr
                 | "<-" , recv_expr ;

chan_expr        = postfix_expr ;

postfix_expr     = primary_expr ,
                   { selector | index | call | send_suffix | cast } ;

selector         = "." , identifier ;
index            = "[" , expr , "]" ;
call             = "(" , arg_list? , ")" ;
arg_list         = expr , { "," , expr } ;

send_suffix      = "<-" , expr ;  (* e.g., tx<-value *)

cast             = "as" , type_expr ;  (* Phase 8: type casting *)

primary_expr     = identifier
                 | literal
                 | "(" , expr , ")"
                 | struct_lit
                 | enum_lit
                 | array_lit ;

array_lit        = "[" , ( expr_list | ( expr , ";" , int_lit ) ) , "]" ;
expr_list        = expr , { "," , expr } ;

literal          = int_lit | float_lit | bool_lit | string_lit | "nil" ;

struct_lit       = identifier , "{" , field_inits? , "}" ;
field_inits      = field_init , { "," , field_init } ;
field_init       = identifier , ":" , expr ;

enum_lit         = identifier , "::" , identifier ,
                   [ "(" , arg_list? , ")" | "{" , named_field_inits? , "}" ] ;
named_field_inits = named_field_init , { "," , named_field_init } ;
named_field_init = identifier , ":" , expr ;

pattern          = identifier
                 | "_" 
                 | literal
                 | enum_pat ;

enum_pat         = identifier , "::" , identifier ,
                   [ "(" , pattern_list? , ")" | "{" , named_pattern_fields? , "}" ] ;
pattern_list     = pattern , { "," , pattern } ;
named_pattern_fields = named_pattern_field , { "," , named_pattern_field } ;
named_pattern_field = identifier , ":" , pattern ;
```

This grammar is compatible with a conventional expression parser. Channel send and receive are expressed via `send_suffix` and the unary `<-` operator.

### 4. Type System

#### 4.1 Primitive and Built-in Types

Ion includes the following primitive types (Phase 4 implementation):

- Machine-sized integer: `int` (signed integer matching the target platform's `int`)
- Signed integers: `i8`, `i16`, `i32`, `i64` (Phase 4)
- Unsigned integers: `u8` (available for raw pointers), `u16`, `u32`, `u64`, `uint` (Phase 4)
- Floating point: `f32`, `f64` (Phase 4)
- Boolean: `bool` (Phase 4)
- Unit / void: `void` (function return type with no value)

Additional built-in generic types:

- `Box<T>` – heap-allocated `T` with owning semantics (fully implemented: `Box::new()`, `Box::unwrap()`)
- `channel<T>` – bounded MPSC channel carrying values of type `T` (lowercase, not `Chan<T>`)
- `Vec<T>` – growable heap-allocated vector (fully implemented: `Vec::new()`, `Vec::with_capacity()`, `Vec::push()`, `Vec::pop()`, `Vec::len()`, `Vec::capacity()`, `Vec::get()`, `Vec::set()`)
- `String` – UTF-8 heap-allocated string (fully implemented: `String::new()`, `String::from()`, `String::push_str()`, `String::len()`)
- `[T; N]` – fixed-size array of `N` elements of type `T` (Phase 3)
- `[]T` – dynamically sized slice (fat pointer) of type `T` (Phase 3)

#### 4.1.1 Array Safety

Fixed-size arrays `[T; N]` have the following safety properties:

- **Bounds checking**: Array indexing `arr[i]` performs runtime bounds checking by default
  - If `i < 0` or `i >= N`, the program panics with an error message via `ion_panic()`
  - The panic prints "Array index out of bounds" to stderr and aborts the program
  - Bounds checking can be disabled in `unsafe` blocks for performance-critical code
- **Compile-time size**: Array size `N` must be a compile-time constant
- **Stack allocation**: Arrays are allocated on the stack by default

**Safe array access:**
```ion
let arr: [int; 5] = [1, 2, 3, 4, 5];
let x = arr[2]; // OK: bounds checked at runtime
```

**Unsafe array access (no bounds checking):**
```ion
let arr: [int; 5] = [1, 2, 3, 4, 5];
unsafe {
    let x = arr[2]; // No bounds check - faster but unsafe
}
```

**Out-of-bounds access (panics):**
```ion
let arr: [int; 3] = [1, 2, 3];
let x = arr[10]; // Runtime panic: "Array index out of bounds"
```


**Standard library enums (user-defined in Phase 1):**
- `Option<T>` – optional value (`Some(T)` or `None`) – can be defined as a generic enum
- `Result<T, E>` – success or error (`Ok(T)` or `Err(E)`) – can be defined as a generic enum

User-defined types:

- `struct` – product types (with generic support in Phase 1, visibility in Phase 2)
- `enum` – tagged unions (sum types) with tuple-style variants (with generic support in Phase 1, visibility in Phase 2)
- `*T` – raw pointer types (Phase 2, for FFI only, dereferencing only in unsafe blocks in Phase 3)
- `[T; N]` – fixed-size array types (Phase 3)
- `[]T` – slice types (Phase 3)

**Implemented in Phase 4:**
- `type` – type aliases: `type Name = T;` and `type Name<T> = Type;` syntax

#### 4.2 Type Equivalence and Aliases

Type aliases created with `type Name = T;` are **transparent**: `Name` is equivalent to `T` for type checking. There is no nominal distinction introduced by `type` in Phase 0.

Struct and enum types are **nominal**: two structs with identical fields but different names are different types.

#### 4.3 Function Types and Signatures

Functions have the form:

```ion
fn name<T1, T2>(param1: T1, param2: T2) -> R { ... }
```

The corresponding function type is:

```ion
fn(T1, T2) -> R
```

Function types are **first-class**: they may be stored in variables, passed as arguments, and returned. However, **function values may not capture references that would violate the no-escape rule** (see Section 5.4).

#### 4.4 Type Inference

Ion supports a **local, Hindley–Milner-inspired inference**:

- `let` bindings may omit the type if the initializer is present:

  ```ion
  let x = 10;          // x: int
  let s = String::new(); // s: String
  ```

- Function parameter and return types **must be annotated** (no inference across function boundaries).
- Generic type parameters on functions and types must be explicit at declaration sites, but may usually be inferred at call sites when unambiguous.

The inference engine is intentionally limited:

- No higher-rank polymorphism.
- No complex trait constraints; only simple, **structural** `Send` constraints may appear in Phase 0. For a generic type `Wrapper<T>`, each monomorphized instantiation `Wrapper<U>` is `Send` if and only if all of its fields (with `T` replaced by `U`) are `Send`.

### 5. Ownership and Borrowing

#### 5.1 Ownership

Every value in Ion has a single owner at any point in time. Ownership is tied to:

- Local variables (`let` bindings)
- Struct fields
- Enum payloads
- Elements in `Vec<T>`, `Box<T>`, etc.

Ownership transfers (moves) occur:

- When binding a value: `let y = x;` moves from `x` to `y`.
- When passing an argument by value.
- When returning a value from a function.
- When sending a value into a channel.

After a move, the previous binding becomes **invalid** and cannot be used.

**Example (valid):**

```ion
fn take(v: Vec<int>) {
    // v is consumed here
}

fn main() {
    let xs = Vec::new();
    take(xs);       // xs moved into take
    // xs is now invalid; any use is a compile error
}
```

#### 5.2 Copy and Move

By default, Ion types are **move-only**. For a small subset of primitive types (e.g., `int`, `bool`, pointers), the implementation may treat moves as cheap copies, but the semantic model is still “move”.

Phase 0 does not expose a `Copy` marker to user code; whether a move is implemented as a copy is an implementation detail.

#### 5.3 Borrowing

References provide scoped, non-owning access:

- `&T` – shared borrow, read-only.
- `&mut T` – exclusive borrow, read-write.

Borrowing does **not** change ownership: the owner remains responsible for destruction. Borrows are created with:

```ion
let x = 10;
let r = &x;      // r: &int
```

and

```ion
let mut x = 10;
let r = &mut x;  // r: &mut int
```

The following borrowing rules apply:

- At any time, either:
  - Any number of `&T` borrows, and **no** `&mut T` borrows, or
  - Exactly one `&mut T` borrow, and **no** `&T` borrows.
- Borrows are restricted to the lexical scope of the function in which they are created (see 5.4).

#### 5.4 No-Escape Rule (Formal)

Each reference has a **borrow scope** equal to a syntactic region within a single function body. The compiler enforces:

- References cannot be:
  - Stored in struct fields or enum variants.
  - Returned from functions.
  - Assigned into global or static variables.
  - Captured by function literals (closures) that may escape the current function.
  - Sent into channels.
  - Stored in values that may cross threads (i.e., be `Send`).

**Example (invalid – returning a reference):**

```ion
fn max_ref(a: &int, b: &int) -> &int {
    if *a > *b {
        return a;    // ERROR: cannot return reference
    } else {
        return b;    // ERROR
    }
}
```

**Example (invalid – storing reference in struct):**

```ion
struct Holder {
    value: &int,   // ERROR: struct fields cannot be references
}
```

**Example (invalid – channel of references):**

```ion
fn main() {
    let (tx, rx) = channel::<&int>(1); // ERROR: references cannot be sent
}
```

**Example (invalid – closure capturing reference and escaping):**

```ion
fn make_printer(x: &int) -> fn() {
    return fn() {
        println("x = {}", x); // would capture x by reference
    }; // ERROR: closure escapes with reference
}
```

**Example (valid – borrow within function):**

```ion
fn print_twice(x: &int) {
    println("{}", x);
    println("{}", x);
} // borrow ends here
```

The type checker implements this by rejecting any attempt to **store or return** a type containing `&` or `&mut` outside the current function’s local variables. In particular, types such as `Option<&T>` or `Result<&T, E>` are only permitted as **local temporaries** within a function body; they cannot be returned, stored in longer-lived data structures, sent through channels, or cross thread boundaries.  Standard library APIs in Phase 0 are designed to avoid exposing such reference-carrying types across function boundaries.

#### 5.5 Destruction and `defer`

When a binding goes out of scope (e.g., block exit, function return, panic unwinding), its owned value is **dropped** exactly once:

- For structs, fields are dropped in declaration order.
- For enums, the active variant’s payload is dropped.
- For `Box<T>`, `T` is dropped, then the allocation is freed.

`defer` schedules an expression to be executed when the current function scope is left, in **last-in, first-out** order.

```ion
fn process() {
    let f = File::open("data.txt")?;
    defer f.close();  // guaranteed to run on any exit path from process

    // ...
}
```

Deferred expressions may not create escaping references or otherwise violate the ownership rules.

### 6. Memory Model

#### 6.1 Stack and Heap

- Local variables (`let`) are allocated on the stack by default.
- `Box<T>` allocates `T` on the heap. Destroying a `Box<T>` drops `T` and frees the heap allocation.
- `Vec<T>` and `String` internally use heap allocations; their behavior is defined by their APIs (Section 7).

Ion does **not** perform implicit heap allocation for:

- Captured closures
- Slices or views
- Temporaries (beyond what is required for expression evaluation)

Any heap allocation must be visible in the code via `Box`, `Vec`, `String`, or other standard types.

#### 6.2 Deterministic Destruction

Ion guarantees that every owned value is dropped exactly once when its owner’s scope ends, except when:

- The program terminates abnormally (e.g., process abort).

In particular:

- Early `return` from a function drops all owned locals before returning.
- Panics (if implemented) unwind the stack, dropping owned values on each frame.
- `spawn`ed threads manage their own stacks independently.

#### 6.3 Aliasing and Safety

Given the ownership and borrowing rules:

- Ion programs cannot exhibit use-after-free or double-free at runtime.
- Data races across threads are prevented if the `Send` rules are correctly enforced (Section 7).

Any violation of safety rules is a **compile-time error**, not undefined behavior.

#### 6.4 C ABI and Layout

By default, `struct` and `enum` layouts are **C-compatible**:

- Field order and alignment follow the target C ABI.
- No hidden metadata is inserted into structs.
- Enums are compatible with “tagged unions” encoded according to a specified ABI (to be detailed in a later phase; Phase 0 may restrict FFI with enums).

Functions may be declared `extern "C"` in Phase 2. Raw pointer types `*T` are available for FFI (distinct from safe references `&T`). Raw pointers are pass-through only in Ion code (no dereferencing). In Phase 0, we assumed:

- Ion compiles to C functions with straightforward signatures.
- Parameter and return passing follows the C calling convention of the target platform.

### 7. Concurrency and `Send`

#### 7.1 Threads and `spawn`

The `spawn` statement creates a new OS thread executing the given block:

```ion
spawn {
    // body
};
```

The block may capture **owned values** from the enclosing scope **by move** only. Capturing references is disallowed:

```ion
fn main() {
    let v = Vec::new();

    spawn {
        // v is moved into this thread; main cannot use v after this point
        work(v);
    };
}
```

Attempting to use `v` after `spawn` is a compile-time error.

#### 7.2 Channels

Ion provides typed, bounded MPSC channels via the `channel<T>` type and built-in `send()` and `recv()` functions:

**Channel Type:**
```ion
let ch: channel<int>;
```

**Sending and Receiving:**
```ion
send(ch, value);  // Moves value into the channel
let x = recv(ch); // Moves value out of the channel, blocking until available
```

**Semantics (Phase 1 implementation):**

- Channels are declared with the type `channel<T>` where `T` must be `Send`.
- `send(ch, value)` moves a value into the channel; the channel itself is read-only.
- `recv(ch)` returns a value of type `T` moved out of the channel.
- Both operations block: `send` when the buffer is full, `recv` when empty.
- Channel creation and capacity management are handled by the runtime.

**Split Channel API (Phase 6 implementation):**

Phase 6 introduces the split channel API with `Sender<T>` and `Receiver<T>` types:

```ion
// Create a channel - returns (Sender<T>, Receiver<T>) tuple
let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();

// Send requires &Sender<T>
send(&tx, 42);

// Recv requires &mut Receiver<T>
let mut rx_mut = rx;
let value = recv(&mut rx_mut);
```

- `channel<T>()` is a built-in function that returns a tuple `(Sender<T>, Receiver<T>)`
- `Sender<T>` and `Receiver<T>` are move-only value types (not pointers)
- `send()` requires `&Sender<T>` (shared reference to sender)
- `recv()` requires `&mut Receiver<T>` (mutable reference to receiver)
- Tuple destructuring is supported: `let (tx, rx) = channel<int>();`
- Both types are `Send` if `T: Send`, allowing them to be moved between threads

#### 7.3 `Send` Property

The `Send` property marks types that are safe to transfer to another thread by value. In Phase 0:

- Primitive types (`int`, `bool`, etc.) are `Send`.
- `Box<T>` is `Send` if `T: Send`.
- `Vec<T>` is `Send` if `T: Send`.
- `String` is `Send`.
- `Option<T>` is `Send` if `T: Send`.
- `Result<T, E>` is `Send` if both `T: Send` and `E: Send`.
- `channel<T>` types are `Send` if `T: Send`.
- Any type containing a reference (`&T`, `&mut T`) is **not** `Send`.

User-defined `struct` and `enum` types are `Send` if and only if **all of their fields / payloads are `Send`**. For generic types, this rule is applied **per instantiation**: e.g., `Wrapper<int>` may be `Send` while `Wrapper<NonSend>` is not, depending on the fields.

The compiler checks `Send` when:

- Moving a value into a `spawn` body.
- Sending a value into a channel whose receiver may be on another thread.

Any attempt to move a non-`Send` type across threads is a compile-time error.

### 8. Standard Library Overview

This section specifies the **surface API and semantics** of core library types. Implementations are provided in Ion and/or C and are not part of the language definition.

#### 8.1 `Option<T>` and `Result<T, E>`

```ion
enum Option<T> {
    Some(T);
    None;
}

enum Result<T, E> {
    Ok(T);
    Err(E);
}
```

Semantics follow the conventional meaning:

- `Option<T>` represents presence or absence of a value.
- `Result<T, E>` represents success (`Ok`) or failure (`Err`).

These enums follow standard ownership rules (payloads are moved in and out).

#### 8.2 `Vec<T>`

`Vec<T>` is a growable, heap-allocated sequence of `T`.

Essential API (implemented):

```ion
struct Vec<T> { /* opaque fields */ }

impl<T> Vec<T> {
    fn new() -> Vec<T>;
    fn with_capacity(cap: int) -> Vec<T>;
    fn push(self: &mut Vec<T>, value: T);
    fn pop(self: &mut Vec<T>) -> Option<T>;
    fn len(self: &Vec<T>) -> int;
    fn capacity(self: &Vec<T>) -> int;
    fn get(self: &Vec<T>, index: int) -> Option<T>;
    fn set(self: &mut Vec<T>, index: int, value: T);
}
```

Note that:

- `Vec<T>` is `Send` if `T: Send`.
- `Vec::get()` and `Vec::pop()` return `Option<T>` to handle out-of-bounds or empty cases.
- `Vec::set()` requires a mutable reference and returns an error code (0 for success, non-zero for failure).

More ergonomic indexed access APIs (e.g., returning "maybe a reference") are intentionally deferred.  Ion favors an **index/handle style** for long-lived references into collections: helper functions typically return indices or keys rather than borrowed references, and callers re-index into the collection within their own function bodies.

#### 8.3 `String`

`String` is a growable, heap-allocated UTF-8 string.

Essential API (implemented):

```ion
struct String { /* opaque fields */ }

// `str` is a primitive, unsized UTF-8 slice type. A value of type `&str`
// is represented as (pointer, length) and does not own its data.

impl String {
    fn new() -> String;
    fn from(s: &str) -> String;
    fn push_str(self: &mut String, s: &str);
    fn len(self: &String) -> int;
}
```

Note that:

- String literals can be directly assigned to `String` type: `let s: String = "hello";`
- `String::from()` creates a heap-allocated copy of a string literal.
- `String::push_str()` appends a string literal to an existing `String`.

`&str` is always a **borrowed view** into existing UTF-8 data; it cannot be returned or stored in long-lived structures in ways that would violate the no-escape rule.  Phase 1 intentionally avoids APIs that would expose `&str` values across function boundaries in ways that require complex lifetime reasoning (e.g., `String::as_str` methods that return borrowed views).

#### 8.4 Channels

**Split Channel API (Phase 6):**

Ion provides a split channel API with `Sender<T>` and `Receiver<T>` types:

```ion
// Create a channel
let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();

// Send a value (requires &Sender<T>)
send(&tx, 42);

// Receive a value (requires &mut Receiver<T>)
let mut rx_mut = rx;
let value = recv(&mut rx_mut);
```

- `channel<T>()` - Built-in function that creates a channel and returns `(Sender<T>, Receiver<T>)`
- `Sender<T>` - Move-only handle for sending messages (value type, not a pointer)
- `Receiver<T>` - Move-only handle for receiving messages (value type, not a pointer)
- `send(&Sender<T>, T)` - Sends a value, blocking if the buffer is full
- `recv(&mut Receiver<T>) -> T` - Receives a value, blocking if the buffer is empty
- Both `Sender<T>` and `Receiver<T>` are `Send` if `T: Send`, allowing them to be moved between threads

See Section 7.2 for detailed channel semantics.

#### 8.5 File I/O

Minimal file API (sketch):

```ion
struct File { /* opaque, not Send unless specified */ }

impl File {
    fn open(path: &String) -> Result<File, IOError>;
    fn read(self: &mut File, buf: &mut Vec<u8>) -> Result<int, IOError>;
    fn write(self: &mut File, buf: &Vec<u8>) -> Result<int, IOError>;
    fn close(self: &mut File) -> Result<void, IOError>;
}

enum IOError {
    NotFound;
    PermissionDenied;
    UnexpectedEof;
    Other(int); // platform error code
}
```

File handles are typically **not `Send`** in Phase 0, to avoid subtle platform-dependent behavior; they must be used from the thread that created them.

### 9. Examples and Edge Cases

#### 9.1 Basic Ownership

```ion
struct Packet {
    id: i32;
    data: Vec<u8>;
}

fn make_packet(data: Vec<u8>) -> Packet {
    Packet { id: 1, data: data } // data moved into Packet
}

fn main() {
    let buf = Vec::<u8>::new();
    let pkt = make_packet(buf);
    // buf is now invalid; pkt owns the data
}
```

#### 9.2 Borrowing Within Functions

```ion
fn increment(x: &mut int) {
    *x = *x + 1;
}

fn main() {
    let mut n = 0;
    increment(&mut n);
    println("n = {}", n);
}
```

#### 9.3 Concurrency with Channels

```ion
fn worker(mut rx: Receiver<Vec<u8>>) {
    loop {
        let buf = recv(&mut rx); // Blocks until value available
        // Process buf...
    }
}

fn main() {
    // Create channel with tuple destructuring
    let (tx, rx): (Sender<Vec<u8>>, Receiver<Vec<u8>>) = channel<Vec<u8>>();

    spawn {
        worker(rx); // rx moved into worker thread
    };

    let buf = Vec::<u8>::new();
    send(&tx, buf); // buf moved into channel
}
```

Note: `Sender<T>` and `Receiver<T>` are `Send` values; here we move `rx` directly into the worker thread. No references cross thread boundaries, and all communication uses channels. The split API allows different threads to own different ends of the channel.

#### 9.4 Rejected Patterns

- Returning references.
- Storing references in structs or enums.
- Channels of references.
- Capturing references in escaping closures.
- Moving non-`Send` values into `spawn`.

Each such pattern must produce a clear, actionable compiler error.

### 10. Phase 7 Implementation Status

Phase 7 extends Phase 6 with critical systems programming features: array initialization syntax, bitwise operators, and complete escape sequence support. **Phase 7 is complete** with all features implemented and tested (76/76 tests passing).

#### 10.0 Array Initialization Syntax

Phase 7 introduces Rust-style array initialization syntax for initializing arrays with repeated values:

**Syntax:**
```ion
let buffer: [u8; 128] = [0; 128];  // Fill array with 128 copies of 0
let ones: [int; 10] = [1; 10];     // Fill array with 10 copies of 1
```

**Features:**
- `[value; count]` syntax where `count` must be a compile-time constant integer literal
- Works in variable declarations, struct field initializers, and function returns
- Supports type coercion for array elements (e.g., `int` to `u8`)
- For zero initialization, generates efficient C code using `{0}` syntax
- For non-zero values, generates explicit initialization lists

**Example:**
```ion
struct Buffer {
    data: [u8; 128];
}

fn make_zeros() -> [int; 4] {
    return [0; 4];
}

fn main() -> int {
    let arr: [int; 5] = [0; 5];
    let buf: Buffer = Buffer { data: [0; 128] };
    let zeros = make_zeros();
    return 0;
}
```

#### 10.1 Bitwise Operators

Phase 7 adds full support for bitwise operations, critical for network programming and binary protocols:

**Operators:**
- `&` - bitwise AND
- `|` - bitwise OR
- `^` - bitwise XOR
- `<<` - left shift
- `>>` - right shift (arithmetic for signed, logical for unsigned)

**Precedence** (from highest to lowest):
1. Unary (`-`, `!`, `*`, `&`, `&mut`)
2. Multiplicative (`*`, `/`, `%`)
3. Additive (`+`, `-`)
4. **Shift** (`<<`, `>>`)
5. **Bitwise AND** (`&`)
6. **Bitwise XOR** (`^`)
7. **Bitwise OR** (`|`)
8. Comparison (`<`, `>`, `==`, `!=`)
9. Logical (`&&`, `||`)

**Type Rules:**
- Operands must be integer types (`int`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `uint`)
- Result type follows standard integer promotion rules
- Shift operators: right operand must be unsigned integer (shift amount)
- For signed types, right shift is arithmetic (sign-extending)
- For unsigned types, right shift is logical (zero-filling)

**Operator Disambiguation:**
- `&ident` or `&mut ident` → reference operator (unary)
- `expr & expr` → bitwise AND (binary)

**Example:**
```ion
// Network byte order extraction
let port: u16 = 8080;
let shift8: u16 = 8;
let high_byte: u16 = port >> shift8;   // High byte
let mask: u16 = 255;  // 0xFF mask
let low_byte: u16 = port & mask;       // Low byte

// Basic bitwise operations
let a: u8 = 170;  // 0xAA
let b: u8 = 240;  // 0xF0
let and_result: u8 = a & b;  // 160 (0xA0)
let or_result: u8 = a | b;    // 250 (0xFA)
let xor_result: u8 = a ^ b;   // 90 (0x5A)
```

#### 10.2 Complete Escape Sequence Support

Phase 7 adds support for all standard C escape sequences in string literals:

**Supported Escape Sequences:**
- `\r` - carriage return (0x0D)
- `\n` - line feed (0x0A)
- `\t` - horizontal tab (0x09)
- `\0` - null terminator (0x00)
- `\\` - backslash (0x5C)
- `\"` - double quote (0x22)
- `\'` - single quote (0x27)

**Example:**
```ion
// HTTP response with proper line endings
let response: String = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n";

// Tab-separated values
let tsv: String = "name\tage\tcity\n";

// Null-terminated strings (for C interop)
let c_string: String = "Hello\0World";
```

This is essential for proper HTTP protocol implementation and C interop.

### 12. Phase 6 Implementation Status

Phase 6 extends Phase 5 with struct-style enum variants, for loops, and the split channel API. **Phase 6 is complete** with all features implemented and tested.

#### 10.0 Split Channel API

Phase 6 introduces the split channel API with `Sender<T>` and `Receiver<T>` types, replacing the simplified channel model from earlier phases.

**Syntax:**
```ion
// Create a channel - returns (Sender<T>, Receiver<T>) tuple
let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();

// Send requires &Sender<T>
send(&tx, 42);

// Recv requires &mut Receiver<T>
let mut rx_mut = rx;
let value = recv(&mut rx_mut);
```

**Features:**
- `channel<T>()` is a built-in function that returns `(Sender<T>, Receiver<T>)`
- `Sender<T>` and `Receiver<T>` are move-only value types (struct values, not pointers)
- Tuple destructuring is supported: `let (tx, rx) = channel<int>();`
- `send(&Sender<T>, T)` requires a shared reference to the sender
- `recv(&mut Receiver<T>) -> T` requires a mutable reference to the receiver
- Both types are `Send` if `T: Send`, allowing them to be moved between threads
- The split API enables better concurrency patterns where different threads own different ends of the channel

#### 10.1 Struct-Style Enum Variants

Ion supports enum variants with named fields (struct-style variants) in addition to tuple-style variants:

**Syntax:**
```ion
enum Result {
    Ok { value: int };
    Err { message: String };
}
```

**Enum Literals:**
```ion
let result: Result = Result::Ok { value: 42 };
```

**Pattern Matching:**
```ion
match result {
    Result::Ok { value: v } => {
        return v;
    }
    Result::Err { message: _m } => {
        return 0;
    }
}
```

Struct variants are fully integrated with the type system, pattern matching, and code generation. They provide better ergonomics for variants with multiple fields compared to tuple-style variants.

#### 10.2 For Loops

Ion supports `for...in` loops for iterating over collections:

**Syntax:**
```ion
for identifier in expr { ... }
```

**Semantics:**
- The iterable expression must be of type `Vec<T>`, `String`, or `[T; N]`
- The loop variable is bound to each element in sequence
- For loops are desugared to while loops with index-based iteration
- The loop body has access to the loop variable

**Example:**
```ion
let mut vec: Vec<int> = Vec::new();
vec.push(42);
vec.push(100);

for x in vec {
    // x is bound to each element: 42, then 100
    let _y = x;
}
```

**Desugaring:**
For loops are desugared to:
```ion
let mut __i = 0;
while __i < container.len() {
    let __opt = container.get(__i);
    match __opt {
        Option::Some(x) => {
            // loop body
            __i = __i + 1;
        }
        Option::None => {}
    }
}
```

### 13. Phase 5 Implementation Status

Phase 5 extends Phase 4 with method call syntax (Uniform Function Call Syntax) for improved ergonomics. **Phase 5 is complete** with all features implemented and tested.

#### 10.0 Method Call Syntax (Uniform Function Call Syntax)

Ion supports method call syntax as **syntactic sugar** for calling functions where the first argument is implicitly the caller. This feature provides a more ergonomic way to call static functions without introducing new language concepts like object-oriented dispatch or dynamic methods.

##### 10.0.1 Syntax

A function call of the form:

```ion
expression.method_name(arg2, arg3, ...)
```

is desugared by the parser and type checker into a normal static function call:

```ion
Type::method_name(expression, arg2, arg3, ...)
```

where `Type` is the concrete type of `expression`.

##### 10.0.2 Resolution Rules

The compiler performs the following steps to resolve a method call `e.m(a1, a2, ...)`:

1. **Determine the Type**: The type checker determines the concrete type of the receiver expression, `T = TypeOf(e)`.
2. **Resolve Type Aliases**: Type aliases are resolved to their underlying types (e.g., `type MyVec = Vec<int>` → `Vec<int>`).
3. **Look up Qualified Function**: The compiler searches for a function named `T::m`.
4. **Check Arity and Type**: The function `T::m` must exist and be callable with a total of `N+1` arguments, where `N` is the number of arguments provided in the method call.

##### 10.0.3 Implicit Borrowing

The compiler automatically infers the correct reference type (`&T` or `&mut T`) for the receiver argument:

- **Mutable Receiver**: If the function signature requires `&mut T` for its first argument and the receiver is a mutable binding, the compiler implicitly borrows as `&mut receiver`.
  - Example: `v.push(10)` where `v: mut Vec<int>` desugars to `Vec::push(&mut v, 10)`.
- **Immutable Receiver**: If the function signature requires `&T` or the receiver is immutable, the compiler borrows as `&receiver`.
  - Example: `s.len()` where `s: String` desugars to `String::len(&s)`.
- **By Value**: If the function signature requires `T` (by value), the receiver is passed directly (moves).

##### 10.0.4 Generic Types

Method call syntax fully supports generic types by leveraging existing monomorphization logic. For example, `Vec::push` is a generic function, and `numbers.push(10)` where `numbers: Vec<int>` correctly resolves to the monomorphized `Vec::push<int>`.

##### 10.0.5 Built-in Methods

Built-in types support method call syntax:

- **Vec<T>**: `new()`, `push(value)`, `pop()`, `len()`, `capacity()`, `get(index)`, `set(index, value)`
- **String**: `new()`, `from(str)`, `push_str(str)`, `len()`

##### 10.0.6 Examples

```ion
// Phase 4 syntax (still valid)
let mut numbers: Vec<int> = Vec::new();
Vec::push(&mut numbers, 10);

// Phase 5 method syntax (preferred)
let mut numbers: Vec<int> = Vec::new();
numbers.push(10);
numbers.push(20);

match numbers.pop() {
    Option::Some(value) => { /* ... */ }
    Option::None => {}
};

let desc: String = String::from("Hello");
let len: int = desc.len();
```

Method call syntax significantly improves readability, especially for chained operations, while maintaining full compatibility with the existing qualified function call syntax.

### 14. Phase 4 Implementation Status

Phase 4 extends Phase 3 with a complete type system including boolean, floating-point, additional integer types, and type aliases. **Phase 4 is complete** with all features implemented and tested (66/66 tests passing).

#### 11.0 Implemented in Phase 4 (Complete)
- **Type System Completeness**: Full primitive type support
  - Boolean type: `bool` with literals `true` and `false`
  - Logical operators: `&&` (and), `||` (or), `!` (not)
  - Floating-point types: `f32` (32-bit float) and `f64` (64-bit double)
  - Additional integer types: `i8`, `i16`, `i32`, `i64`, `u16`, `u32`, `u64`, `uint` (unsigned int)
  - Type aliases: `type Name = T;` for creating new names for existing types
  - Generic type aliases: `type Result<T> = Option<T>;` with parameter substitution
  - Type promotion rules: automatic coercion between compatible numeric types
  - **Breaking change**: `if` and `while` conditions now require `bool` instead of truthy `int` values
  - **Parser improvements**: Enhanced lookahead to correctly distinguish struct literals from blocks in boolean expressions (e.g., `if x && !y { ... }`)

### 15. Phase 3 Implementation Status

Phase 3 extends Phase 2 with arrays, slices, explicit unsafe blocks, and multi-file compilation. **Phase 3 is complete** with all features implemented and tested (52/52 tests passing).

#### 12.1 Implemented in Phase 3 (Complete)
- **Arrays and Slices**: Low-overhead memory access
  - Fixed-size arrays: `[T; N]` syntax (e.g., `[int; 5]`)
  - Dynamically sized slices: `[]T` syntax (e.g., `[]int`)
  - Array literals: `[1, 2, 3]` syntax
  - Indexing: `arr[i]` returns the element value (not a reference)
  - Implicit array-to-slice coercion: `&[T; N]` automatically coerces to `&[]T` in function calls
- **Explicit Unsafe Blocks**: Mark code that circumvents safety guarantees
  - `unsafe { ... }` blocks for low-level operations
  - Raw pointer dereferencing (`*ptr`) only allowed in unsafe blocks
  - Raw pointer arithmetic (`ptr + offset`) only allowed in unsafe blocks
  - Extern function calls require unsafe context
  - Core safety (no-escape, Send checking) still enforced even in unsafe
- **Advanced Compilation**: Multi-file compilation support
  - Single-file mode (default, Phase 2 compatible): generates one `.c` file
  - Multi-file mode (`--mode multi`): generates separate `.c` and `.h` files per module
  - Header generation for `pub` items (functions, structs, enums)
  - Automatic object file compilation (`.c` → `.o`) and linking orchestration
  - Proper include path handling for cross-module dependencies
  - All headers generated before compilation to ensure dependencies are available
- **Variadic Functions**: Support for C variadic functions in extern blocks
  - `fn printf(format: *u8, ...) -> int;` syntax with `...` ellipsis
  - Variadic functions require `unsafe` context when called
  - Variadic functions require `unsafe` context when called

#### 12.2 Implemented in Phase 2
- **Module System**: Multi-file programs with `import "file.ion" as name;`
  - Relative import paths (`./file.ion`, `../file.ion`) with mandatory `.ion` extension
  - Import cycle detection using DFS
  - Qualified name resolution: `mod::item` syntax
- **Visibility Control**: `pub` keyword for functions, structs, and enums
  - Non-public items are only accessible within the same file
  - Public items can be accessed from other modules via qualified names
- **Foreign Function Interface (FFI)**: 
  - `extern "C" { fn name(...) -> ...; }` blocks
  - Raw pointer types `*T` for FFI (e.g., `*u8` for `char*` in C)
  - Automatic string literal to `*u8` conversion for extern function calls
  - C function prototypes are emitted in generated code

#### 12.3 Implemented in Phase 1

- **Enums**: Generic and non-generic enums with tuple-style variants
- **Pattern Matching**: `match` expressions with:
  - Enum variant patterns: `Option::Some(value)`, `Option::None`
  - Wildcard patterns: `_`
  - Binding patterns: `value` (extracts payload values)
  - Nested patterns in variant payloads
  - Exhaustiveness checking
- **Generics**: Monomorphization-based generics for:
  - Struct declarations: `struct Point<T> { x: T; y: T; }`
  - Enum declarations: `enum Option<T> { Some(T); None; }`
  - Function declarations: `fn id<T>(x: T) -> T { x }`
- **Heap Allocation Types**:
  - `Box<T>` fully implemented: `Box::new()`, `Box::unwrap()`
  - `Vec<T>` fully implemented: `Vec::new()`, `Vec::with_capacity()`, `Vec::push()`, `Vec::pop()`, `Vec::len()`, `Vec::capacity()`, `Vec::get()`, `Vec::set()`
  - `String` fully implemented: `String::new()`, `String::from()`, `String::push_str()`, `String::len()`, with string literal conversion: `let s: String = "hello";`
  - `Box<T>` fully implemented: `Box::new()`, `Box::unwrap()`
- **Control Flow**:
  - `while expr { ... }` loops
  - Function calls: `fn_name(arg1, arg2)`
- **String Literals**: `"..."` string literals with automatic conversion to `String` type

#### 12.4 Not Yet Implemented
- Advanced iterator pipelines and zero-cost abstractions beyond the basics.

### 16. Phase 8 Implementation Status

Phase 8 extends Phase 7 with type casting, array element assignment, complete comparison operators, and a safe standard library. **Phase 8 is complete** with all features implemented and tested.

#### 13.1 Implemented in Phase 8 (Complete)
- **Comparison Operators**: Full support for `<=`, `>=` (in addition to `==`, `!=`, `<`, `>`)
- **Modulo Operator**: `%` for integer types
- **Type Casting**: `as` keyword for numeric conversions (e.g., `float as int`)
- **Array Element Assignment**: `arr[i] = value` syntax for mutable arrays
- **Standard Library**: Safe I/O module (`stdlib/io.ion`)
  - `io::print`, `io::println` for safe string output
  - `String` field access (`.data`, `.len`) for low-level manipulation

### 17. Future Work (Non-Normative)

The following features are deliberately **out of scope** for Phase 2:

- Asynchronous/await syntax and futures.
- Complex trait or typeclass systems.
- Macros and compile-time metaprogramming.
- Advanced iterator pipelines and zero-cost abstractions beyond the basics.

Any such addition must:

- Preserve the no-escape rule and simple ownership model.
- Not require GC or complex runtime machinery.

### 18. Appendix: Recommended Design Patterns (Non-Normative)

This appendix describes idioms that work well with Ion’s ownership and no-escape borrowing rules.  They are not part of the core semantics, but library and application authors are encouraged to follow them for clarity and safety.

#### 14.1 Handle / Index Access Instead of Borrowed Returns

Because Ion does not allow returning references from functions, helpers that would traditionally return `&T` should instead return:

- An **index** (e.g., `int`) into a collection, or
- A **key/handle** type that wraps an index or opaque identifier.

Example:

```ion
struct Db {
    customers: Vec<Customer>;
}

// Find a customer index; -1 indicates not found.
fn find_customer_index(db: &Db, id: int) -> int {
    let mut i = 0;
    while i < db.customers.len() {
        if db.customers[i].id == id {
            return i;
        }
        i = i + 1;
    }
    return -1;
}

fn use_customer(db: &mut Db, id: int) {
    let idx = find_customer_index(db, id);
    if idx >= 0 {
        db.customers[idx].active = true;
    }
}
```

This pattern keeps **all borrows local** to the caller while still allowing helpers to encapsulate search logic.

#### 14.2 Callback-Based Accessors

For more complex logic, helpers can accept a **callback** to operate on a found element, rather than returning a reference:

```ion
fn with_customer_mut(db: &mut Db, id: int, f: fn(&mut Customer)) {
    let mut i = 0;
    while i < db.customers.len() {
        if db.customers[i].id == id {
            f(&mut db.customers[i]); // borrow is local to this function
            return;
        }
        i = i + 1;
    }
}
```

The callback itself must obey the no-escape rule (it cannot store the reference), but this style allows reusable traversal logic.

#### 14.3 Owned Results Instead of Borrowed Views

At API boundaries (public functions, module exports), favor **owned results** over borrowed views:

- Return `Vec<T>` instead of `&[T]`.
- Return `String` instead of `&str`.
- Return structs/enums that own their payloads instead of borrowing them.

Borrowed views (`&T`, `&str`, local `Option<&T>` temporaries) are best used **inside** a single function to avoid copying in tight loops, not as part of public APIs.

#### 14.4 Short-Lived Local View Types

Local helper types that contain references (e.g., a small struct bundling several `&T` values) are allowed **within a single function** as long as:

- They are not returned.
- They are not stored in longer-lived data structures.
- They do not cross thread or channel boundaries.

Example:

```ion
fn compare(a: &Customer, b: &Customer) -> int {
    struct View {
        id: &int;
        score: &int;
    }

    let va = View { id: &a.id, score: &a.score };
    let vb = View { id: &b.id, score: &b.score };

    if *va.score != *vb.score {
        return *va.score - *vb.score;
    }
    return *va.id - *vb.id;
}
```

This leverages references for readability without leaking them beyond the function body.

#### 14.5 Concurrency: Message-Passing Ownership Transfer

For concurrent designs:

- Treat each `spawn`ed thread as an **owner** of the values moved into it.
- Use channels to move ownership of work items between threads.
- Avoid shared mutable state; if multiple threads must coordinate, do so by exchanging owned messages.

Example:

```ion
struct Job {
    data: Vec<u8>;
}

fn worker(mut rx: Receiver<Job>) {
    loop {
        match rx.recv() {
            Ok(job) => {
                process(job.data); // job and its data are owned here
            }
            Err(_) => {
                return;
            }
        }
    }
}

fn main() {
    let (tx, rx) = channel::<Job>(128);

    spawn {
        worker(rx); // rx moved into worker thread
    };

    let job = Job { data: Vec::<u8>::new() };
    tx.send(job); // ownership moved to worker
}
```

This emphasizes that concurrency in Ion is about **moving ownership** between threads, not sharing it.

