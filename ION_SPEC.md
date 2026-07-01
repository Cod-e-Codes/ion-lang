## Ion Language Specification

### 1. Introduction

Ion is a systems programming language designed for:

- **Ownership and borrowing** with **move-only semantics** and **no-escape references**
- **Memory safety without garbage collection**
- **Message-passing parallelism** via **typed, bounded channels** and OS threads
- **Simple syntax** and predictable performance
- A minimal, C-oriented runtime and ABI

This document defines Ion semantics for the current compiler: ownership, types, control flow, generics, concurrency, and the C code generation backend. It is intended to be:

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

Feature coverage is validated by the integration test suite. See [tests/README.md](tests/README.md).

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

`fn`, `let`, `mut`, `struct`, `enum`, `type`, `box`, `if`, `else`, `while`, `for`, `loop`, `match`, `defer`, `return`, `break`, `continue`, `spawn`, `channel`, `true`, `false`, `nil`, `import`, `as`, `pub`, `extern`, `unsafe`.

Note: The `as` keyword is used both for module aliases (`import "file.ion" as name;`) and for type casting (`expr as Type`).

This list is intentionally small; future additions must justify their complexity.

#### 2.3 Unsafe Blocks and Safety Boundaries

The `unsafe` keyword marks code blocks where Ion's safety guarantees are suspended:

- **Array and slice bounds checking** is disabled for indexing within `unsafe` blocks
- **Raw pointer dereferencing** is only permitted within `unsafe` blocks  
- **FFI calls** to `extern "C"` functions must be wrapped in `unsafe` blocks

##### Unsafe Contract

Code within `unsafe` blocks must manually uphold the following invariants:

1. **No out-of-bounds array or slice access**: All indices must be valid
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

- **Integer literals** (e.g., `0`, `42`, `0xFF`, `0b1010`) – implemented
- **String literals**: `"..."` with complete escape sequence support
  - `\r` (carriage return), `\n` (newline), `\t` (tab), `\0` (null)
  - `\\` (backslash), `\"` (double quote), `\'` (single quote)
  - Can be assigned to `String` type
- **Floating-point literals** (e.g., `3.14`, `1e9`, `.5`, `3.`) – implemented
- **Boolean literals**: `true`, `false` – implemented

The exact numeric literal grammar is given in the EBNF (Section 3.1).

##### 2.2.4 Operators and Delimiters

Ion uses the following operators:

- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Bitwise: `&` (AND), `|` (OR), `^` (XOR), `<<` (left shift), `>>` (right shift)
- Assignment: `=`, `+=` (compound assignment desugars to `x = x + e` for supported `+` types)
- Type casting: `as` keyword for explicit type conversions
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
- Block comments are not defined (may be added later).
- Documentation uses the same `//` syntax; contiguous comment lines immediately above a declaration (no blank line between the last comment and the declaration) are attached to that item for IDE hover and future doc tools. See [§12.6](#126-documentation-comments-non-normative).

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

line_comment  = "//" , { comment_char } ;
comment_char  = /* any character except newline */ ;
```

Line comments are discarded during lexing and do not appear in generated C. Adjacent `//` lines above declarations are recovered from source during parsing and attached to AST nodes (see [§12.6](#126-documentation-comments-non-normative)).

#### 3.2 Modules and Declarations

```ebnf
file             = { import_decl | top_decl } ;

import_decl      = "import" , string_lit , [ "as" , identifier ] , ";" ;

top_decl         = struct_decl
                 | enum_decl
                 | type_alias
                 | fn_decl ;
```

Import string literals name a module file. Resolution is tooling-defined (see [§10.1](#101-project-build-ion-build)): file-relative paths (`./`, `../`), same-directory modules, stdlib search paths (`stdlib/io.ion`, `io.ion`), then project-root-relative paths. The import statement grammar is unchanged.

```ebnf
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
type_param       = identifier ;  (* no trait bounds *)

params           = param , { "," , param } ;
param            = identifier , ":" , type_expr ;

return_type      = "->" , type_expr ;
```

Visibility is controlled by the `pub` keyword. Top-level declarations (functions, structs, enums) may be prefixed with `pub` to make them accessible from other modules. Non-public items are only accessible within the same file. Public items can be accessed from other modules via qualified names (`mod::item`).

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

Note: Union types (`A | B`) are reserved for future use and may be restricted or removed; enums are the primary sum type mechanism.

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
                 | break_stmt
                 | continue_stmt
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
match_arm        = pattern , [ "if" , expr ] , "=>" , block ;

return_stmt      = "return" , [ expr ] , ";" ;

break_stmt       = "break" , ";" ;

continue_stmt    = "continue" , ";" ;

defer_stmt       = "defer" , expr , ";" ;

spawn_stmt       = "spawn" , block , ";" ;
```

`else if` chains use the `if_stmt` alternative after `else` (for example `else if cond { ... }`).

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

cast             = "as" , type_expr ;  (* type casting *)

primary_expr     = identifier
                 | literal
                 | "(" , expr , ")"
                 | struct_lit
                 | enum_lit
                 | array_lit
                 | fn_literal ;

fn_literal       = "fn" , "(" , params? , ")" , [ "->" , type_expr ] , block ;

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

Ion includes the following primitive types:

- Machine-sized integer: `int` (signed integer matching the target platform's `int`)
- Signed integers: `i8`, `i16`, `i32`, `i64`
- Unsigned integers: `u8` (available for raw pointers), `u16`, `u32`, `u64`, `uint`
- Floating point: `f32`, `f64`
- Boolean: `bool`
- Unit / void: `void` (function return type with no value)

Each integer primitive exposes compile-time limits as `Type::MIN` and `Type::MAX` (for example `int::MIN`, `i32::MAX`, `u8::MIN`). These lower to C-safe literals (avoiding `-2147483648`-style unary overflow in generated C).

Additional built-in generic types:

- `Box<T>` – heap-allocated `T` with owning semantics (`Box::new()`, `Box::unwrap()`)
- `Sender<T>` and `Receiver<T>` – move-only handles for the two ends of a bounded MPSC channel
- `Vec<T>` – growable heap-allocated vector (`Vec::new()`, `Vec::with_capacity()`, `Vec::push()`, `Vec::pop()`, `Vec::len()`, `Vec::capacity()`, `Vec::get()`, `Vec::get_ref()`, `Vec::set()`)
- `String` – UTF-8 heap-allocated string (fully implemented: `String::new()`, `String::from()`, `String::push_str()`, `String::push_byte()`, `String::len()`)
- `[T; N]` – fixed-size array of `N` elements of type `T`
- `[]T` – dynamically sized slice (fat pointer) of type `T`
- `(T1, T2, ...)` – fixed-size tuple value type (see §4.1.2)

#### 4.1.2 Tuple Values

Tuple types `(T1, T2, ...)` describe anonymous product types with positional fields `f0`, `f1`, ... accessed in source as `.0`, `.1`, etc.

```ion
let t: (int, int) = (10, 20);
let x: int = t.0;
let (a, b) = t; // destructure by move
```

Rules:

- Tuple literals `(expr1, expr2, ...)` require at least one element; empty `()` is not supported.
- Field indices are 0-based; out-of-range access is a compile error.
- Moving a tuple moves all non-copy fields together; destructuring `let (a, b) = t` moves each bound field out of `t`.
- Nested tuples, tuple equality (`==`), tuples in struct fields, and generic tuple types are not supported in the current compiler.

#### 4.1.1 Array Safety

Fixed-size arrays `[T; N]` have the following safety properties:

- **Bounds checking**: Array indexing `arr[i]` performs runtime bounds checking by default
  - If `i < 0` or `i >= N`, the program panics with an error message via `ion_panic()`
  - The panic prints "Array index out of bounds" to stderr and aborts the program
  - Bounds checking can be disabled in `unsafe` blocks for performance-critical code
- **Compile-time size**: Array size `N` must be a compile-time constant
- **Stack allocation**: Arrays are allocated on the stack by default
- **Index type**: The index expression may be any integer type (`int`, `i32`, `u32`, etc.).

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

#### 4.1.2 Slice Safety

Dynamically sized slices `[]T` (and `&[]T`) have the following safety properties:

- **Bounds checking**: Slice indexing `s[i]` performs runtime bounds checking by default
  - If `i < 0` or `i >= s.len`, the program panics with an error message via `ion_panic()`
  - The panic prints "Slice index out of bounds" to stderr and aborts the program
  - Bounds checking can be disabled in `unsafe` blocks for performance-critical code
- **Fat pointer representation**: Slices are `(data, len)` pairs at runtime

**Safe slice access:**
```ion
fn first(s: &[]int) -> int {
    return s[0]; // OK: bounds checked against s.len at runtime
}
```

**Unsafe slice access (no bounds checking):**
```ion
unsafe {
    let x = s[0]; // No bounds check - faster but unsafe
}
```

**Out-of-bounds access (panics):**
```ion
return s[10]; // Runtime panic: "Slice index out of bounds" when len <= 10
```

**Array-to-slice coercion:**

A reference to a fixed-size array `&[T; N]` coerces to `&[]T` where the type checker accepts it (let bindings and function arguments). Codegen builds a temporary `ion_slice_T` fat pointer with `data` pointing at the array and `len = N`.

```ion
fn sum(s: &[]int) -> int { return s[0] + s[1]; }

fn main() -> int {
    let arr: [int; 3] = [10, 20, 30];
    return sum(&arr); // &[int; 3] coerced to &[]int at the call site
}
```


**Standard library enums (user-defined):**
- `Option<T>` – optional value (`Some(T)` or `None`) – can be defined as a generic enum
- `Result<T, E>` – success or error (`Ok(T)` or `Err(E)`) – can be defined as a generic enum

User-defined types:

- `struct` – product types (with generic and visibility support)
- `enum` – tagged unions (sum types) with tuple-style or struct-style variants (with generic and visibility support)
- `*T` – raw pointer types (for FFI; dereferencing only in `unsafe` blocks)
- `[T; N]` – fixed-size array types
- `[]T` – slice types

**Type aliases:**
- `type` – type aliases: `type Name = T;` and `type Name<T> = Type;` syntax

#### 4.2 Type Equivalence and Aliases

Type aliases created with `type Name = T;` are **transparent**: `Name` is equivalent to `T` for type checking. There is no nominal distinction introduced by `type`.

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

Function types are **first-class**: they may be stored in variables, passed as arguments, and returned.

**Fn literals** (capture-free only): an expression `fn(params) [-> R] { ... }` has type `fn(T1, T2, ...) -> R` matching its signature. The body may reference only parameters and locals declared inside the literal; any use of a binding from an outer scope is a compile-time error (`ClosureCapture`). Fn literals lower to plain C function pointers (each site gets a unique `static` function); there is no environment payload and no heap allocation.

Named functions and capture-free fn literals may not capture references that would violate the no-escape rule (see Section 5.4). **Capturing closures** (literals that move owned state from outer scopes) are not implemented; use named functions with extra parameters, explicit context structs, or `spawn` for move-only thread capture.

#### 4.4 Type Inference

Ion supports a **local, Hindley–Milner-inspired inference**:

- `let` bindings may omit the type if the initializer is present:

  ```ion
  let x = 10;          // x: int
  let s = String::new(); // s: String
  ```

- Function parameter and return types **must be annotated** (no inference across function boundaries).
- Generic type parameters on functions and types must be explicit at declaration sites, but may usually be inferred at call sites when unambiguous.
- `match` expressions: non-diverging arms must produce the same type (trailing expression or unit for an empty block). Arms that always `return`, `break`, or `continue` do not contribute a value to arm unification (see also Section 5.2). A match used as an rvalue where one arm diverges and another produces a value is a compile error. Within a single arm, control-flow paths that mix `return`/`break`/`continue` with value-producing fall-through are also a compile error. Numeric coercion between arm types follows the same rules as assignment.

The inference engine is intentionally limited:

- No higher-rank polymorphism.
- No complex trait constraints; only simple, **structural** `Send` constraints. For a generic type `Wrapper<T>`, each monomorphized instantiation `Wrapper<U>` is `Send` if and only if all of its fields (with `T` replaced by `U`) are `Send`.

#### 4.5 Type Casting and Array Assignment

- **Type casting**: `expr as Type` performs explicit numeric conversions (e.g., `f64 as int`).
- **Array element assignment**: `arr[i] = value` mutates a mutable array element. Subject to bounds checking unless inside `unsafe`.
- **Array initialization**: `[value; count]` fills an array with `count` copies of `value`, where `count` is a compile-time constant.

#### 4.6 Method Call Syntax

`expr.method(args)` is syntactic sugar for `Type::method(expr, args)`, where `Type` is the concrete type of `expr`.

- The compiler infers `&T` or `&mut T` for the receiver based on the function signature.
- Generic methods use existing monomorphization; type arguments may be inferred from the first argument.
- Qualified calls (`Vec::push(&mut vec, 10)`) remain valid.

#### 4.7 Control Flow Extensions

- **`loop { ... }`**: infinite loop; use `break` to exit and `continue` for the next iteration.
- **`for identifier in expr`**: iterates over `Vec<T>`, `[T; N]`, or `String`. Loop variable type is `T` for vectors and arrays, `u8` for strings (raw bytes).
- **`break`**: exits the innermost enclosing `while`, `loop`, or `for` loop.
- **`continue`**: skips to the next iteration of the innermost enclosing `while`, `loop`, or `for` loop. In `for` loops, the step (index increment) still runs.
- Both `break` and `continue` are compile errors outside of a loop body.
- **Match guards**: `pattern if expr => { ... }` where `expr` must be `bool`.
- **Struct-style enum variants**: `enum E { Ok { value: int }; }` with matching literals and patterns.

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

Ion does not expose a `Copy` marker to user code; whether a move is implemented as a copy is an implementation detail.

After an `if` statement, ownership is merged from branches that can reach the following code. Branches that always `return`, `break`, or `continue` are omitted from the merge. If two fall-through paths disagree on whether a binding is still valid, the compiler reports an error.

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
- While any lasting borrow of a variable is active, that variable cannot be used directly: no reads, assignment, or moves. This applies to copy types (e.g. `int`) as well as move-only types. Use the reference binding instead; `&mut` and `&` both enforce this exclusivity on the owner for the borrow's lifetime.

**Field and subpath borrows**

Lasting borrows through fields or indexing (e.g. `let r = &mut s.x`, `let r = &arr[i]`) register a borrow on the **root owner binding** (`s`, `arr`), not on a separate field slot. Ion does not support Rust-style disjoint field borrows: at most one `&mut` path into an owner at a time, and while the owner is borrowed no direct use of the owner (including other field paths or whole-owner `&mut s`) is allowed. Multiple shared `&` paths into the same owner remain allowed (`let a = &s.x; let b = &s.y`). Ephemeral `&` / `&mut` in call arguments are checked for aliasing at the use site but do not register lasting borrow counts.

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
        println("x = {}", *x); // ERROR: ClosureCapture (cannot reference outer x)
    }; // ERROR: closure escapes with reference
}
```

Capturing closures are not implemented; the compiler rejects any outer binding referenced from a fn literal body.

**Example (valid – borrow within function):**

```ion
fn print_twice(x: &int) {
    println("{}", x);
    println("{}", x);
} // borrow ends here
```

The type checker implements this by rejecting any attempt to **store or return** a type containing `&` or `&mut` outside the current function’s local variables. In particular, types such as `Option<&T>` or `Result<&T, E>` are only permitted as **local temporaries** within a function body; they cannot be returned, stored in longer-lived data structures, sent through channels, or cross thread boundaries. Standard library APIs are designed to avoid exposing such reference-carrying types across function boundaries.

#### 5.5 Destruction and `defer`

When a binding goes out of scope (e.g., block exit, function return, panic unwinding), its owned value is **dropped** exactly once:

- For structs, fields are dropped in declaration order.
- For enums, the active variant’s payload is dropped.
- For `Box<T>`, `T` is dropped, then the allocation is freed.

`defer` schedules an expression to be executed when the **current block** scope is left, in **last-in, first-out** order. On `return`, all enclosing block defers and drops run innermost-first before the function returns.

```ion
fn process() {
    defer log_cleanup(); // runs when process() returns

    if ready {
        defer arm_cleanup(); // runs when the if arm ends or on return through this arm
        // ...
    }
}
```

When a binding goes out of scope (block exit, function return), owned values with heap resources are dropped automatically:

- `Box<T>`: `ion_box_free`
- `Vec<T>`: `ion_vec_free`
- `String`: `ion_string_free`
- `Sender<T>` / `Receiver<T>`: `ion_channel_handle_drop` (refcounted; freed when both ends are dropped)
- Struct fields with owned heap types (`Box`, `Vec`, `String`, channels, or nested structs/enums containing them) are dropped in declaration order when the struct goes out of scope.
- Enum payloads are dropped for the active variant when the enum goes out of scope.

Uninitialized `Box`/`Vec`/`String` bindings are zero-initialized to `NULL` so drop is a no-op.

### 6. Memory Model

#### 6.1 Stack and Heap

- Local variables (`let`) are allocated on the stack by default.
- `Box<T>` allocates `T` on the heap. Destroying a `Box<T>` drops `T` and frees the heap allocation.
- `Vec<T>` and `String` internally use heap allocations; their behavior is defined by their APIs (Section 7).

Ion does **not** perform implicit heap allocation for:

- Captured closures (not implemented; fn literals are capture-free and use static functions only)
- Slices or views
- Temporaries (beyond what is required for expression evaluation)

Any heap allocation must be visible in the code via `Box`, `Vec`, `String`, or other standard types.

#### 6.2 Deterministic Destruction

Ion guarantees that every owned value is dropped exactly once when its owner’s scope ends, except when:

- The program terminates abnormally (e.g., process abort).

In particular:

- Early `return` from a function drops all owned locals (and runs block defers) before returning.
- `spawn` thread entry functions use the same scope-exit machinery; captures are dropped when the thread body finishes.
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
- Enums are compatible with “tagged unions” encoded according to a specified ABI (to be detailed later; FFI with enums may be restricted).

Functions may be declared `extern "C"`. Raw pointer types `*T` are available for FFI (distinct from safe references `&T`). Raw pointers are pass-through only in Ion code (no dereferencing). The compiler assumes:

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

Ion provides typed, bounded MPSC channels via built-in `Sender<T>` and `Receiver<T>` types and the `channel<T>()` function:

```ion
let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
send(&tx, 42);
let value = recv(&mut rx);
```

Semantics:

- `channel<T>()` is a built-in function that returns `(Sender<T>, Receiver<T>)`. Element type `T` must be `Send`.
- `Sender<T>` and `Receiver<T>` are move-only value types (not pointers).
- `send(&tx, value)` moves a value into the channel. Requires `&Sender<T>`.
- `recv(&mut rx)` moves a value out of the channel. Requires `&mut Receiver<T>`. Blocks until a value is available.
- `send` blocks when the buffer is full; `recv` blocks when empty.
- Tuple destructuring is supported: `let (tx, rx) = channel<int>();`
- Both `Sender<T>` and `Receiver<T>` are `Send` when `T: Send`, so either end may be moved between threads.
- Channel handles share a refcounted backing channel. Dropping either handle decrements the refcount; the channel is freed when the last handle is dropped at scope exit.

#### 7.3 `Send` Property

The `Send` property marks types that are safe to transfer to another thread by value:

- Primitive types (`int`, `bool`, etc.) are `Send`.
- `Box<T>` is `Send` if `T: Send`.
- `Vec<T>` is `Send` if `T: Send`.
- `String` is `Send`.
- `Option<T>` is `Send` if `T: Send`.
- `Result<T, E>` is `Send` if both `T: Send` and `E: Send`.
- `Sender<T>` and `Receiver<T>` are `Send` if `T: Send`.
- `(T1, T2, ...)` is `Send` if every element type is `Send`.
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
    fn get_ref(self: &Vec<T>, index: int) -> Option<&T>;
    fn set(self: &mut Vec<T>, index: int, value: T);
}
```

Note that:

- `Vec<T>` is `Send` if `T: Send`.
- `Vec::new()` and `Vec::with_capacity()` infer `T` from a `let` type annotation when present (e.g. `let v: Vec<i32> = Vec::new()`).
- `Vec::get()` and `Vec::pop()` return `Option<T>` to handle out-of-bounds or empty cases. Both **move** the element out of the vector. To preserve vector length after a read-only scan, either use `Vec::get_ref()` (below) or copy fields and `Vec::set()` a rebuilt struct literal to put the value back (nested `Vec` fields still move on put-back).
- `Vec::get_ref()` returns `Option<&T>`: a **local, stack-only borrow** of an in-place element. It does not move or hollow the slot. The result is only valid as a short-lived binding within the current function (for example in a `match` arm). It cannot be returned, stored in structs or enums, sent on channels, or cross `spawn`. While an `&T` from `get_ref` is active, the root owner of the vector (the binding behind `&Vec<T>`) is shared-borrowed: `&mut Vec<T>` on that owner, `Vec::set`, `Vec::push`, and `Vec::pop` on the same vector are rejected until the borrow ends. Out-of-range or negative indices yield `Option::None`. Nested inspection (`order.lines` then `get_ref`) follows the same root-owner borrow rules as field paths (Section 5.3).
- `Vec::set()` requires a mutable reference and returns an error code (0 for success, non-zero for failure). After shared borrows from `get_ref` end, `Vec::set` on the same index is allowed.

For cross-function or long-lived access, Ion still favors an **index/handle style**: helpers return indices or keys and callers re-index within their own function bodies.

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
    fn push_byte(self: &mut String, b: u8);
    fn len(self: &String) -> int;
}
```

Note that:

- String literals can be directly assigned to `String` type: `let s: String = "hello";`
- `String::from()` creates a heap-allocated copy of a string literal.
- `String::push_str()` appends a string literal or an owned `String` (reads the source buffer).
- `String::push_byte()` appends a single byte to an existing `String`.
- `==` and `!=` compare UTF-8 byte content (value equality), not pointer identity.

`&str` is always a **borrowed view** into existing UTF-8 data; it cannot be returned or stored in long-lived structures in ways that would violate the no-escape rule. The standard library intentionally avoids APIs that would expose `&str` values across function boundaries in ways that require complex lifetime reasoning (e.g., `String::as_str` methods that return borrowed views).

#### 8.4 Channels

See Section 7.2 for channel semantics and API.

#### 8.5 File I/O

**Implemented (MVP):** `stdlib/fs.ion` provides whole-file read via POSIX `open`/`read`/`close` wrapped in `unsafe` blocks.

```ion
// stdlib/fs.ion
pub enum ReadResult { Ok(String); Err(int); }
pub fn read_to_string_result(path: String) -> ReadResult;
```

- `path` is an owned `String` (typically from a string literal or `String::from`).
- On success, returns `ReadResult::Ok` with file bytes as a `String` (raw UTF-8; no validation).
- On failure: `ReadResult::Err(-1)` if `open` fails, `ReadResult::Err(-2)` if `read` fails.
- **Platform:** POSIX and MinGW (`open`/`read`/`close`). Not available on MSVC-only toolchains without a POSIX compatibility layer.

Generic `Result<T, E>` lives in `stdlib/result.ion` for library authors; `fs` uses the concrete `ReadResult` enum.

Import with `import "stdlib/fs.ion" as fs;` then `fs::read_to_string_result(path)`.

**Deferred (not implemented):**

```ion
struct File { /* opaque, not Send unless specified */ }

// Sketch only; `impl` blocks are not valid Ion syntax today.
fn open(path: &String) -> Result<File, IOError>;
fn read(file: &mut File, buf: &mut Vec<u8>) -> Result<int, IOError>;
fn write(file: &mut File, buf: &Vec<u8>) -> Result<int, IOError>;
fn close(file: &mut File) -> Result<void, IOError>;

enum IOError {
    NotFound;
    PermissionDenied;
    UnexpectedEof;
    Other(int);
}
```

File handles would typically be **not `Send`**, to avoid subtle platform-dependent behavior across threads.

#### 8.6 Standard I/O Modules

The stdlib provides safe wrappers in `stdlib/io.ion`, `stdlib/fmt.ion`, and `stdlib/fs.ion`:

**`stdlib/io.ion`:**
- `io::print(s: String)` – print string to stdout
- `io::println(s: String)` – print string with newline
- `io::print_str(s: *u8, len: int)` – print raw bytes with length validation
- `io::print_int(n: int)` – print signed integer in decimal

**`stdlib/fmt.ion`:**
- `fmt::int_to_string(n: int) -> String`
- `fmt::print_int(n: int)`
- `fmt::println_int(n: int)`

**`stdlib/fs.ion`:**
- `fs::read_to_string_result(path: String) -> ReadResult` – read entire file (POSIX/MinGW; `Err(-1)` on open failure, `Err(-2)` on read failure)

All I/O functions wrap POSIX calls in safe Ion code. Import with `import "stdlib/io.ion" as io;` (or `fmt.ion`, `fs.ion`).

`String` exposes `.data` (`*u8`) and `.len` (`int`) fields for low-level access when needed.

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

### 10. Tooling and Known Limitations

#### 10.1 Project build (`ion-build`)

The `ion-build` binary is the default developer workflow for applications. It reads `ion.toml` at the project root (discovered by walking upward from the current working directory), then runs the full pipeline: transpile Ion to C, compile generated C, link with `runtime/ion_runtime.c`, and write the executable.

```powershell
cargo build --release --bin ion-build
.\target\release\ion-build.exe build
```

Manifest discovery walks upward from the current directory for a file named `ion.toml`. Pass `--manifest <path>` when cwd is not the example directory; the manifest's parent directory is the project root and `main` paths are relative to that directory.

`ion-compiler` remains available for codegen inspection, LSP internals, and integration tests that grep `.c` output. It does not require `ion.toml`.

**`ion.toml` fields (tooling, not language semantics):**

| Field | Required | Description |
|-------|----------|-------------|
| `name` | yes | Project name (informational) |
| `main` | yes | Entry `.ion` file, relative to the manifest directory |
| `output` | yes | Executable name (`.exe` added on Windows when linking) |
| `mode` | no | `single` (default) or `multi` for per-module `.c`/`.h` codegen |
| `out_dir` | no | Build output directory relative to project root (default: `target`) |
| `stdlib_paths` | no | Extra stdlib search directories (relative to project root) |
| `cflags` | no | Extra flags when compiling generated `.c` files (e.g. `-Drecv_sys=recv` for FFI name mapping) |
| `ldflags` | no | Extra flags passed when linking (e.g. `-lm` overrides) |
| `emit_in_source` | no | When `true`, emit `.c`/`.h`/`.o` next to sources instead of `out_dir` |

**Stdlib import resolution:** Import string literals such as `import "stdlib/io.ion" as io;` resolve through stdlib search paths, not only paths relative to the importing file. Search order:

1. Manifest `stdlib_paths` entries
2. `ION_STDLIB` environment variable (`;`-separated on Windows, `:` elsewhere)
3. `{project_root}/stdlib`
4. Install-relative `stdlib/` next to the compiler executable (walking up)

File-relative imports (`./`, `../`) and same-directory modules are resolved first. The CLI (`ion-compiler`, `ion-build`) and LSP share `build::discover_import_config` and the stdlib search order above (from `ion.toml` when present, otherwise by walking up for `stdlib/` and install-relative paths next to the executable).

On Windows with MinGW, linking adds `-lws2_32` automatically (channels, spawn, sockets). Honor `CC` for the C compiler (same as `tests/test_runner.sh`).

#### 10.2 Language server (LSP)

The `ion-lsp` binary and VS Code/Cursor extension provide:

- Syntax highlighting (TextMate grammar, no server required)
- Parse, import-resolution, and type diagnostics (multiple type-check errors per file when independent)
- Hover: expression types, symbol signatures, and attached `//` documentation prose when present (functions, structs, enums, type aliases, fields, variants, imports, file-level overview)
- Completion: prefix-filtered keywords, builtins, local symbols, `alias::item` module imports, and struct/enum members after `.` or `Type::`
- Go to definition: variables, functions (`foo`, `mod::func`), user methods, struct fields, enum variants, and type aliases
- Find references, document outline, signature help, and semantic tokens (functions, structs, enums, types, fields)
- Workspace refresh: re-checks open files when a watched `.ion` dependency changes on disk

Built-in methods (`Vec::push`, `String::len`, etc.) show signature hover but have no source location for go-to-definition.

The CLI `ion-compiler`, `ion-build`, and the LSP use `TypeChecker::check_program_collecting` to gather multiple independent type diagnostics. Import failures are reported per `import` statement via `Compiler::load_imports`.

Build with `cargo build --release --bin ion-lsp`. Rebuild after compiler or LSP changes; reload the editor window so `ion.lspPath` picks up the new binary. A stale `ion-lsp` or workspace `ion-compiler` can disagree with a freshly built CLI. Set `ion.lspPath` in editor settings to the executable path.

#### 10.3 Known limitations

Beta compatibility and runtime ABI notes live in [docs/BETA.md](docs/BETA.md)
and [docs/ABI.md](docs/ABI.md). Features listed below are either intentionally
constrained in the beta subset or unstable until a later release documents a
stronger contract.

- No trait bounds on generics
- String `for...in` iterates bytes (`u8`), not Unicode code points or graphemes
- `if`/`else` merge: ownership after an `if` is merged from branches that can fall through to the following code. A move in a branch that always `return`s, `break`s, or `continue`s does not block use after the `if`. If two fall-through paths disagree (one moved, one valid), it is still an error.
- `while`/`for` loops: a non-copy variable moved anywhere in the loop body is an error (repeated iteration would need the binding again); copy types and borrows are unchanged. For read-only scans over an owned `Vec<T>`, prefer `Vec::get_ref` (Section 8.2) or index/handle helpers; `Vec::get` move-out still requires consume-once or put-back per iteration.
- Match guards on the same variant are lowered to a single `switch` case with sequential `if` checks
- LSP go-to-definition for built-in methods (`Vec::push`, `String::len`, etc.) has no target (signature hover only)
- LSP go-to-definition for type names in type annotations (no source spans on `Type` AST nodes)
- Function types: capture-free fn literals implemented; no capturing closures, no generic `fn(T) -> R` type parameters, no method values as fn pointers
- Tuple values: no nested tuples, `==` on tuples, struct fields holding tuples, or generic `(T1, T2)` parameters. Flat tuples may hold owned heap types (for example `(Vec<T>, int)`).

### 11. Future Work (Non-Normative)

The following features are **not planned** for the current compiler:

- Asynchronous/await syntax and futures.
- Complex trait or typeclass systems.
- Macros and compile-time metaprogramming.
- Advanced iterator pipelines and zero-cost abstractions beyond the basics.
- Capturing closures (fn literals that move owned environment from outer scopes).

Any such addition must:

- Preserve the no-escape rule and simple ownership model.
- Not require GC or complex runtime machinery.

### 12. Appendix: Recommended Design Patterns (Non-Normative)

This appendix describes idioms that work well with Ion’s ownership and no-escape borrowing rules.  They are not part of the core semantics, but library and application authors are encouraged to follow them for clarity and safety.

#### 12.1 Handle / Index Access Instead of Borrowed Returns

Because Ion does not allow returning references from functions, helpers that would traditionally return `&T` should instead return:

- An **index** (e.g., `int`) into a collection, or
- A **key/handle** type that wraps an index or opaque identifier.

Within a single function, read-only inspection of `Vec<T>` elements can use `Vec::get_ref` (Section 8.2), which yields a local `Option<&T>` without move-out.

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

#### 12.2 Callback-Based Accessors

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

#### 12.3 Owned Results Instead of Borrowed Views

At API boundaries (public functions, module exports), favor **owned results** over borrowed views:

- Return `Vec<T>` instead of `&[T]`.
- Return `String` instead of `&str`.
- Return structs/enums that own their payloads instead of borrowing them.

Borrowed views (`&T`, `&str`, local `Option<&T>` temporaries) are best used **inside** a single function to avoid copying in tight loops, not as part of public APIs.

#### 12.4 Short-Lived Local View Types

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

#### 12.5 Concurrency: Message-Passing Ownership Transfer

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
        let job = recv(&mut rx);
        process(job.data);
    }
}

fn main() {
    let (tx, rx): (Sender<Job>, Receiver<Job>) = channel<Job>();

    spawn {
        worker(rx);
    };

    let job = Job { data: Vec::<u8>::new() };
    send(&tx, job);
}
```

This emphasizes that concurrency in Ion is about **moving ownership** between threads, not sharing it.

#### 12.6 Documentation Comments (Non-Normative)

Ion has no separate `///` or `//!` documentation syntax. Documentation is ordinary `//` line comments attached to declarations by **adjacency** during parsing (Go/Odin convention):

- Contiguous `//` lines immediately above an item, with **no blank line** between the last comment line and the declaration, document that item.
- Comments immediately above `pub` attach to the following declaration (`pub` is not part of the doc block).
- A blank line between the comment block and the declaration breaks association; the comments are not attached.
- File- or module-level overview: leading `//` lines at the top of a file before the first `import` or declaration attach to `Program`.
- The same rule applies to struct fields and enum variants inside their bodies.
- Trailing inline `//` on the same line as code is explanatory only and is not item documentation.

Doc comments supplement [ION_SPEC.md](ION_SPEC.md) for IDE hover and a future `ion doc` tool; they are not a second normative specification. `ion-lsp` shows signature or type text first, then a blank line, then attached prose when present. Imported symbols surface docs from the defining module's AST.

**Examples:**

```ion
// Safe console output helpers.
import "stdlib/io.ion" as io;

// Returns zero on success.
fn main() -> int {
    io::println(String::from("hi"));
    return 0;
}
```

```ion
// A point in 2D space.
struct Point {
    // Horizontal coordinate.
    x: int;
    y: int;
}
```

```ion
// Success or failure with an owned message.
enum Result<T, E> {
  // Operation succeeded.
  Ok(T);
  Err(E);
}
```

```ion
// Write an owned string to stdout followed by a newline.
pub fn println(s: String) {
    // ...
}
```

Section-divider comments in examples should be separated from declarations by a blank line so they are not attached as docs:

```ion
// ============================================
// Section title
// ============================================

struct Widget { value: int; }
```

