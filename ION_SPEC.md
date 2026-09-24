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
- Be stored on the heap or in an array (`Box<&T>`, `Vec<&T>`, `[ &T; N ]`), including as locals or parameters
- Be captured by closures that may escape their defining scope
- Be moved into channels
- Cross thread boundaries (e.g., passed to `spawn`, stored in `Send` values)

Any construct that would require reasoning about reference lifetimes beyond the current function body is rejected at compile time.  This implies that APIs which conceptually return “references” (for example, `Option<&T>`) must instead be expressed in terms of owned values, indices/handles, or callback-style access.

#### 1.3 Ownership Rules (Summary)

1. **Single ownership**: Every value has exactly one owner at a time.
2. **Move by default**: Assignment, passing as arguments, and returning values **moves** ownership.
3. **Lexical borrows**: `&T` and `&mut T` borrows are restricted to the current function’s stack frame.
4. **Channel transfers**: Sending a value through a channel moves ownership into the channel.
5. **No self-referential borrows**: Types cannot store `&T` / `&mut T` (or types that contain them) in their own fields. Recursive *owned* types are allowed when every size cycle passes through heap or pointer indirection (`Box<T>`, `Vec<T>`, or a raw pointer). A bare value cycle such as `struct Node { next: Node }` or `next: Option<Node>` is rejected as infinite size (insert `Box` / `Vec` / `*T`).

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

`fn`, `let`, `mut`, `struct`, `enum`, `type`, `capability`, `impl`, `const`, `if`, `else`, `while`, `for`, `loop`, `match`, `select`, `defer`, `return`, `break`, `continue`, `spawn`, `scope`, `channel`, `protocol`, `endpoint`, `send`, `recv`, `true`, `false`, `import`, `as`, `pub`, `extern`, `unsafe`.

Built-in type names `Box`, `Vec`, `String`, `Slice`, and `File` are tokenized as keywords for generic syntax (`Box<T>`, etc.) and builtin method qualification (`Slice::len`, `File::open`); they are not reserved as identifiers elsewhere.

Note: The `as` keyword is used both for module aliases (`import "file.ion" as name;`) and for type casting (`expr as Type`).

This list is intentionally small; future additions must justify their complexity.

#### 2.3 Unsafe Blocks and Safety Boundaries

The `unsafe` keyword marks code blocks where Ion's safety guarantees are suspended:

- **Array and slice bounds checking** is disabled for indexing and index assignment within `unsafe` blocks
- **FFI calls** to `extern "C"` functions must be wrapped in `unsafe` blocks

There is no unary `*` dereference operator. Raw pointers `*T` are pass-through values for FFI only.

##### Unsafe Contract

Code within `unsafe` blocks must manually uphold the following invariants:

1. **No out-of-bounds array or slice access**: All indices must be valid
2. **FFI memory**: the C callee owns nothing Ion still owns; Ion does not free C memory unless a documented API says so. Mutating `String` bytes through `.data` is an FFI contract violation (the buffer must remain well-formed UTF-8).
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

Integer `+`, `-`, and `*` wrap in two's complement on every integer type. Generated C uses a same-width unsigned operation, then casts back for signed types (the meaning does not depend on `-fwrapv`). `/` and `%` panic if the divisor is `0`, and panic on signed `MIN / -1` and `MIN % -1`. Shifts panic if the right operand is greater than or equal to the bit width of the left operand. The right operand stays unsigned. An in-range shift result wraps modulo `2^width` of the left operand (`4660u16 << 4` is `9024`). `<<` and unsigned `>>` use that same unsigned operation. Signed `>>` is arithmetic (sign-extending), implemented explicitly in generated C.
- Assignment: `=`, `+=` (compound assignment desugars to `x = x + e` for supported `+` types). Assignment targets may be locals, index expressions, or field paths on owned structs or `&mut Struct` receivers (for example `vm.ip += 1`). Replacing a field stores the owned field type. Reading a non-Copy field through `&` or `&mut` is still a borrow of that field.
- Type casting: `as` keyword for explicit type conversions
- Field access: `.`
- Postfix try: `?` on owned `Option<T>` and `Result<T, E>` (see Section 8.1)
- Address-of / borrow: `&` (borrow shared) and `&mut` (borrow exclusive)
- Channel I/O: `send(sender, value)`, `recv(receiver)`, `clone_sender(sender)`, and `channel<T>()` / `channel<T>(cap)` built-ins (see Section 7.2)

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
- Documentation uses the same `//` syntax; contiguous comment lines immediately above a declaration (no blank line between the last comment and the declaration) are attached to that item for IDE hover and future doc tools. See [§12.1](#121-documentation-comments-non-normative).

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

Line comments are discarded during lexing and do not appear in generated C. Adjacent `//` lines above declarations are recovered from source during parsing and attached to AST nodes (see [§12.1](#121-documentation-comments-non-normative)).

#### 3.2 Modules and Declarations

```ebnf
file             = { import_decl | top_decl } ;

import_decl      = "import" , string_lit , [ "as" , identifier ] , ";" ;

top_decl         = struct_decl
                 | enum_decl
                 | type_alias
                 | capability_decl
                 | impl_decl
                 | protocol_decl
                 | const_decl
                 | fn_decl ;

protocol_decl    = "protocol" , identifier , "{" , { protocol_step } , "}" ;
protocol_step    = ( "send" | "recv" ) , type_expr , ";"
                 | "end" , ";" ;

const_decl       = "const" , identifier , ":" , type_expr , "=" , expr , ";" ;
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

capability_decl  = "capability" , identifier , "{" , { capability_method } , "}" ;
capability_method = "fn" , identifier , "(" , params? , ")" , return_type? , ";" ;

impl_decl        = "impl" , type_params? , identifier , "for" , type_expr , "{" , { fn_decl } , "}" ;

fn_decl          = "fn" , identifier , type_params? , "(" , params? , ")" ,
                   return_type? , block ;

type_params      = "<" , type_param , { "," , type_param } , ">" ;
type_param       = identifier , [ ":" , trait_bound , { "+" , trait_bound } ] ;
trait_bound      = identifier ;  (* Copy, Eq, Send, or a capability name *)

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
                 | scope_stmt
                 | select_stmt
                 | unsafe_stmt
                 | ";" ;

unsafe_stmt      = "unsafe" , block ;

let_stmt         = "let" , identifier , [ ":" , type_expr ] ,
                   [ "=" , expr ] , ";" ;

expr_stmt        = expr , ";" ;

if_stmt          = "if" , expr , block , [ "else" , ( block | if_stmt ) ] ;

while_stmt       = "while" , expr , block ;

for_stmt         = "for" , identifier , "in" , expr , block ;

match_stmt       = "match" , expr , "{" , { match_arm } , "}" ;
match_arm        = pattern , [ "if" , expr ] , "=>" , block ;

return_stmt      = "return" , [ expr ] , ";" ;

break_stmt       = "break" , ";" ;

continue_stmt    = "continue" , ";" ;

defer_stmt       = "defer" , expr , ";" ;

spawn_stmt       = "spawn" , block , ";" ;

scope_stmt       = "scope" , block ;

select_stmt      = "select" , "{" , { select_arm } , "}" ;
select_arm       = [ "let" , identifier , "=" ] , recv_expr , "=>" , block
                 | identifier , "=>" , block
                 | identifier , "(" , expr , ")" , "=>" , block ;
```

`else if` chains use the `if_stmt` alternative after `else` (for example `else if cond { ... }`). The `in` token in `for_stmt` is required but is not otherwise reserved.

`select` is a reserved keyword. The `default` and `timeout` arm heads are contextual identifiers, not keywords: a recv arm is `recv(...) => { ... }` or `let v = recv(...) => { ... }`; a poll arm is `default => { ... }`; a timed wait is `timeout(ms) => { ... }`. At most one `default` or one `timeout` arm is allowed, never both. See Section 7.2.

#### 3.5 Expressions

The expression grammar is presented in precedence levels (from lowest to highest):

```ebnf
expr             = assign_expr ;

assign_expr      = assign_target , ( "=" | "+=" ) , assign_expr
                 | logical_or_expr ;
assign_target    = identifier | field_expr | index_expr ;

logical_or_expr  = logical_and_expr , { "||" , logical_and_expr } ;
logical_and_expr = comparison_expr , { "&&" , comparison_expr } ;

comparison_expr  = equality_expr ,
                   { ( "<" | ">" | "<=" | ">=" ) , equality_expr } ;

equality_expr    = bitwise_or_expr , { ( "==" | "!=" ) , bitwise_or_expr } ;

bitwise_or_expr  = bitwise_xor_expr , { "|" , bitwise_xor_expr } ;
bitwise_xor_expr = bitwise_and_expr , { "^" , bitwise_and_expr } ;
bitwise_and_expr = shift_expr , { "&" , shift_expr } ;

shift_expr       = additive_expr ,
                   { ( "<<" | ">>" ) , additive_expr } ;

additive_expr    = multiplicative_expr ,
                   { ( "+" | "-" ) , multiplicative_expr } ;

multiplicative_expr =
                   unary_expr ,
                   { ( "*" | "/" | "%" ) , unary_expr } ;

unary_expr       = ( "!" | "-" | "&" | "&mut" ) , unary_expr
                 | postfix_expr ;

postfix_expr     = primary_expr ,
                   { selector | index | call | cast | try_op } ;

selector         = "." , identifier ;
index            = "[" , expr , "]" ;
call             = "(" , arg_list? , ")" ;
arg_list         = expr , { "," , expr } ;

cast             = "as" , type_expr ;  (* type casting *)
try_op           = "?" ;  (* quoted terminal; not EBNF optional *)

primary_expr     = identifier
                 | literal
                 | "(" , expr , ")"
                 | struct_lit
                 | enum_lit
                 | array_lit
                 | fn_literal
                 | match_expr
                 | send_expr
                 | recv_expr
                 | channel_expr
                 | spawn_expr ;

match_expr       = "match" , expr , "{" , { match_arm } , "}" ;

send_expr        = "send" , "(" , expr , "," , expr , ")" ;
recv_expr        = "recv" , "(" , expr , ")" ;
channel_expr     = "channel" , "<" , type_expr , ">" , "(" , [ expr ] , ")" ;
spawn_expr       = "spawn" , block ;

fn_literal       = "fn" , "(" , params? , ")" , [ "->" , type_expr ] , block ;

array_lit        = "[" , ( expr_list | ( expr , ";" , int_lit ) ) , "]" ;
expr_list        = expr , { "," , expr } ;

literal          = int_lit | float_lit | bool_lit | string_lit ;

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
named_pattern_field = identifier , [ ":" , pattern ] ;
```

A named pattern field may omit `: pattern`. The field name is then a binding of that name: `Event::Set { state, code }` is `Event::Set { state: state, code: code }`. An explicit pattern is kept: `Event::Set { state: State::Done, code }` puns only `code`. Struct patterns use the same field form (`Point { x, y }`).

#### 3.6 Nested constructors

Nested constructors (`Outer::Wrap(Inner::A)`) are type-checked against the payload type, and if every covering arm uses nested constructors, those constructors must exhaust the inner enum (same "variant not covered" message, inner name). A later catch-all binding (`other =>`) covers remaining outer values, including other payloads of a nested constructor, and binds the whole outer scrutinee. The compiler lowers nested constructors to nested `match` on extracted payloads; codegen sees only the existing match form.

Channel send and receive use the `send` and `recv` built-ins. `channel<T>()` uses capacity 1. `channel<T>(cap)` sets a program-visible bound (`cap` is `int`, must be `>= 1`). `clone_sender` is a built-in call (Section 7.2).

### 4. Type System

#### 4.1 Primitive and Built-in Types

Ion includes the following primitive types:

- Machine-sized integer: `int` (signed integer matching the target platform's `int`)
- Signed integers: `i8`, `i16`, `i32`, `i64`
- Unsigned integers: `u8` (available for raw pointers), `u16`, `u32`, `u64`, `uint`
- Floating point: `f32`, `f64`
- Boolean: `bool`
- Unit / void: `void` (function return type with no value)
- `str`: unsized UTF-8 slice type; use as `&str` (borrowed view, stack-local only; see Section 8.3)

Each integer primitive exposes compile-time limits as `Type::MIN` and `Type::MAX` (for example `int::MIN`, `i32::MAX`, `u8::MIN`). These lower to C-safe literals (avoiding `-2147483648`-style unary overflow in generated C).

Additional built-in generic types:

- `Box<T>` – heap-allocated `T` with owning semantics (`Box::new()`, `Box::new_in()`, `Box::unwrap()`). The allocation is an allocator header followed by `T`. The Ion pointer addresses `T`. `Box::unwrap` moves `T` out and frees the allocation without dropping `T`.
- `Allocator` – `Copy` value of three function pointers and a `*u8` context. `heap()` is malloc, realloc, and free. `make_allocator` is legal only inside `unsafe`. `Vec`, `Box`, and `String` store a copy and use it on grow and drop. `Vec::new`, `Box::new`, and `String::new` use `heap()`.
- `Sender<T>` and `Receiver<T>` – move-only handles for the two ends of a bounded MPSC channel
- `JoinHandle<T>` – move-only handle for a joinable `spawn` expression. `T` is the block's return type and defaults to `void`. Not `Copy`. `Send` when `T` is `Send`. Drop detaches. `join` moves `T` out.
- `File` – owned OS file handle (not `Send`; drop closes)
- `Vec<T>` – growable heap-allocated vector (`Vec::new()`, `Vec::with_capacity()`, `Vec::push()`, `Vec::pop()`, `Vec::len()`, `Vec::capacity()`, `Vec::get()`, `Vec::get_ref()`, `Vec::set()`)
- `String` – well-formed UTF-8 heap-allocated string (`String::new()`, `String::from()`, `String::from_utf8()`, `String::get()`, `String::push_str()`, `String::push_byte()`, `String::len()`)
- `[T; N]` – fixed-size array of `N` elements of type `T`
- `[]T` – dynamically sized slice (fat pointer) of type `T` (`Slice::len()`, `Slice::get_ref()`)
- `(T1, T2, ...)` – fixed-size tuple value type (see §4.1.3)

#### 4.1.1 Array Safety

Fixed-size arrays `[T; N]` have the following safety properties:

- **Bounds checking**: Array indexing `arr[i]` performs runtime bounds checking by default
  - If `i < 0` or `i >= N`, the program panics with an error message via `ion_panic()`
  - The panic prints "Array index out of bounds" to stderr and aborts the program
  - Bounds checking can be disabled in `unsafe` blocks for performance-critical code
- **Compile-time size**: Array size `N` is an integer literal, a `const` item, or a `const` parameter.
- **Stack allocation**: Arrays are allocated on the stack by default
- **Index type**: The index expression may be any integer type (`int`, `i32`, `u32`, etc.).
- **C lowering**: `[T; N]` lowers to a named C array typedef (`typedef int arr_int_2[2];`, nested `typedef arr_int_2 arr_arr_int_2_3[3];`) so nested arrays and `Box<[T; N]>` / `Vec<[T; N]>` are valid C type specifiers. Indexing stays `a[i]`. Functions still cannot return a C array type; array returns decay to a pointer to the first element.

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
  - If `i < 0` or `i >= Slice::len(s)`, the program panics with an error message via `ion_panic()`
  - The panic prints "Slice index out of bounds" to stderr and aborts the program
  - Bounds checking can be disabled in `unsafe` blocks for performance-critical code
- **Fat pointer representation**: Slices are `(data, len)` pairs at runtime

**Safe slice access:**
```ion
fn first(s: &[]int) -> int {
    return s[0]; // OK: bounds checked against the slice length at runtime
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

A reference to a fixed-size array `&[T; N]` coerces to `&[]T` where the type checker accepts it (let bindings and function arguments). Codegen builds a temporary fat-pointer slice with `data` pointing at the array and `len = N`.

```ion
fn sum(s: &[]int) -> int { return s[0] + s[1]; }

fn main() -> int {
    let arr: [int; 3] = [10, 20, 30];
    return sum(&arr); // &[int; 3] coerced to &[]int at the call site
}
```

**Length:**

```ion
// Slice::len(s: &[]T) -> int
```

`Slice::len` returns the number of elements in the slice as `int`. Method form `s.len()` desugars to `Slice::len`; fixed arrays may use `arr.len()` or `Slice::len(&arr)` via array-to-slice coercion. Field access `s.len` is invalid: slices are not structs. An empty slice has length `0`. The call does not register a lasting borrow.

**Non-panicking element borrow:**

```ion
// Slice::get_ref(s: &[]T, index: int) -> Option<&T>
```

`Slice::get_ref` returns a **local, stack-only** `Option<&T>` for an in-bounds element (same no-escape and root-owner shared-borrow rules as `Vec::get_ref` in Section 8.2). Negative or out-of-range indices yield `Option::None` instead of panicking. Method form `s.get_ref(i)` desugars to `Slice::get_ref`; fixed arrays may use `arr.get_ref(i)` or `Slice::get_ref(&arr, i)` via array-to-slice coercion. While the borrow is live, the root owner cannot be mutated or moved.

#### 4.1.3 Tuple Values

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
- Nested field access `t.0.0` is two tuple indices. The lexer does not treat `0.0` after `.` as a float.
- `==` and `!=` compare tuples elementwise when every element type is `Eq` (generated C does not use struct `==`). `JoinHandle` and `File` are not `Eq`.
- Tuple types may appear as struct fields and as generic type arguments (`fn first<T>(p: (T, int)) -> T`).


**Standard library enums (user-defined):**
- `Option<T>` – optional value (`Some(T)` or `None`) – can be defined as a generic enum
- `Result<T, E>` – success or error (`Ok(T)` or `Err(E)`) – can be defined as a generic enum

User-defined types:

- `struct` – product types (with generic and visibility support)
- `enum` – tagged unions (sum types) with tuple-style or struct-style variants (with generic and visibility support)
- `*T` – raw pointer types (for FFI; pass-through only, no unary `*` deref)
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

**Fn literals**: an expression `fn(params) [-> R] { ... }` that names no outer binding has type `fn(T1, T2, ...) -> R` and lowers to a `static` C function pointer.

A literal that names an outer owned binding moves those bindings into a compiler-generated struct. The value is that struct. It is not coerced to `fn(...) -> R` and it is not a heap object. A call that moves a non-`Copy` capture consumes the closure. A call that only reads `Copy` captures, or that uses `&` or `&mut` on the closure's own fields, can run again. The closure is `Send` only when every capture is `Send`. Moving a non-`Copy` binding into that struct is `BorrowConflict` while a `&` or `&mut` of that same binding is still in scope, including a reference that is never read. A use of the owner after every carrier's last use, outside a closure, follows Section 5.3.

A capture whose type is `&T` or `&mut T`, or a direct `&name` or `&mut name`, is `ClosureCapture`. Named functions and fn literals may not capture references that would violate the no-escape rule (see Section 5.4).

#### 4.4 Type Inference

Ion supports a **local, Hindley–Milner-inspired inference**:

- `let` bindings may omit the type if the initializer is present:

  ```ion
  let x = 10;          // x: int
  let s = String::new(); // s: String
  ```

- Function parameter and return types **must be annotated** (no inference across function boundaries).
- Generic type parameters on functions and types must be explicit at declaration sites, but may usually be inferred at call sites when unambiguous.
- `match` expressions: non-diverging arms must produce the same type (trailing expression or unit for an empty block). Arms that always `return`, `break`, or `continue` do not contribute a value to arm unification (see also Section 5.2). An rvalue `match` may mix a diverging arm with a value-producing arm (postfix `?` desugars to this shape). Within a single arm, control-flow paths that mix `return`/`break`/`continue` with value-producing fall-through are also a compile error. Numeric coercion between arm types follows the same rules as assignment.

The inference engine is intentionally limited:

- No higher-rank polymorphism.
- Generic enum variants with no payload (`Option::None`) infer type arguments only from an adjacent expected type (a `let` annotation, a struct field, a function parameter / call argument including built-in value parameters such as `send` and `Box::new`, a return type, an assignment target, or an element of an array or tuple literal including `[value; N]` repeat arrays and enum variant payloads). They do not take `T` from a later statement; without that context the compiler requires an annotation.
- Generic enum constructor payloads are checked against the expected generic instantiation when one is known. Integer literals are `int`, not placeholders for a type parameter `T`: `let x: Option<bool> = Option::Some(1)` and `let x: Option<String> = Option::Some(1)` are type errors.
- Generic type parameters may declare optional bounds (`Copy`, `Eq`, `Send`, or a capability name). Bounds are checked at monomorphization: each concrete instantiation must satisfy every bound on the corresponding parameter. `Copy`, `Eq`, and `Send` are structural (see Section 4.8). Other bounds name capabilities declared in the program.
- Structural `Send` still applies per instantiation even without an explicit bound: for a generic type `Wrapper<T>`, each monomorphized `Wrapper<U>` is `Send` if and only if all of its fields (with `T` replaced by `U`) are `Send`. An unbounded function parameter `T` is not itself `Send`; see Sections 4.8 and 7.3.

#### 4.5 Type Casting and Array Assignment

- **Type casting**: `expr as Type` performs explicit numeric conversions (e.g., `f64 as int`). Integer `as` keeps the low bits of the destination width (`0x12ff as u8` is `0xff`).
- **Array element assignment**: `arr[i] = value` mutates a mutable array element. Subject to bounds checking unless inside `unsafe`.
- **Array initialization**: `[value; count]` fills an array with `count` copies of `value`, where `count` is a compile-time constant.

#### 4.6 Method Call Syntax

`expr.method(args)` is syntactic sugar for a call of `method` on the concrete type of `expr`.

- The compiler infers `&T` or `&mut T` for the receiver based on the function signature.
- Generic methods use existing monomorphization; type arguments may be inferred from the first argument.
- Qualified calls (`Vec::push(&mut vec, 10)`) remain valid.
- A capability method (Section 4.8) is chosen when exactly one visible impl for that type provides `method`. Two capabilities with the same method name require the qualified form `Show::show(value)`.
- Inside a generic function, `value.method()` on a type parameter uses the capability named in that parameter's bounds. The call is monomorphized to the impl for the concrete type.

#### 4.7 Control Flow Extensions

- **`loop { ... }`**: infinite loop; use `break` to exit and `continue` for the next iteration.
- **`for identifier in expr`**: iterates over `Vec<T>`, `[T; N]`, `String` (raw bytes as `u8`), or a value that implements `Iter<T>` (Section 8.1). The loop variable is an owned `T`. An `Iter<T>` value is moved into the loop. `next` returns `Option<T>`, never `&T`.
- **`break`**: exits the innermost enclosing `while`, `loop`, or `for` loop.
- **`continue`**: skips to the next iteration of the innermost enclosing `while`, `loop`, or `for` loop. In `for` loops, the step (index increment) still runs.
- Both `break` and `continue` are compile errors outside of a loop body.
- Owned values and `defer`s in scopes exited by `break` or `continue` are cleaned up as specified in Section 5.5.
- **Match guards**: `pattern if expr => { ... }` where `expr` must be `bool`.
- **Struct-variant field pun**: `Event::Set { state, code }` is `Event::Set { state: state, code: code }`. `Event::Set { code, .. }` ignores the other fields. A named-field pattern must list every field or include `..`.
- **Literal patterns**: `bool`, integer types, and string literals. Inclusive integer ranges are `lo..hi` with integer literal endpoints. `lo` greater than `hi` is a compile error.
- **Or-patterns**: `A | B`. Every alternative binds the same names.
- **Struct patterns**: `Point { x, y }` and `Point { x, .. }`. A bare field name is punning. `@` binds the scrutinee and the inner pattern: `q @ Point { x: 1, y }`.
- **Tuple rest**: `let (a, .., b) = t` binds `a` to the first element and `b` to the last. `..` appears once. A refutable pattern in `let` is a compile error.
- **Non-enum match**: `match` on an integer, `bool`, `String`, or struct tests arms in order. `bool` is exhaustive when `true` and `false` are covered. Other non-enum matches are exhaustive when an arm is irrefutable (`_`, a binding, or a struct pattern whose fields are irrefutable) and has no guard. Matching `&T` or `&mut T` reborrows the place: copy fields bind as `T`, and other fields bind as `&T` or `&mut T`. The arm does not move or drop the referent. Enum match stays one `switch` on the variant tag.
- **Struct-style enum variants**: `enum E { Ok { value: int }; }` with matching literals and patterns.

#### 4.8 Capabilities and Bounds

A **capability** is a named set of function signatures. `Self` in those signatures is the implementing type.

```ion
capability Show {
    fn show(self: &Self) -> int;
}

impl Show for Point {
    fn show(self: &Point) -> int { return self.x; }
}

fn paint<T: Show>(value: &T) -> int { return value.show(); }
```

Rules:

- One impl per type per capability, in the same module as that struct or enum. No blanket impls, no capability inheritance, and no `dyn`.
- `Copy`, `Eq`, and `Send` are structural compiler capabilities. User code cannot declare or impl those names.
- A generic impl monomorphizes to ordinary functions, the same path as a generic function.
- `value.show()` resolves to the impl when exactly one visible capability supplies `show`. Otherwise the call is `Show::show(value)`.
- Bounds use the existing `T: Foo + Bar` syntax. A concrete type satisfies a user capability when an impl of that capability exists for it. A type parameter satisfies it when its bounds include it. Instantiation checks declared bounds.

`Copy`, `Eq`, and `Send` stay structural:

| Bound | Meaning (structural) |
|-------|----------------------|
| `Copy` | Copied rather than moved: primitives, references, function pointers, and tuples, arrays, structs, and enums whose fields or payloads are `Copy` and that have no `impl Drop`. `Box`, `Vec`, `String`, channels, `JoinHandle`, `File`, raw pointers, and protocol endpoints are not `Copy`. `Allocator` is `Copy`. |
| `Eq` | Type supports `==` and `!=` with correct semantics (primitives, `String`, references, function pointers, arrays and tuples of `Eq` types, structs and enums whose fields or payloads are all `Eq`). |
| `Send` | Type may cross thread boundaries (Section 7.3). |

A type parameter is `Send` only when it has a `Send` bound in the current scope (`T: Send`). Unbounded `T` is not `Send`. `channel<T>()` and `spawn` capture use that predicate: `fn wrap<T>(v: T) { channel<T>(); }` is a type error; `fn wrap<T: Send>(v: T) { channel<T>(); }` is allowed. Instantiation still checks declared bounds, so `wrap(&x)` is `TraitBoundNotSatisfied` when `wrap` requires `T: Send`. Names that are neither a declared `struct`/`enum` nor a type parameter are not `Send`.

At each monomorphization site (generic call, struct or enum construction, type-alias substitution), the compiler substitutes concrete types for parameters and rejects any instantiation where a concrete type does not satisfy a declared bound. Unknown bound names are rejected at the declaration site.

#### 4.9 Const

`const NAME: T = expr;` defines a const item. `T` may be `bool` or any integer width when the value fits that width. `expr as Type` keeps the low bits of the destination width. `match` on `bool` and integers is allowed: literals, inclusive ranges `lo..hi`, `_`, bindings, and or-patterns. `const fn` may call other const functions and use `if`. Loops, `spawn`, `defer`, and `unsafe` are illegal in `const fn`. A failing evaluation is a compile error.

`const_assert(expr);` fails compilation when `expr` is not `true`.

`fn pad<const N: int>(buf: &mut [u8; N])` is a const parameter. A call instantiates `N` from the argument's array length. `[T; N]` accepts that parameter or a const item.

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
    let xs: Vec<int> = Vec::new();
    take(xs);       // xs moved into take
    // xs is now invalid; any use is a compile error
}
```

#### 5.2 Copy and Move

By default, Ion types are **move-only**. A `Copy` type is copied, and the original binding stays valid (Section 4.8).

A consuming call inside a binary operator, a unary operator, or an index expression marks that move. A place read (`x`, `s.f`, `a[i]`) stays a read.

Whether a move of a non-`Copy` value is implemented as a byte copy is an implementation detail. The `Copy` bound on generic parameters (Section 4.8) names types that are copied rather than moved at the ownership level.

After an `if` statement, ownership is merged from branches that can reach the following code. Branches that always `return`, `break`, or `continue` are omitted from the merge. If two fall-through paths disagree on whether a binding is still valid, the compiler reports an error.

After a `match` expression, ownership is joined from arms whose bodies can fall through. Diverging arms are omitted. If fall-through arms disagree on whether a binding is still valid, the compiler reports an error at the match. Nested unstructured leftovers (for example a `let` whose initializer is a fully diverging match) stay conservative AST-structured analysis, not a CFG rewrite.

Postfix `?` consumes its operand like a `match` scrutinee: the `Option` or `Result` is moved, and the success payload is a fresh owned rvalue. The error arm diverges via `return` (Section 8.1).

After a `while`, `loop`, or `for` statement, ownership uses the same join lattice on structured edge snapshots (not a full CFG):

- **Reentry** (back-edge): contributors are body fall-through and `continue`. A binding that is valid at loop entry must stay valid on every reentry contributor; otherwise the compiler reports an error at the loop.
- **Exit** (after the loop): for `while`/`for`, contributors are the loop-head state (ordinary condition-false / zero-trip exit) plus every `break` snapshot; for `loop`, contributors are `break` snapshots only. When those contributors disagree on whether a binding is still valid, the binding is moved after the loop. Each exit that still owns the value drops it on that exit. A later use is `UseAfterMove`. Contributors that agree stay as they are. A `return` inside a loop is checked normally and does not contribute to reentry or exit joins.
- A block stops at the first `return`, `break`, or `continue`. Statements after that are unreachable and are not a reentry edge.
- A `while` whose body falls through is still a reentry edge. The compiler does not prove that the condition stays false.

Beta limitations that remain are precision under-approximations of this join model, not a second ownership thesis. Section 11 exclusions reject features that would force GC, richer lifetimes, or heavy runtime machinery; they are separate from checker precision.

#### 5.3 Borrowing

References provide scoped, non-owning access:

- `&T` – shared borrow, read-only.
- `&mut T` – exclusive borrow. Mutation through `&mut` is supported for **struct field paths** on an `&mut Struct` receiver and for **callee parameters** of type `&mut T` (for example `Vec::push`, `recv`). There is no unary `*` and no assign-through a bound scalar reference such as `let r: &mut int = &mut x; r = 1;`. Write the owner (or a field) directly and let the borrow checker reject conflicts while a lasting borrow is live.

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
- While a lasting borrow of the whole owner is active, that variable cannot be used directly: no reads, assignment, or moves. This applies to copy types (e.g. `int`) as well as move-only types. A lasting borrow stays live until the last use of every binding that holds it. A copy (`let c = a`), a field or index reborrow (`let d = &mut a.x`, `let d = &mut a[i]`), a tuple or `match` result that yields the reference, an assignment (`c = a`), an enum value (`Option::Some(a)`), and a struct literal (`Hold { v: a }`) are carriers of that same loan. Creating the reference inside that value is the same loan (`Hold { v: &mut s }`, `Option::Some(&mut s)`, `(&mut s.x, 1)`, or a `match` arm that yields `&mut s.x`). The loan is live everywhere the value that holds the pointer is live. A `match` arm that stores that reference in a local and then yields the local keeps the loan on the match result. An enum, struct, or array that holds the reference keeps the loan until that binding leaves scope. A nested block does not end an outer loan while any of those bindings is used again later in the outer block. A use inside a loop covers the whole loop. A carrier used only in one arm of an `if` is not live in the other arm. A loan still held by a binding after the `if` stays live. Ephemeral borrows in call arguments stay on that call. A `match` arm that binds a reference payload binds the pointer. It does not copy or drop the referent. A field whose type is already a reference is that pointer. Passing it to a function does not take its address again. A move of a non-`Copy` binding into a closure conflicts with a carrier of that binding that is still in scope, even when the carrier is never read.

**Field and subpath borrows**

`s.x` and `s.y` are different places, so `let a = &mut s.x; let b = &mut s.y` is allowed while both borrows are live. Nested paths conflict when they share a field prefix: `s.a.b` conflicts with `s.a.c` and with `s.a`. A literal index is its own path segment, so `&mut a[0]` and `&mut a[1]` do not conflict. A non-literal index borrows the whole owner. `&mut a` conflicts with `&mut a[0]`. Call arguments use the same rule, so `both(&mut a[0], &mut a[0])` conflicts and `both(&mut a[0], &mut a[1])` does not. Slice paths borrow the whole owner. A borrow of the whole owner conflicts with every path into that owner. Multiple shared `&` paths remain allowed. Ephemeral `&` / `&mut` in call arguments are checked at the call and do not register a lasting borrow.

#### 5.4 No-Escape Rule (Formal)

Each reference has a **borrow scope** equal to a syntactic region within a single function body. The compiler enforces:

- References cannot be:
  - Stored in struct fields or enum variants.
  - Stored in `Box<T>`, `Vec<T>`, or arrays, even as function-local bindings or parameters (`Box<&T>` / `Vec<&T>`).
  - Returned from functions.
  - Assigned into global or static variables.
  - Captured by function literals (closures) that may escape the current function.
  - Sent into channels.
  - Stored in values that may cross threads (i.e., be `Send`).

**Example (invalid – returning a reference):**

```ion
fn max_ref(a: &int, b: &int) -> &int {
    return a; // ERROR: cannot return reference
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
    let (tx, rx): (Sender<&int>, Receiver<&int>) = channel<&int>(); // ERROR: references cannot be sent
}
```

**Example (invalid – closure capturing reference and escaping):**

```ion
fn make_printer(x: &int) -> fn() {
    return fn() {
        let _y: &int = x; // ERROR: ClosureCapture (cannot reference outer x)
    }; // ERROR: closure escapes with reference
}
```

A reference capture is `ClosureCapture`. An owned outer binding moves into the closure value (Section 4.3).

**Example (valid – borrow within function):**

```ion
struct Counter { n: int; }

fn read_twice(c: &Counter) -> int {
    return c.n + c.n;
} // borrows end here
```

The type checker implements this by rejecting any attempt to **store or return** a type containing `&` or `&mut` outside the current function’s local variables. In particular, types such as `Option<&T>` or `Result<&T, E>` are only permitted as **local temporaries** within a function body; they cannot be returned, stored in longer-lived data structures, sent through channels, or cross thread boundaries. `Box<&T>` and `Vec<&T>` are not stack temporaries: boxing or pushing a reference copies the pointer onto the heap and is a compile-time error even as a local or parameter. Standard library APIs are designed to avoid exposing such reference-carrying types across function boundaries.

**Example (invalid – boxing a reference):**

```ion
fn main() -> int {
    let x: int = 1;
    let b: Box<&int> = Box::new(&x); // ERROR: cannot store a reference in Box
    return 0;
}
```

#### 5.5 Destruction and `defer`

When a binding goes out of scope, its remaining owned value is dropped exactly once. Scope exit includes block fall-through, `return`, `break`, and `continue`. `ion_panic` prints to stderr and `abort()`s; drops do not run.

Drop order:

- A user `impl Drop` runs once, then fields, payloads, and elements drop in the order below. The `drop` body may read and mutate `self` through `&mut`. It must not move fields out. Builtin `File`, `Vec`, `String`, `Box`, and channel drops stay in the compiler and run as part of that later field drop. `ion_panic` still aborts with no drops.
- A type with a `Drop` impl cannot be partially moved. That includes moving a non-Copy field out through a `match` pattern. A wildcard or `..` leaves the field to be dropped with the value. Ending a value early is a nested block. There is no second manual `drop` call.
- Locals in one drop scope: reverse declaration order.
- Struct fields: declaration order.
- Enum variant payloads: declaration order of the active variant's fields or positional payloads.
- Tuple fields: positional order (`f0`, `f1`, ...).
- Array and `Vec<T>` elements at whole-value destruction: increasing index `0 .. len` (then `ion_vec_free` for `Vec`).
- `Box<T>`: drop `T` (when it needs destruction), then `ion_box_free`.
- `String`: `ion_string_free`.
- `Sender<T>` / `Receiver<T>`: `ion_channel_sender_drop` / `ion_channel_receiver_drop` (separate sender and receiver counts; the backing channel is freed when both counts reach 0). Remaining buffered elements of `T` are dropped before the buffer is freed.
- `SendResult<T>`: drop `Closed(T)` payload when that variant is destroyed; `Sent` has no payload.

`Box::unwrap` moves `T` out first, then frees the allocation; it does not drop `T`.

Within one block, that block's `defer`s run in last-in, first-out order, then that block's remaining locals drop in reverse declaration order. Nested exit is innermost-first.

`defer` schedules an expression to run when the **current block** scope is left. On `return`, every enclosing block is unwound to the function epilogue (defers then locals, innermost-first).

`break` destroys owned values and runs defers in scopes exited by the break, through and including the loop body scope, then exits the loop. `continue` performs the same cleanup for scopes exited by the continue, then begins the next iteration. For a `for` loop, the iteration step runs after continue cleanup.

Partial moves drop only remaining owned parts. Moved-out pointer fields stay nulled (structs and tuple `fN` slots).

Library methods that drop as a side effect of an algorithm stay unspecified until that method documents an order. `Vec::set` drops the previous element before overwrite (Section 8.2).

```ion
fn process() {
    defer log_cleanup(); // runs when process() returns

    if ready {
        defer arm_cleanup(); // runs when the if arm ends or on return through this arm
        // ...
    }
}
```

Uninitialized `Box`/`Vec`/`String` bindings are zero-initialized to `NULL` so drop is a no-op.

### 6. Memory Model

#### 6.1 Stack and Heap

- Local variables (`let`) are allocated on the stack by default.
- `Box<T>` allocates `T` on the heap. Destroying a `Box<T>` drops `T` and frees the heap allocation.
- `Vec<T>` and `String` internally use heap allocations; their behavior is defined by their APIs (Section 7).

Ion does **not** perform implicit heap allocation for:

- Move closures (the capture struct is a stack value, Section 4.3, not a heap allocation)
- Slices or views
- Temporaries (beyond what is required for expression evaluation)

Any heap allocation must be visible in the code via `Box`, `Vec`, `String`, or other standard types.

#### 6.2 Deterministic Destruction

Ion guarantees that every owned value is dropped exactly once when its owner’s scope ends, except when:

- The program terminates abnormally (e.g., process abort).

In particular:

- Early `return` from a function drops all owned locals (and runs block defers) before returning.
- `break` and `continue` drop owned values and run defers in the scopes they exit, through and including the loop body (Section 5.5). For `for`, the iteration step runs after continue cleanup.
- `spawn` thread entry functions use the same scope-exit machinery; captures are dropped when the thread body finishes.
- `spawn`ed threads manage their own stacks independently.
- `ion_panic` prints a message and `abort()`s. Drops do not run. Allocation failure, `Vec`/`String` grow failure, `spawn` failure, and channel create failure panic this way instead of returning NULL or ignoring a status code. `panic::abort` passes `String` data through `ion_abort_bytes`, which calls `ion_panic`.
- Replacing a field (`s.f = new`) drops the previous field value, then stores `new`. The right-hand side is evaluated first.

#### 6.3 Aliasing and Safety

Given the ownership and borrowing rules:

- Ion programs cannot exhibit use-after-free or double-free at runtime.
- Data races across threads are prevented if the `Send` rules are correctly enforced (Section 7).

Any violation of safety rules is a **compile-time error**, not undefined behavior.

#### 6.4 C ABI and Layout

By default, `struct` and `enum` layouts are **C-compatible**:

- Field order and alignment follow the target C ABI.
- No hidden metadata is inserted into structs.
- Enums lower to a tagged union: `int tag` plus `union { struct variant_N { ... payload fields ... }; ... } data`. Variant index `N` is declaration order. Payload-less variants still set `tag` and leave `data` unused. FFI must match this layout from the same compiler version.

Functions may be declared `extern "C"`. Linkage other than `"C"` is a compile-time error. Raw pointer types `*T` are available for FFI (distinct from safe references `&T`). Raw pointers are pass-through only in Ion code (the language has no unary `*` deref). The compiler assumes:

- Ion compiles to C functions with straightforward signatures.
- Parameter and return passing follows the C calling convention of the target platform.
- The C callee owns nothing Ion still owns. Ion does not free C memory unless a documented API says so.

### 7. Concurrency and `Send`

#### 7.1 Threads and `spawn`

`spawn { ... };` as a statement creates a new OS thread and detaches it. The same form used as an expression yields `JoinHandle<T>`:

```ion
spawn {
    // body
};

let h: JoinHandle<int> = spawn {
    return 7;
};
let n: int = join(h);
```

`T` is the type of `return` in the block and defaults to `void`. A non-void `T` must be `Send`. The thread writes `T` into heap storage. `join` moves `T` out and frees that storage. `join` on `JoinHandle<void>` waits and returns nothing.

The block may capture **owned values** from the enclosing scope **by move** only. Capturing references is disallowed:

```ion
fn work(v: Vec<int>) { }

fn main() {
    let v: Vec<int> = Vec::new();

    spawn {
        // v is moved into this thread; main cannot use v after this point
        work(v);
    };
}
```

Attempting to use `v` after `spawn` is a compile-time error.

`JoinHandle<T>` is not `Copy`. It is `Send` when `T` is `Send`. Dropping an unused handle detaches the thread (`ion_thread_detach`). `join(handle)` waits for the thread, moves `T` out, and consumes the handle. Statement `spawn { };` outside `scope` does not produce a handle.

`scope { ... }` joins every `JoinHandle` still owned in that block, in reverse creation order, and drops a `void` result. A handle moved out of the scope is not joined there. A statement `spawn` inside `scope` is joinable. A child must not wait on its parent.

#### 7.2 Channels

Ion provides typed, bounded MPSC channels via built-in `Sender<T>` and `Receiver<T>` types and the `channel<T>()` / `channel<T>(cap)` function. Programs declare the conventional enums (same pattern as `Vec::get` returning `Option<T>`):

```ion
enum Option<T> {
    Some(T);
    None;
}
enum SendResult<T> {
    Sent;
    Closed(T);
}
enum TrySendResult<T> {
    Sent;
    Full(T);
    Closed(T);
}
enum TryRecvResult<T> {
    Msg(T);
    Empty;
    Closed;
}

let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
send(&tx, 42);
let value: Option<int> = recv(&mut rx);
```

Semantics:

- `channel<T>()` returns `(Sender<T>, Receiver<T>)` with buffer capacity **1**. `channel<T>(cap)` uses `cap` slots. `cap` has type `int`. A literal `cap < 1` is a compile-time error. A non-literal `cap < 1` panics at runtime. Element type `T` must be `Send`. There is no unbounded channel.
- `Sender<T>` and `Receiver<T>` are move-only value types (not pointers). `Sender<T>` is not `Copy`.
- `clone_sender(&tx) -> Sender<T>` copies the sender handle and increments the sender count. `Receiver<T>` cannot be cloned.
- `send(&tx, value) -> SendResult<T>` moves a value into the channel. Requires `&Sender<T>`. The value is checked against `T`, so `send(&tx, Option::None)` infers from the sender. Returns `SendResult::Sent` when ownership moved into the buffer. Returns `SendResult::Closed(value)` when no receiver remains, giving `T` back. An unused result at `send(&tx, v);` still drops `Closed(T)` so the value cannot leak. `send` blocks while the buffer is full and a receiver still exists.
- `recv(&mut rx) -> Option<T>` moves a value out of the channel. Requires `&mut Receiver<T>`. Returns `Option::Some(v)` while messages remain. Returns `Option::None` after every sender has been dropped and the buffer is empty. Blocks until one of those holds. Does not yield an uninitialized `T`.
- `try_send(&tx, value) -> TrySendResult<T>` is nonblocking. Runtime status `0` / `-2` / `-1` maps to `Sent` / `Full(T)` / `Closed(T)`. An unused statement `try_send(...)` still drops `Full(T)` and `Closed(T)`.
- `try_recv(&mut rx) -> TryRecvResult<T>` is nonblocking. Runtime status `0` / `-2` / `-1` maps to `Msg(T)` / `Empty` / `Closed`.
- `select { ... }` waits on a set of `recv` arms. The chosen arm copies the message in the same `try_recv` (no peek-then-recv). Waiters are registered before the empty recheck so a concurrent `send` cannot park forever on a message already in the buffer. A bound `let v = recv(&mut rx)` has type `Option<T>`. An unbound `recv(&mut ry) =>` still takes the message and drops the unused `Option<T>`. `default =>` polls (`timeout_ms = 0`). `timeout(ms) =>` waits up to `ms` milliseconds (`ms` is `int`; a literal `ms < 0` is a compile error; a runtime `ms < 0` panics). Without `default` or `timeout`, the runtime waits forever (`timeout_ms = -1`). At most one of `default` or `timeout`.
- Tuple destructuring: `let (tx, rx): (Sender<T>, Receiver<T>) = channel<T>();` (annotation required).
- `Sender<T>` and `Receiver<T>` are `Send` when `T: Send`, so either end may be moved between threads.
- Disconnect: the runtime tracks `sender_count` and `receiver_count` separately. Last `Sender` drop (including clones) disconnects receive, wakes waiters, and does not destroy the channel while a `Receiver` lives. Last `Receiver` drop disconnects send and wakes waiters. The backing channel is freed when both counts reach 0. Remaining buffered elements of `T` are dropped first.

#### 7.3 `Send` Property

The `Send` property marks types that are safe to transfer to another thread by value:

- Primitive types (`int`, `bool`, etc.) are `Send`.
- `Box<T>` is `Send` if `T: Send`.
- `Vec<T>` is `Send` if `T: Send`.
- `String` is `Send`.
- `Option<T>` is `Send` if `T: Send`.
- `Result<T, E>` is `Send` if both `T: Send` and `E: Send`.
- `Sender<T>` and `Receiver<T>` are `Send` if `T: Send`.
- `SendResult<T>` is `Send` if `T: Send`.
- `TrySendResult<T>` and `TryRecvResult<T>` are `Send` if `T: Send`.
- `(T1, T2, ...)` is `Send` if every element type is `Send`.
- `JoinHandle<T>` is `Send` when `T` is `Send`.
- `Allocator` is `Send`.
- A protocol endpoint is `Send` when every payload is `Send`.
- `File` is **not** `Send`.
- Any type containing a reference (`&T`, `&mut T`) is **not** `Send`.

User-defined `struct` and `enum` types are `Send` if and only if **all of their fields / payloads are `Send`**. For generic types, this rule is applied **per instantiation**: e.g., `Wrapper<int>` may be `Send` while `Wrapper<NonSend>` is not, depending on the fields.

A type parameter is `Send` if and only if it has a `Send` bound in the current scope. Unbounded parameters and unknown type names (not a declared `struct`/`enum`, not a type parameter) are not `Send`. `channel<T>()` and `spawn` capture use this predicate.

The compiler checks `Send` when:

- Moving a value into a `spawn` body.
- Sending a value into a channel whose receiver may be on another thread.

Any attempt to move a non-`Send` type across threads is a compile-time error.

#### 7.4 Protocol endpoints

`channel<T>()` and `clone_sender` stay the untyped MPSC bag. A protocol is a second endpoint that cannot be cloned:

```ion
protocol Ping {
    send int;
    recv bool;
    end;
}

let (client, server) = endpoint<Ping>();
let client1 = send(client, 7);
let (n, server1) = recv(server);
```

`send` and `recv` consume the endpoint and return it at the next step. `recv` returns `(payload, next endpoint)`. The other end is the dual: send and recv are swapped. After the last step, another `send` or `recv` is an error. Drop before `end` is allowed. There is no clone.

Each direction is one existing channel of a compiler-generated message struct. The two ends are crossed because a channel is unidirectional. Payload types must be `Copy` and `Send`. The endpoint is not `Copy`.

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

Postfix `?` is visible sugar for `match` plus `return`. `Option` and `Result` keep the rules above, including the same `E` and no conversion between error types. There is no `From` / `FromResidual` and no `Try` capability.

Another owned enum is eligible when it has one success variant: the variant named `Ok` or `Some` with exactly one positional payload, or, when those names are absent, the single variant that has a payload. Every other variant returns unchanged. The function's return type must be that same enum. `ReadResult { Ok(String); Err(int); }` and `Parse { Done(int); Bad; Empty; }` qualify. `SetResult { Ok; OutOfBounds; }` does not, because `Ok` has no payload. A bare error enum does not. Two payload variants and no `Ok` or `Some` stay an error. Named fields make the enum ineligible. It is not legal on `&Option<T>`, `&Result<T, E>`, or a reference to any other enum. Channel result enums stay ineligible when they do not have that one success variant.

```ion
// Result<T, E> in a function that returns Result<U, E>  (U may differ; E must be equal)
let x: T = expr?;
// same as
let x: T = match expr {
    Result::Ok(v) => { v; }
    Result::Err(e) => { return Result::Err(e); }
};

// Option<T> in a function that returns Option<U>
let x: T = expr?;
// same as
let x: T = match expr {
    Option::Some(v) => { v; }
    Option::None => { return Option::None; }
};
```

`?` is postfix, same tier as `.field`, `[index]`, and call. `Option?` is only legal in a function or fn literal whose return type is `Option<_>`. `Result?` is only legal when the return type is `Result<_, E>` with the same `E`. For any other eligible enum, the function must return that same enum. Mixing `Option` and `Result` is a type error. `?` is a compile error inside `spawn` bodies (spawn lowers to a different C function). Propagating `Option<&T>` still hits `ReferenceEscape` on return.

`stdlib/iter.ion` declares:

```ion
capability Iter<T> {
    fn next(self: &mut Self) -> Option<T>;
}
```

`for x in iter` on a type that implements `Iter<T>` moves the iterator and loops on `next` until `None`. `T` is the impl's payload. `next` returns an owned value, never `&T`. The built-in desugar for `Vec<T>`, `String`, and `[T; N]` does not use this capability.

`stdlib/option.ion` and `stdlib/result.ion` add capture-free helpers. Import them explicitly (`import "stdlib/option.ion" as option`). `Option` in `option.ion` is the same enum shape programs already declare; do not declare it again in a file that imports `option.ion` or a module that imports it (`string.ion`, `map.ion`).

```ion
pub fn map<T, U>(value: Option<T>, f: fn(T) -> U) -> Option<U>;
pub fn and_then<T, U>(value: Option<T>, f: fn(T) -> Option<U>) -> Option<U>;
pub fn unwrap_or<T>(value: Option<T>, fallback: T) -> T;
pub fn expect<T>(value: Option<T>, message: String) -> T;

pub fn map<T, U, E>(value: Result<T, E>, f: fn(T) -> U) -> Result<U, E>;
pub fn and_then<T, U, E>(value: Result<T, E>, f: fn(T) -> Result<U, E>) -> Result<U, E>;
pub fn unwrap_or<T, E>(value: Result<T, E>, fallback: T) -> T;
pub fn expect<T, E>(value: Result<T, E>, message: String) -> T;
```

`map` and `and_then` call `f` and move `T` into it. `unwrap_or` returns the success payload or `fallback`. `expect` returns the success payload. On `None` or `Err` it calls `panic::abort`, which calls `ion_abort_bytes` and does not return. `ion_abort_bytes` calls `ion_panic`. `panic.ion` is imported by `option.ion` and `result.ion`.

Call them as `option::map` and `result::map`. There is no prelude.

#### 8.2 `Vec<T>`

`Vec<T>` is a growable, heap-allocated sequence of `T`.

Essential API (compiler builtins; method calls desugar to the qualified forms below):

```ion
// Vec::new() -> Vec<T>
// Vec::with_capacity(cap: int) -> Vec<T>
// Vec::push(vec: &mut Vec<T>, value: T)
// Vec::pop(vec: &mut Vec<T>) -> Option<T>
// Vec::len(vec: &Vec<T>) -> int
// Vec::capacity(vec: &Vec<T>) -> int
// Vec::get(vec: &Vec<T>, index: int) -> Option<T>
// Vec::get_ref(vec: &Vec<T>, index: int) -> Option<&T>
// Vec::set(vec: &mut Vec<T>, index: int, value: T) -> SetResult
```

Method syntax (`vec.push(x)`, `vec.get_ref(i)`) desugars to the qualified forms above.

Note that:

- `Vec<T>` is `Send` if `T: Send`.
- `Vec::new()` and `Vec::with_capacity()` infer `T` from a `let` type annotation when present (e.g. `let v: Vec<i32> = Vec::new()`).
- `Vec::get()` and `Vec::pop()` return `Option<T>` to handle out-of-bounds or empty cases. Both **move** the element out of the vector. For a `T` that needs destruction, `Vec::get` hollows the slot (zero-fills it) so later vector drop does not free the moved value again. Copy `T` is left in place (move and copy are indistinguishable). To preserve vector length after a read-only scan, either use `Vec::get_ref()` (below) or copy fields and `Vec::set()` a rebuilt struct literal to put the value back (nested `Vec` fields still move on put-back).
- `Vec::get_ref()` returns `Option<&T>`: a **local, stack-only borrow** of an in-place element. It does not move or hollow the slot. The result is only valid as a short-lived binding within the current function (for example in a `match` arm). Match arms bind the element as `&T`; for enum elements, an inner `match` on that binding dispatches variants directly (no unary `*` deref). Copy fields in struct or enum variant patterns bind as `T`; non-copy fields bind as `&T`. Codegen uses `T*` for types with owned fields and copies by value for copy types, so repeated scans over `Vec<struct-with-nested-Vec>` do not double-free nested fields. It cannot be returned, stored in structs or enums, sent on channels, or cross `spawn`. While an `&T` from `get_ref` is active, the root owner of the vector (the binding behind `&Vec<T>`) is shared-borrowed: `&mut Vec<T>` on that owner, `Vec::set`, `Vec::push`, and `Vec::pop` on the same vector are rejected until the borrow ends. Out-of-range or negative indices yield `Option::None`. Nested inspection (`order.lines` then `get_ref`) follows the same root-owner borrow rules as field paths (Section 5.3). Field paths through `&Struct` that are already references (for example `order.lines` when `order: &Order`) are passed to `Vec` methods without an extra `&`.
- `Vec::set()` requires a mutable reference and returns conventional `SetResult` (`Ok` or `OutOfBounds`), not `int`. Programs declare `enum SetResult { Ok; OutOfBounds; }` (same pattern as `Option`). When `T` needs destruction, the previous element at that index is dropped before the new value is written. After shared borrows from `get_ref` end, `Vec::set` on the same index is allowed. An unused statement `Vec::set(...)` still produces `SetResult` in generated C.

For cross-function or long-lived access, Ion still favors an **index/handle style**: helpers return indices or keys and callers re-index within their own function bodies. When slots in a growable table can be reused, prefer `Handle` / `Arena<T>` in Section 8.6 over a raw `int` index.

#### 8.3 `String`

`String` is a growable, heap-allocated UTF-8 string.

Essential API (compiler builtins; method calls desugar to the qualified forms below):

```ion
// String::new() -> String
// String::from(s: &str) -> String
// String::from_utf8(bytes: Vec<u8>) -> Option<String>
// String::get(s: &String, index: int) -> Option<u8>
// String::push_str(s: &mut String, other: &str)
// String::push_byte(s: &mut String, b: u8)
// String::len(s: &String) -> int
```

`str` is a primitive slice type; `&str` is a borrowed UTF-8 view `(pointer, length)`.

The `String` invariant is well-formed UTF-8 (RFC 3629: no overlong encodings, no surrogates, no code points above U+10FFFF). `from_literal` / `push_str` / `from` validate. `String::from_utf8` consumes a `Vec<u8>` and returns `None` if the bytes are ill-formed. `push_byte` may only append `0x00..=0x7F` (ASCII); a non-ASCII byte panics. `for` over `String` is still **byte** iteration over a validated buffer. Raw I/O uses `Vec<u8>`. Mutating through `.data` is an `unsafe`/FFI contract violation.

Note that:

- String literals can be directly assigned to `String` type: `let s: String = "hello";`
- The same literal coercion applies when a string literal is passed as a call argument to a parameter typed `String` (not only in `let` bindings).
- `String::from()` creates a heap-allocated copy of a string literal.
- `String::get()` returns `Option<u8>` for a byte at `index`. Negative or out-of-range indices yield `Option::None` (non-panicking complement to `s[i]`, which still aborts on OOB). The result is a by-value `u8` (copy); no lasting borrow is registered. Method form `s.get(i)` desugars to `String::get`.
- `String::push_str()` appends a string literal or an owned `String` (reads the source buffer). The appended bytes must be well-formed UTF-8.
- `String::push_byte()` appends a single ASCII byte (`0x00..=0x7F`) to an existing `String`.
- `==` and `!=` compare UTF-8 byte content (value equality), not pointer identity.

- `String::from` and stdlib APIs accepting `&str` also accept string literals and `&String` at call sites.
- `&str` is always a **borrowed view** into existing UTF-8 data; it cannot be returned or stored in long-lived structures in ways that would violate the no-escape rule. The standard library intentionally avoids APIs that would expose `&str` values across function boundaries in ways that require complex lifetime reasoning (e.g., `String::as_str` methods that return borrowed views).

#### 8.4 Channels

Programs declare conventional `Option<T>`, `SendResult<T>`, `TrySendResult<T>`, and `TryRecvResult<T>` enums (same pattern as `Vec::get`). `channel<T>()` has capacity 1; `channel<T>(cap)` sets the bound. `clone_sender(&tx)` is MPSC. `send` returns `SendResult<T>`; `recv` returns `Option<T>`; `try_send` / `try_recv` are nonblocking. `select` waits on recv arms. Last sender drop unblocks `recv` with `None`. See Section 7.2.

#### 8.5 File I/O

**Whole-file UTF-8 read:** `stdlib/fs.ion` provides POSIX `open`/`read`/`close` wrapped in `unsafe` blocks.

```ion
// stdlib/fs.ion
pub enum ReadResult { Ok(String); Err(int); }
pub fn read_to_string_result(path: String) -> ReadResult;
```

- `path` is an owned `String` (typically from a string literal or `String::from`).
- Bytes are accumulated as `Vec<u8>`, then converted with `String::from_utf8`.
- On success, returns `ReadResult::Ok` with a well-formed UTF-8 `String`.
- On failure: `ReadResult::Err(-1)` if `open` fails, `ReadResult::Err(-2)` if `read` fails, `ReadResult::Err(-3)` if the file is not well-formed UTF-8.
- **Platform:** POSIX and MinGW (`open`/`read`/`close`). Not available on MSVC-only toolchains without a POSIX compatibility layer.

Generic `Result<T, E>` lives in `stdlib/result.ion` for library authors; `fs` uses the concrete `ReadResult` enum.

Import with `import "stdlib/fs.ion" as fs;` then `fs::read_to_string_result(path)`.

**Owned `File` (compiler builtin):** streaming I/O with an owned handle. Not `Send`. Drop closes.

```ion
// File::open(path: &String) -> Option<File>
// File::create(path: &String) -> Option<File>
// File::read(file: &mut File, buf: &mut Vec<u8>) -> int
// File::write(file: &mut File, buf: &Vec<u8>) -> int
// File::close(file: &mut File)
```

- `File::open` uses `"rb"`; `File::create` uses `"w+b"`. Failure is `Option::None`.
- `File::read` fills up to `buf.capacity` and sets `buf` length to bytes read. Returns the byte count, or `-1` on error. Use `Vec::with_capacity` so there is room to read.
- `File::write` writes `buf.len` bytes. Returns the byte count, or `-1` on error.
- `File::close` closes if still open. Drop also closes (safe to close twice).
- **Platform:** POSIX and MinGW (`fopen`/`fread`/`fwrite`/`fclose`). Not MSVC-only.

#### 8.6 `Handle` / `Arena<T>`

Generational handles live in `stdlib/handle.ion` (a library, not compiler builtins). Import with `import "stdlib/handle.ion" as handle;`.

```ion
pub struct Handle { index: int; generation: int; }
pub enum Slot<T> {
    Occupied { generation: int, value: T };
    Free { generation: int, next: int };
}
pub struct Arena<T> {
    slots: Vec<Slot<T>>;
    free_head: int;
    live: int;
}
pub enum Take<T> { Hit(T); Miss; }

pub fn invalid() -> Handle;
pub fn copy(h: &Handle) -> Handle;
pub fn new<T>() -> Arena<T>;
pub fn len<T>(arena: &Arena<T>) -> int;
pub fn contains<T>(arena: &Arena<T>, h: Handle) -> bool;
pub fn insert<T>(arena: &mut Arena<T>, value: T) -> Handle;
pub fn remove<T>(arena: &mut Arena<T>, h: Handle) -> Take<T>;
```

Compiler builtin (not a library function):

```ion
// Arena::get_ref(arena: &Arena<T>, h: Handle) -> Option<&T>
```

- `Handle` is two `int` fields (`Send`). Invalid is `index: -1, generation: 0`. Ion does not treat user structs as `Copy`; `copy(&h)` reconstructs from the fields when a by-value call would consume the only binding.
- `insert` reuses `free_head` or `Vec::push`. `remove` bumps generation, links the slot into the free list, and returns `Take::Hit` with the owned value or `Take::Miss` for stale/OOB handles.
- Peek cannot be a returning library function (no-escape). Prefer the compiler builtin `Arena::get_ref(&arena, h) -> Option<&T>` (stack-local, same no-escape as `Vec::get_ref`). `Handle` is moved; use `handle::copy` when the binding must stay. Occupied generation must match. Through `&Arena` / `&World`, method form `arena.get_ref(h)` is valid. The older `arena.slots.get_ref(h.index)` peek remains legal.
- Annotate `let mut arena: Arena<int> = handle::new();`. Through `&mut World`, `world.entities` is already `&mut Arena` (call `insert(world.entities, v)`). On an owned `World`, pass `&mut world.entities`.
- `slots` is public because the library peek used `Vec::get_ref`. Direct mutation of `slots` can break the free list.
- `Arena::get_ref` is a compiler builtin (not a library function). IR method lowering must not treat it as `Vec::get_ref`.
- `Vec::new()` inside `new<T>` must be annotated (`let slots: Vec<Slot<T>> = Vec::new();`).
- `Handle` is not phantom `Handle<T>`. Mixing two arenas with the same handle is a user error; `contains` still rejects stale generations.

See [verified-patterns.md](.cursor/skills/writing-ion-code/references/verified-patterns.md) (Index and handle search) and [examples/handle_table/](examples/handle_table/).

#### 8.7 Standard I/O Modules

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

Owned streaming I/O uses the compiler builtin `File` (Section 8.5), not this module.

All I/O functions wrap POSIX calls in safe Ion code. Import with `import "stdlib/io.ion" as io;` (or `fmt.ion`, `fs.ion`).

`String` exposes `.data` (`*u8`) and `.len` (`int`) fields for low-level access when needed.

#### 8.8 String helpers

`stdlib/string.ion` imports `option.ion`. These functions read a `String` by byte index and keep the well-formed UTF-8 invariant. `slice`, `trim_ascii`, and `split_once` build a new `String` with `String::from_utf8`.

```ion
pub fn slice(s: &String, start: int, end: int) -> String;
pub fn contains(s: &String, needle: &String) -> bool;
pub fn starts_with(s: &String, prefix: &String) -> bool;
pub fn ends_with(s: &String, suffix: &String) -> bool;
pub fn trim_ascii(s: &String) -> String;
pub struct Split { head: String; tail: String; }
pub fn split_once(s: &String, sep: u8) -> Option<Split>;
```

`contains`, `starts_with`, and `ends_with` compare bytes. An empty needle makes `contains` return true. `trim_ascii` drops leading and trailing ASCII space, tab, CR, and LF (`32`, `9`, `13`, `10`). `split_once` splits on the first `sep` byte and returns `None` when it is absent. `slice` returns an empty `String` when the range is empty or `from_utf8` rejects the bytes.

#### 8.9 `Hash`

`stdlib/hash.ion` declares:

```ion
capability Hash {
    fn hash(self: &Self) -> int;
}
```

Integer primitives and `String` satisfy `Hash` without a user impl. `value.hash()` on those types calls the runtime mixers `ion_hash_int`, `ion_hash_i8`, `ion_hash_i16`, `ion_hash_i32`, `ion_hash_i64`, `ion_hash_u8`, `ion_hash_u16`, `ion_hash_u32`, `ion_hash_u64`, `ion_hash_uint`, and `ion_hash_string`. A user type impls `Hash` in the same module as the type. `Copy`, `Eq`, and `Send` stay structural and are not impls of this capability.

#### 8.10 `HashMap<K, V>`

`stdlib/map.ion` is an open-addressed table. Import `stdlib/map.ion`. It imports `option.ion` and `hash.ion`.

```ion
pub struct HashMap<K, V> {
    slots: Vec<Slot<K, V>>;
    len: int;
    used: int;
    cap: int;
}

pub fn new<K, V>() -> HashMap<K, V>;
pub fn insert<K: Hash + Eq, V>(map: &mut HashMap<K, V>, key: K, value: V);
pub fn remove<K: Hash + Eq, V>(map: &mut HashMap<K, V>, key: K) -> Option<V>;
pub fn len<K, V>(map: &HashMap<K, V>) -> int;
pub fn for_each<K, V>(map: &mut HashMap<K, V>, f: fn(&V) -> int);
pub fn into_iter<K, V>(map: HashMap<K, V>) -> MapIter<K, V>;
```

`new` allocates 16 empty slots. `len` is the number of full slots. `used` counts full slots and tombstones. `insert` doubles the table when `used * 2 >= cap`. `grow` moves live full slots into the new vector and drops the previous slot vector. Tombstones are not copied. Probes in `insert` and `remove` borrow a slot with `Vec::get_ref` (`Empty`, `Tomb`, or `Full`, and `==` on the key). That borrow ends before any `Vec::set` or moving `Vec::get` on the same vector. `Vec::set` runs only for the index that receives a new pair or a tombstone. A replaced `V` is still dropped by `Vec::set`. `remove` uses `Vec::get` only on the matching index, because that move is the returned `V`, and leaves a tombstone so later probes still see keys past the hole. `for_each` calls `f` with a borrow of `V` from `get_ref` and does not write the slot back. That borrow does not escape the call. `into_iter` consumes the map. `MapIter` implements `Iter<(K, V)>` and `next` pops slots until it yields an owned pair or `None`. Keys are `Hash + Eq`. They do not have to be `Copy`: `==` reads both keys, and `hash` borrows. User structs are not `Copy` unless every field is `Copy` and the struct has no `impl Drop`. Integer keys and `String` use the compiler `Hash` impl. A field of `&mut HashMap` is already `&mut Vec`, so slot access passes `map.slots` to `Vec::get_ref`, `Vec::get`, and `Vec::set`. Occupied slots are dropped with the map because the slot vector owns them. `HashMap<K, V>` is `Send` when `K` and `V` are `Send`.

#### 8.11 Math, path, env, and time

Import each module explicitly.

`stdlib/math.ion`:

```ion
pub fn abs(x: int) -> int;
pub fn min(a: int, b: int) -> int;
pub fn max(a: int, b: int) -> int;
pub fn clamp(x: int, lo: int, hi: int) -> int;
pub enum MathError { DivByZero; }
pub fn checked_div(a: int, b: int) -> Result<int, MathError>;
```

`checked_div` returns `Err(MathError::DivByZero)` when `b` is 0. Otherwise it returns `a / b`.

`stdlib/path.ion` treats `/` and `\` as separators and returns owned `String` values:

```ion
pub fn file_name(path: &String) -> String;
pub fn parent(path: &String) -> String;
pub fn join(dir: &String, name: &String) -> String;
```

`file_name` is the suffix after the last separator, or the whole path when there is none. `parent` is the prefix before that separator, or an empty `String` when there is none. A leading separator is kept as a one-byte parent. `join` inserts `/` when `dir` does not already end in a separator.

`stdlib/env.ion`:

```ion
pub enum EnvError { Missing; NotAscii; }
pub fn get(name: String) -> Result<String, EnvError>;
```

`get` copies the process environment value into a new `String`. `Missing` means the name is unset. `NotAscii` means a copied byte is outside `0..=127`. Values longer than 255 bytes are truncated.

`stdlib/time.ion`:

```ion
pub fn millis() -> int;
```

`millis` is milliseconds since the Unix epoch, masked so the `int` is non-negative.

### 9. Examples and Edge Cases

These examples illustrate core semantics (moves, borrows, channels). **Copy-paste idioms** (index/handle search, Vec put-back, concurrency patterns) live in [`.cursor/skills/writing-ion-code/references/verified-patterns.md`](.cursor/skills/writing-ion-code/references/verified-patterns.md), checked against `tests/` and `examples/`.

#### 9.1 Basic Ownership

See [verified-patterns.md](.cursor/skills/writing-ion-code/references/verified-patterns.md) (Ownership move) and [tests/test_move_basic.ion](tests/test_move_basic.ion).

#### 9.2 Borrowing Within Functions

```ion
struct Counter { n: int; }

fn bump(c: &mut Counter) {
    c.n += 1;
}
```

See [tests/test_field_assign_plus.ion](tests/test_field_assign_plus.ion).

#### 9.3 Concurrency with Channels

See [verified-patterns.md](.cursor/skills/writing-ion-code/references/verified-patterns.md) (Concurrency and ownership transfer), [examples/spawn_channel/spawn_channel.ion](examples/spawn_channel/spawn_channel.ion), and [tests/test_spawn_channel.ion](tests/test_spawn_channel.ion).

`Sender<T>` and `Receiver<T>` are `Send` values moved between threads; communication is by channel, not shared references.

#### 9.4 Rejected Patterns

See [verified-patterns.md](.cursor/skills/writing-ion-code/references/verified-patterns.md) (Rejected patterns) and negative tests under `tests/`. Summary:

- Returning references.
- Storing references in structs or enums.
- Storing references in `Box`, `Vec`, or arrays (`Box<&T>`), including as locals or parameters.
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

**Runtime and stdlib discovery:** `ion-build` locates `runtime/ion_runtime.h` by walking upward from the project root, then from the current working directory if needed. Stdlib search paths walk upward from the project root (see below) and also check install-relative `stdlib/` next to the compiler executable. A build fails with `runtime/ion_runtime.h not found` when neither walk finds a `runtime/` directory; keep the project under a tree that contains `runtime/` and `stdlib/` (repo root or an unpacked release archive).

`ion-compiler` remains available for codegen inspection, LSP internals, and integration tests that grep `.c` output. It does not require `ion.toml`. The input must be a **program entry file** that defines `fn main() -> int` (after imports are merged); helper-only modules are compiled as imports of that entry, not as standalone `ion-compiler` inputs.

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
| `cflags_windows` | no | Extra `cflags` applied only on Windows (e.g. `-Dclose=closesocket` for Winsock) |
| `cflags_unix` | no | Extra `cflags` applied only on Linux and macOS |
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

- No trait objects, blanket impls, or capability inheritance. `Copy`, `Eq`, and `Send` stay structural and cannot be implemented by user code. Other bounds name a `capability` declared in the program (Section 4.8).
- String `for...in` iterates bytes (`u8`), not Unicode code points or graphemes
- `if`/`else` merge: ownership after an `if` is merged from branches that can fall through to the following code. A move in a branch that always `return`s, `break`s, or `continue`s does not block use after the `if`. If two fall-through paths disagree (one moved, one valid), it is still an error.
- Match arms that fall through join ownership the same way. Nested unstructured leftovers (for example a `let` whose initializer is a fully diverging match) stay AST-structured analysis without a CFG rewrite.
- Loop ownership uses structured reentry/exit joins (Section 5.2). A `while` whose body falls through is still a reentry edge. Proving that the condition will not be true again needs value-sensitive analysis, and that stays rejected. Remaining conservatism is AST-structured analysis without a full CFG (for example nested unstructured control flow may still under-approximate). A use inside a loop covers the whole loop. For read-only scans over an owned `Vec<T>`, prefer `Vec::get_ref` (Section 8.2) or index/handle helpers; `Vec::get` move-out still requires consume-once or put-back per iteration when the body reenters.
- Match guards on the same variant are lowered to a single `switch` case with sequential `if` checks
- LSP go-to-definition for built-in methods (`Vec::push`, `String::len`, etc.) has no target (signature hover only)
- LSP go-to-definition for type names in type annotations (no source spans on `Type` AST nodes)
- Function types: a capture-free fn literal is a function pointer. A literal that moves owned outer bindings is a closure value and is not a function pointer. A reference capture is `ClosureCapture`. There are no generic `fn(T) -> R` type parameters and no method values as fn pointers.
- Tooling: IR lowering copies types and resolved method callees from the type checker (`TypeInfo`). Codegen reads those fields. It does not re-infer expression types or reclassify method receivers. A missing type, a missing resolved method, or a `METHOD::` callee after a successful check is a compiler bug, not a fallback to `int`.

### 11. Future Work (Non-Normative)

The following features are **not planned** for the current compiler:

- Asynchronous/await syntax and futures.
- Trait objects, blanket impls, and capability inheritance.
- Macros and compile-time metaprogramming.
- Iterator pipelines and iterators that yield references.

Any such addition must:

- Preserve the no-escape rule and simple ownership model.
- Not require GC or complex runtime machinery.

### 12. Appendix: Recommended Design Patterns (Non-Normative)

Canonical idioms and copy-paste examples: [`.cursor/skills/writing-ion-code/references/verified-patterns.md`](.cursor/skills/writing-ion-code/references/verified-patterns.md). That file is the single source checked against `tests/` and `examples/`; update it when adding patterns rather than duplicating examples here.

| Topic | Where |
|-------|--------|
| Index/handle instead of returning `&T` | verified-patterns.md: Index and handle search; `stdlib/handle.ion`; `tests/test_handle_arena_*.ion`; `examples/handle_table/` |
| `Vec::get` / `Vec::set` put-back | verified-patterns.md: Mutating Vec elements; `tests/test_vec_get_putback.ion` |
| Owned API boundaries | verified-patterns.md: Owned API boundaries |
| Compare via `&Struct` fields | verified-patterns.md: Comparing borrowed structs |
| `spawn` and channel ownership | verified-patterns.md: Concurrency and ownership transfer |
| Compile-error anti-patterns | verified-patterns.md: Rejected patterns |

#### 12.1 Documentation Comments (Non-Normative)

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
    io::println("hi");
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

