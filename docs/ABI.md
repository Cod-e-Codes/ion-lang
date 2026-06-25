# Ion runtime ABI notes

This document describes the runtime data shapes that Ion-generated C and the
standard library agree on. It is intentionally a beta-facing contract, not a
promise that every internal helper symbol is stable before 1.0.

## General rules

- Generated C is the compiler/runtime ABI boundary.
- Ion values are owned by exactly one binding unless borrowed through `&T` or
  `&mut T`.
- Owned values with runtime resources are dropped exactly once by generated
  scope-exit code.
- References are stack-only views and must not be stored in runtime containers
  or sent across threads.

## `String`

`String` is an owned runtime allocation for byte strings. Its C layout uses a
`uint8_t*` data pointer plus length and capacity fields. String iteration in the
beta subset is byte iteration (`u8`), not Unicode scalar-value or grapheme
iteration.

Stable beta expectations:

- `String::new`, `String::from`, `String::len`, `String::push_str`, and
  `String::push_byte` preserve ownership of the `String` receiver unless a
  function explicitly consumes it.
- Dropping a `String` releases its backing allocation once.
- `String == String` and `String != String` compare byte contents.

## `Vec<T>`

`Vec<T>` is an owned dynamic array with pointer, length, and capacity. The C
backend monomorphizes element-specific helpers where needed.

Stable beta expectations:

- `Vec::new`, `Vec::push`, `Vec::pop`, `Vec::get`, `Vec::set`, `Vec::len`, and
  `Vec::capacity` remain available through the stdlib/builtin surface.
- Dropping `Vec<T>` drops owned elements when `T` needs destruction.
- Bounds-sensitive operations either return `Option<T>` where documented or
  trigger the runtime panic path for checked indexing.

## `Box<T>`

`Box<T>` owns one heap-allocated `T`.

Stable beta expectations:

- `Box::new` allocates and owns a value.
- `Box::unwrap` consumes the box and returns the owned payload.
- Dropping a `Box<T>` drops the payload and releases the allocation once.

## Arrays and slices

Fixed arrays `[T; N]` are inline values. Slices `[]T` are fat views carrying a
data pointer and length. Safe indexing emits runtime bounds checks; indexing
inside `unsafe` blocks may omit those checks.

## Enums and structs

Struct layout is C-oriented and field-order preserving in generated C. Enum
layout is compiler-generated and should be treated as stable only for C emitted
by the same Ion compiler/runtime version unless a beta release explicitly
documents a layout guarantee.

## Channels and `spawn`

Channels are typed ownership-transfer queues backed by the C runtime. Sending a
value moves ownership into the channel; receiving moves ownership out. `spawn`
creates an OS thread and transfers only structurally `Send` values into the
thread context.

Stable beta expectations:

- References do not cross channel or thread boundaries.
- Channel handles are runtime resources and are released by generated drops.
- Runtime failures such as allocation or thread creation failures call the Ion
  panic path.

## Drops and panics

Generated code is responsible for normal-scope destruction. Runtime panics abort
the process; they are not currently exception-style unwinds. Code that relies on
drop execution after a runtime panic is outside the beta contract.
