### integer literals

`1u8` `1i8` `1u16` `1i16` `1u32` `1i32` `1u64` `1i64` | `1u128` `i128`

`1uz` (usize)

`1iz` (isize)

---

### builtin types

`uint8` `int8` `uint16` `int16` `uint32` `int32` `uint64` `int64` | `uint128` `int128`

`isize` `usize`

`float` `double`

`bool`

`nil`

~~`char`~~ ~~`uchar`~~

---

custom-width integer types

`bit{n}` -> `bit7` `bit24`, `bit16`

---

lex -> parse -> ast -> sema -> ir -> (optimizations) -> regalloc -> instruction encoding -> executable/object file

---

### asm blocks

intel syntax!

```
asm {
    ""
}
```

---

### keywords

`typeid`

`typeof` `sizeof` `alignof` `offsetof`

`unreachable`

`inline` `noinline`

`noreturn` `packed`

`likely` `unlikely`

`extern "C"`

`aligned(8)`

---

### notes

`#staticassert`

`#error`

`#warning`

`#target("arm64", "linux")`

`#os("windows")`

`#arch("x64")`

`#deprecated`

`#version`

`#doc`

---

### intrinsics

`@clz(x)` count leading zeros

`@ctz(x)` count trailing zeroes

| Role                   | Syntax    | Reason                        |
| ---------------------- | --------- | ----------------------------- |
| compile-time only      | `#name`   | directives, no runtime effect |
| semantic modifier      | keyword   | changes how code behaves      |
| builtin/intrinsic func | `@name()` | low-level compiler function   |
| ABI / linkage modifier | `extern`  | needs parser support          |

---

### casts

`cast`/`type`

`bitcast`

`floatcast`

`intcast`

`ptrcast`

| Cast      | Purpose                       | Notes                  |
| --------- | ----------------------------- | ---------------------- |
| `cast`    | Safe, context-based conversions           | Default option         |
| `floatcast` | Float ↔ int, truncating/overflowing | Only for numeric types |
| `intcast` | Numeric, lossy, signed ↔ unsigned | Only for numeric types |
| `bitcast` | Raw bit reinterpretation          | Must match size        |
| `ptrcast` | Pointer ↔ pointer or int            | Use with caution       |
