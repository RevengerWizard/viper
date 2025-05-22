### integer literals

`u8` `i8` `u16` `i16` `u32` `i32` `u64` `i64` | `u128` `i128`

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

typeid

typeof sizeof alignof offsetof

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

| Cast      | Safety     | Purpose                       | Notes                  |
| --------- | ---------- | ----------------------------- | ---------------------- |
| `cast`    | Safe       | Checked conversions           | Default option         |
| `intcast` | Unsafe-ish | Signed/unsigned or lossy cast | Only for numeric types |
| `bitcast` | Unsafe     | Bit reinterpretation          | Must match size        |
| `ptrcast` | Unsafe     | Pointer-type casts            | Use with caution       |
