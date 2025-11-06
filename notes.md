### todo

- `import`

- intrinsics

- `asm` blocks

- `def` declarations

- `const` variable declarations

- fix casts, conversions

- fix `const` types

- object files

- linker?

- length from string literals/arrays

- function prototypes from dynamic (Win32) libraries

- globals emission

- strings emission

- macros?

- stronger types?

- enums

### integer literals

`1u8` `1i8` `1u16` `1i16` `1u32` `1i32` `1u64` `1i64` | `1u128` `1i128`

`1f32` `1f64`

`1uz` (usize)

`1iz` (isize)

---

### builtin types

`uint8` `int8` `uint16` `int16` `uint32` `int32` `uint64` `int64` | `uint128` `int128`

`isize` `usize`

`float32` `float64`

`float16` `float80` ?

`ptrdiff` `intptr` `uintptr` ?

`bool`

`nil`

~~`char`~~ ~~`uchar`~~

---

custom-width integer types

`bit{n}` -> `bit7` `bit24`, `bit16`

---

### cmd

`vxc [mode] [options] [file]`

- `build`

- `link`

---

`vxc [options] [file]`

`--nostd`

`--version` `--help`

`--emit <kind>` -  `obj | asm | ir | exe | bin`

`--target <kind>`

---

`VIPER_PATH`
`VIPER_LIB`
`VIPER_TARGET`

---

### asm blocks

intel syntax!

`asm` functions?

`inline` `asm` functions?

```
fn asm rdtsc() : int64
{
    rdtsc;
    shl rdx, 32;
    or rax, rdx;
}
```

```
asm
{
    mov eax, a;
    imul eax, b;
}
```

---

### std

`io::write`

`io::read`

`io::fmt`

`io::print`

`io::println`

`str::len`

`mem::copy`

`mem::set`

`mem::alloc`

`math::sin`

`math::cos`

`math::pi`

---

### keywords

`import`

`pub`

`typeid`

`typeof` `sizeof` `alignof` `offsetof`

`lenof`

`unreachable`

`inline` `noinline`

`noreturn` `packed`

`likely` `unlikely`

`extern "C"`

`aligned(8)`

`alias` ?

---

### notes

`#fall`

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

`@clz(x)` --- count leading zeros

`@ctz(x)` --- count trailing zeroes

`@memcpy`

`@syscall`

---

### attributes

`[[lib("kernel32")]]`

`[[export]]`

`[[used]]`

`[[alias("other")]]`

`[[naked]]`

`[[syscall]]`

`[[interrupt]]`

`[[entry]]`

---

| Syntax     | Role                |
| ---------- | ------------------- |
| `#name`    | compile-time only   |
| `keyword`  | semantic modifier   |
| `@name()`  | builtin/intrinsic   |
| `[[name]]` | function attributes |

---

### casts

`cast`/`type`

`bitcast`

`floatcast`

`intcast`

`ptrcast`

`constcast` ?

| Cast        | Purpose                             | Notes                  |
| ----------- | ----------------------------------- | ---------------------- |
| `cast`      | Safe, context-based conversions     | Default option         |
| `floatcast` | Float ↔ int, truncating/overflowing | Only for numeric types |
| `intcast`   | Numeric, lossy, signed ↔ unsigned   | Only for numeric types |
| `bitcast`   | Raw bit reinterpretation            | Must match size        |
| `ptrcast`   | Pointer ↔ pointer or int            | Use with caution       |

float32 <- int16, uint16, int8, uint8, bool
float64 <- int32, uint32, int16, uint16, int8, uint8, bool

uint8 <- bool
uint16 <- uint8 <- bool
uint32 <- uint16 <- uint8 <- bool
uint64 <- uint32 <- uint16 <- uint8 <- bool

int8 <- bool
int16 <- int8 | uint8 | bool
int32 <- int16 | uint16 | int8 | uint8 | bool
int64 <- int32 | uint32 | int16 | uint16 | int8 | uint8 | bool

| Cast Type   | Source Types           | Target Types          | Safety Level                                     |
|-----------  | ---------------------- | --------------------- | ------------------------------------------------ |
| `cast`      | Numeric, bool, void*   | Numeric, bool, typed* | **Safe** - No data loss or well-defined behavior |
| `intcast`   | Numeric                | Integer only          | **Unsafe** - Potential data loss, wrap-around    |
| `floatcast` | Numeric                | Float only            | **Unsafe** - Truncation, precision loss          |
| `bitcast`   | Any                    | Same-size type        | **Unsafe** - Raw reinterpretation                |
| `ptrcast`   | Pointer, integer       | Pointer, integer      | **Unsafe** - Memory safety concerns              |

### ir



### abi

### x64

[rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11, r12, r13, r14, r15]

max physical registers : 16

Windows x64

- caller-saved (volatile), can be freely used in expressions
rax, rcx, rdx, r8, r9, r10, r11

- callee-saved (non-volatile), must preserve across function calls
rbx, rsi, rdi, r12, r13, r14, r15

- special purpose
rsp (stack pointer)
rbp (base pointer)

- floating point registers
xmm0, xmm1, xmm2, xmm3 (volatile)
xmm6, xmm7, xmm8, xmm9 (non-volatile)

- params
rcx, rdx, r8, r9 (first 4 int)
xmm0, xmm1, xmm2, xmm3 (first 4 floats)

---

Linux System-V x64

- caller-saved
rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11

- callee-saved
rbx, rbp, r12, r13, r14, r15

# win32 API

```c
var h : HANDLE = GetStdHandle(STD_OUTPUT_HANDLE);
var written : DWORD;
WriteConsoleA(h, "Hello\n", 6, &written, nil);
```