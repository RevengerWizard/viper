### todo

- [X] `union` handling

- [X] `import`, `from`, `as`

- [X] `asm` blocks

- [x] `def` declarations

- [X] fix `const` types

- [X] enums

- [X] incomplete struct/union declarations `struct tea_State;`

- [X] support output of .dot files

- [X] nil-able types `T*?`

- [X] strings emission

- [X] globals emission

- [X] function prototypes from dynamic (Win32) libraries

- [X] `for` loop statement

- [X] fix nested function calls in code generation

- [X] `switch` statement

- [X] only allow `bool` type evaluated expressions in conditions, i.e. `if x != nil` instead of `if x`

- [ ] explicit uninitialized variables with ` = undefined`
    - [ ] zero initialization by default

- [ ] intrinsics

- [ ] object files
    - [X] COFF
    - [ ] ELF

- [ ] fix casts, conversions

- [ ] linker?

- [ ] length from string literals/arrays

- [ ] macros?

- [ ] stronger types? `type` vs `alias`

- [ ] default constant values for `struct`/`union` fields?

- [ ] default constexpr `fn` parameters

- [ ] not-deferenceable `const T*`

- [ ] local `def`/`alias`/`type`/`struct`/`union`/`enum` declarations

- [ ] instruction selection: x64, ARM64, RV64
    - [ ] x64
    - [ ] ARM64
    - [ ] RV64

- [ ] multi-target architectures: x64, ARM64, RV64
    - [ ] x64
    - [ ] ARM64
    - [ ] RV64

- [ ] `packed` for `struct`/`union`

- [ ] `aligned(n)` | `aligned(T)`

- [ ] anonymous `struct`/`union`/`enum` types

- [ ] `std` modules
    - [ ] `std::io`
    - [ ] `std::mem`
    - [ ] `std::math`

- [ ] memory allocator!

- [ ] `switch type(x)`

- [ ] untyped `def` declarations should expand into context type, not default `int32`

- [ ] `enum E : uint8` must create separate type, not `alias`

- [ ] `sizeof(T)` vs `sizeof(expr)` ?
    
---

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

`bool`

`nil`

`float16` `float80` ?

`ptrdiff` `intptr` `uintptr` ?

~~`char`~~ ~~`uchar`~~ ?

---

custom-width integer types

`bit{n}` -> `bit7`, `bit24`, `bit16`

---

1. lex
2. parse -> AST
3. AST -> sema -> AST
4. AST -> codegen -> IR
5. IR -> opt -> IR
7. IR -> low -> IR
6. IR -> regalloc
7. IR -> emit -> ISA

---

1. lex
2. parse -> AST
3. AST -> sema -> AST
4. AST -> codegen -> IR
5. IR -> opt -> IR
6. IR -> low -> LIR
7. LIR -> sel -> LIR
8. LIR -> regalloc
9. LIR -> emit -> ISA

---

### cmd

`vxc <mode> [options] [file]`

- `check`
- `comp`
- `build`
- `link`
- `doc` ?

`-` - stdout

`-o <file>` - output
`-I <dir>` - import path
`-W<level>` - warnings
`-D <name[=val]>` - define

---

`--stats=mem`
`--stats=time`

```
== stats ==
lex:     120 KB
parse:   340 KB
sema:    1.2 MB
ir:      2.8 MB
ra:      900 KB
total:   5.4 MB
time:    42 ms
```

---

`--nostd` - exclude std library
`--version`
`--entry <sym>`
`--sanitize`
`--help`
`--stats=<kind>`
`--arch=<kind>`
`--target=<kind>` - target
`--dump=<kind>` - dump compiler internals

`--subsystem=<kind>` - Windows subsystem?
`--subsystem=console`

---

`--dump=ast`
`--dump=ir`
`--dump=cfg`
`--dump=dot`
`--dump=li`
`--dump=typecache`

---

`vxc check file.vp`
`vxc build file.vp`
`vxc link out.obj -o test`

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

- `io`
- `fmt`
- `str`
- `mem`
- `os`
- `conv` ?

---

### std::os

```vp
noreturn fn exit(x : uint32) : void;
fn getenv() : void;
```

---

### std::io

```vp
//io::stdout, io::stdin, io::stderr

fn open(path : const uint8*, flags : OpenFlags, err : IOError*) : FILE*;
fn close(FILE* f, ) : void;
fn write(f : FILE*, buf : const uint8*, n : usize) : usize;
fn read(f : FILE*, buf : uint8*, n : usize) : usize;
fn flush(f : FILE*) : void;
fn seek(f : FILE*) : void;
```

---

### std::str

- base `2`-`36` ?
- base `0` auto-detect base (`0x`, `0b`, `0o`)
- on conversion error, always return `0`

```vp
fn isalpha(c : uint8) : bool;
fn isdigit(c : uint8) : bool;
```

```vp
fn len(s : const uint8*) : usize;   // str::len

enum ConvError : uint32
{
    OK,
    EMPTY,
    INVALID,
    OVERFLOW,
    UNDERFLOW,
    INVALID_BASE
}

fn tou8(s : const uint8*, len : usize, base : Base, err : ConvError*?) : int8;   // str::tou8
fn toi8(s : const uint8*, len : usize, base : Base, err : ConvError*?) : uint8;   // str::toi8

fn tou16(s : const uint8*, len : usize, base : Base, err : ConvError*?) : int16;   // str::tou16
fn toi16(s : const uint8*, len : usize, base : Base, err : ConvError*?) : uint16;   // str::toi16

fn tou32(s : const uint8*, len : usize, base : Base, err : ConvError*?) : uint32;   // str::tou32
fn toi32(s : const uint8*, len : usize, base : Base, err : ConvError*?) : int32;  // str::toi32

fn tou64(s : const uint8*, len : usize, base : Base, err : ConvError*?) : uint64;   // str::tou64
fn toi64(s : const uint8*, len : usize, base : Base, err : ConvError*?) : int64;   // str::toi64

fn tof32(s : const uint8*, len : usize, err : ConvError*?) : float32;   // str::tof32
fn tof64(s : const uint8*, len : usize, err : ConvError*?) : float64;   // str::tof64

fn tobool(s : const uint8*, len : usize, err : ConvError*?) : bool; // str::tobool
```

---

### std::fmt

---

### std::mem

```vp
fn copy(dst : void*, src : const void*, n : usize) : void;
fn move(dst : void*, src : const void*, n : usize) : void;
fn set(dst : void*, val : uint8, n : usize) : void;
fn cmp(a : const void*, b : const void*, n : usize) : int32;

fn alloc(n : usize) : void*;
fn realloc(p : void*, n : usize) : void*;
fn free(p : void*) : void;
```

---

### std::math

```vp
// math::abs
fn absi64(x : int64) : int64;
fn absf64(x : float64) : float64;

// math::sin
fn sinf32(x : float32) : float32;
fn sinf64(x : float64) : float64;

// math::cos
fn cosf32(x : float32) : float32;
fn cosf64(x : float64) : float64;

// math::sqrt
fn sqrtf32(x : float32) : float32;
fn sqrtf64(x : float64) : float64;

// math::min(a : T, b : T) : T
// math::max(a : T, b : T) : T

//math::pi : float64;
//math::e : float64;
```

---

### fmt

```
%[flags][width][.precision]type
```

- `%` start specifier
- `%%` -> `%`

- integers
`%u8` `%i8` 
`%u16` `%i16`
`%u32` `%i32` 
`%u64` `%i64`
`%uz` `%iz` -> `usize`/`isize`
- floats
`%f32` `%f64`
`%e32` `%e64` scientific
- bool
`%b` -> `true`/`false`
`%B` -> `1`/`0`
- strings
`%s` -> `const uint8*` (nil-terminated)
`%S` -> `(const uint8*, usize)`

`%s` with `nil` -> `(nil)`
`%s` with `(nil, n)` -> `(nil)`
- pointers
`%p` -> hex, `0x` prefixed, pointer-sized
- others
`%c` single byte `uint8`, ASCII

- `%i32` -> `-42`
- `8%i32`

Flags

Single-char, order-independent, optional
- `-` left align
- `0` zero pad
- `#` alternate form
- `+` always show sign

Width

Number digit
- `%8i32`
- `-16s`
- `%08u32`

Base

- `#x` hex (lower)
- `#H` hex (upper)
- `#b` binary

- `%#xu32` -> `0xDEADBEEF`
- `%#bu8` -> `0b101010`

Precision

- `%.4i32` -> `0042`
- `%.2f32` -> `3.14`
- `%.5s` -> at most 5 bytes

---

### keywords

`import`

`pub`

`typeof` `sizeof` `alignof` `offsetof`

`inline`

`noreturn` `packed`

`alias`

`export` `extern`

---

`lenof`

`unreachable`

`likely` `unlikely`

`aligned(8)`

`extern "C"`

---

`undefined` ?

`goto` ?

`use` ?

`typeid` ?

`defer` ?

---

### import

```vp
import mod
from mod import *
from mod import foo, bar, baz
from mod import foo
from mod import foo as f, bar as b, baz as z

mod::thing

import std::math
from std::math import *
from std::math import pi
from std::math import sinf32 as sin

std::math::pi
```

```vp
import windows as win
```

---

### macros

- single time evaluation of arguments
- macro declarations cannot shadow other declaration kinds
- macro name cannot shadow other already declared identifiers
- macro expressions
- macro blocks/statements

```
struct VecHeader
{
    len : uint32;
    size : uint32;
    data : uint8[];
}

macro len(v) = hdr(v).len;
macro size(v) = hdr(v).size;
macro end(v) = v + len(v);
macro hdr(v) = cast(VecHeader*, cast(uint8*, v - offset(VecHeader, data)));
```

```
macro assert(cond, msg) = cond ? void(0) : assertx(msg);
```

```
macro min(a, b) = b < a ? b : a;
macro max(a, b) = b > a ? b : a;
macro clamp(x, a, b) = max(a, min(x, b));
macro lerp(bits, a, b, p) = a + ((b - a) * p) >> bits;
```

---

### notes

`#if`

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

`[[used]]`

`[[alias("other")]]`

`[[naked]]`

`[[syscall]]`

`[[interrupt]]`

`[[start]]` `[[entry]]`

`[[format]]`

---

| Syntax     | Role                |
| ---------- | ------------------- |
| `#name`    | compile-time only   |
| `keyword`  | semantic modifier   |
| `@name()`  | builtin/intrinsic   |
| `[[name]]` | attributes          |

---

### casts

`cast`/`type`

`bitcast`

`floatcast`

`intcast`

`ptrcast`

~~`constcast`~~ ?

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

| Cast Type  | Target Types          | Source Types           | Safety Level                                     |
|----------- | --------------------- | ---------------------- | ------------------------------------------------ |
| `cast`     | Numeric, bool, typed* | Numeric, bool, void*   | **Safe** - No data loss or well-defined behavior |
| `intcast`  | Integer only          | Numeric                | **Unsafe** - Potential data loss, wrap-around    |
| `floatcast`| Float only            | Numeric                | **Unsafe** - Truncation, precision loss          |
| `bitcast`  | Same-size type        | Same size type         | **Unsafe** - Raw reinterpretation                |
| `ptrcast`  | Pointer, integer      | Pointer, integer       | **Unsafe** - Memory safety concerns              |

---

### ir

---

### lir

- lower level IR
- instruction selection

---

### abi

### x64

`rax`, `rcx`, `rdx`, `rbx`, `rsp`, `rbp`, `rdi`, `rsi`, `r8-r15`

`xmm0-xmm15`

max physical registers : 16

- special purpose
`rsp` (stack pointer)
`rbp` (base pointer)

---

Windows x64

- caller-saved (volatile), can be freely used in expressions
`rax`, `rcx`, `rdx`, `r8`, `r9`, `r10`, `r11`
`xmm0-xmm3` (volatile)

- callee-saved (non-volatile), must preserve across function calls
`rbx`, `rsi`, `rdi`, `r12`, `r13`, `r14`, `r15`
`xmm6-xmm9` (non-volatile)

- params
`rcx`, `rdx`, `r8`, `r9` (first 4 int)
`xmm0-xmm3` (first 4 floats)

---

Linux System-V x64

- caller-saved
`rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`, `r9`, `r10`, `r11`
`xmm0-xmm15`

- callee-saved
`rbx`, `rbp`, `r12`, `r13`, `r14`, `r15`
`xmm8-xmm15`

- params
`rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`
`xmm0-xmm7`

---

### syscall

`rax` = syscall number
`rdi` = arg0
`rsi` = arg1
`rdx` = arg2
`r10` = arg3
`r8` = arg4
`r9` = arg5

`rax` = return

- clobbers
`rcx`, `r11`

---

`[[syscall]]`

- x64: `syscall`, rax/rdi/rsi/rdx/r10/r8/r9, rcx/r11 clobbered
- arm64: `svc #0`, x8/x0-x5
* macos: `svc #0`, x16/x0-x5
- rv64: `ecall`, a7/a0-a5

- max 6 arguments
- all arguments must be ints/pointers
- return type must be int/pointer

- `[[syscall]]` implicitly `inline` and `naked`
- cannot take address
- cannot be recursive
- cannot be exported (dynamic link)
- must end in `@syscall` or equivalent `asm`

```
[[syscall]]
fn syswrite(fd : uint32, buf : const uint8*, count : usize) : isize
{
    @syscall(1);
}
```

```
[[syscall]]
fn syswrite(fd : uint32, buf : const uint8*, count : usize) : isize
{
    asm
    {
        mov eax, 1;
        syscall;
    }
}
```

```
[[syscall]]
noreturn fn sysexit(status : int32) : void
{
    asm
    {
        mov x8, 0x5d;
        svc 0;
    }
}
```

```
[[syscall]]
noreturn fn sysexit(status : int32) : void
{
    @svc(0x5d);
}
```

---

`packed struct` | `packed union`

- eliminate padding
- reduces alignment to 1

---

`aligned(n)` | `aligned(T)`

- `struct`, `union` types
- fields?
- variables?
- `aligned(0)` not allowed
- power of 2

`aligned(16) struct Vec { ... }`

```
struct S
{
    aligned(16) x : float64;
}
```

`aligned(64) var buf : uint8[1024]`;

---

`var x : int64 = undefined;`
`var x : int64;   // = 0`

---

### linker

---

### wat

```c
fn example<T>(a : T, b : T) : T
{
    // get type info with some operator, I'll use % as an example
    const T_info: type = %T;
    if !T_info.is_numerical { #error("raagh"); }

    switch T_info.id
    case %int32.id { return a + b; }
    case %float.id: { return a / b; }
    else: { return 0; }
}
```

---

`int32::max`
`int64::min`
`float64::max`
`bool::align`
`bool::size`

```
#alloc
fn foo() {  }
```

```
for var i : int32 = 0; i < 10; i++ {  }
```

```
for i : int32 = 0; i < 10; i++ {  }
```

```
for i := 0u32; i < 10; i++ {  }
```

```
switch x
case 10 {  }
case 11 {  }
default {  }

var c = switch x 
case 10 => 'A';
case 12 => 'C';
default => '\0';
```

```
struct Data
{
    kind : enum : uint8 {
        EMPTY,
        HALF,
        FULL
    };
    union
    {
        
    }
}
```

```vp
type X = struct;

type U = union;

type S = struct { ... };

type R = enum { ... };

var x : struct { id : int32; name : const uint8*; } = {12, "tony"};

var y : struct { int32; bool; } = {13, true};

var e : enum : int32 { STOP, RUN, END } = e::STOP;
```

```vp
union StatusReg
{
    raw : uint8;
    bits : packed struct
    {
        idle : bit1;
        low_brightness : bit1;
        normal_brightness : bit1;
        high_brightness : bit1;
        party_mode : bit1;
        debug_mode : bit1;
        reserved : bit1;
        factory_test : bit1;
    }
}
```

- struct inheritance?

```vp
struct Pet
{
    name : const uint8*;
    weight : float32;
}

struct Cat
{
    a : int32;
    b : float32;
    use pet : Pet;
}

let cat : Cat;
cat.name = "Mittens";
```