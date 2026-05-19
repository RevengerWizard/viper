# Viper Standard Library

---

## std

- `io`
- `fmt`
- `str`
- `mem`
- `math`
- `os`
- `conv`

---

## std::mem

```vp
pub fn calloc(n : usize) : void*?;
pub fn alloc(n : usize) : void*?;
pub fn realloc(p : void*, n : usize) : void*?;
pub fn free(p : void*) : void;

pub fn copy(dst : void*, src : const void*, n : usize) : void;  // memcpy
pub fn move(dst : void*, src : const void*, n : usize) : void;  // memmove
pub fn set(dst : void*, val : uint8, n : usize) : void; // memset
pub fn cmp(a : const void*, b : const void*, n : usize) : isize;    // memcmp
pub fn zero(dst : void*, n : usize) : void; // memset(_, 0, _)
```

---

## std::str

```vp
pub fn len(s : const uint8*) : usize;   // strlen

pub fn cmp(a : const uint8*, alen : usize, b : const uint8*, blen : usize) : int32;
pub fn eq(a : const uint8*, alen : usize, b : const uint8*, blen : usize) : bool;

pub fn find(hay : const uint8*, hlen : usize, needle : const uint8*, nlen : usize) : usize; // returns hlen if not found 
pub fn rfind(hay : const uint8*, hlen : usize, needle : const uint8*, nlen : usize) : usize; pub fn findc(s : const uint8*, slen : usize, c : uint8) : usize; // returns slen if not found 
pub fn rfindc(s : const uint8*, slen : usize, c : uint8) : usize; 

pub fn startswith(s : const uint8*, slen : usize, prefix : const uint8*, plen : usize) : bool; 
pub fn endswith(s : const uint8*, slen : usize, suffix : const uint8*, sfxlen : usize) : bool;

pub fn isalpha(c : uint8) : bool;
pub fn isdigit(c : uint8) : bool;
pub fn isalnum(c : uint8) : bool;
pub fn isspace(c : uint8) : bool;
pub fn isupper(c : uint8) : bool;
pub fn islower(c : uint8) : bool;
pub fn toupper(c : uint8) : bool;
pub fn tolower(c : uint8) : bool;
```

`find`
Searches for *first occurrence* of `needle` inside `hay`.
- Returns the *index* where it starts
- Returns `hlen` if not found

```
"hello world", "world" -> 6
"hello", "x" → 5  (not found → hlen)
```

`rfind`
Searches backwards for *first occurrence* of `needle` inside `hay`

```
"ababa", "ba" -> 3
```

---

## std::conv

```vp
pub enum ConvError : uint32
{
    OK,
    INVALID,    // bad characters
    OVERFLOW,   // out of range for target type
    UNDERFLOW,
    EMPTY       // zero-length input 
}

// int -> str
pub fn i8tos(buf : uint8*, len : usize, num : int8) : usize;
pub fn u8tos(buf : uint8*, len : usize, num : uint8) : usize;
pub fn i16tos(buf : uint8*, len : usize, num : int16) : usize;
pub fn u16tos(buf : uint8*, len : usize, num : uint16) : usize;
pub fn i32tos(buf : uint8*, len : usize, num : int32) : usize;
pub fn u32tos(buf : uint8*, len : usize, num : uint32) : usize;
pub fn i64tos(buf : uint8*, len : usize, num : int64) : usize;
pub fn u64tos(buf : uint8*, len : usize, num : uint64) : usize;

// float -> str
pub fn f32tos(buf : uint8*, len : usize, num : float32) : usize;
pub fn f64tos(buf : uint8*, len : usize, num : float64) : usize;

// bool -> str
pub fn booltos(buf : uint8*, len : usize, val : bool) : usize;

// str -> int
pub fn stoi8(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : int8;
pub fn stou8(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : uint8;
pub fn stoi16(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : int16;
pub fn stou16(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : uint16;
pub fn stoi32(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : int32;
pub fn stou32(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : uint32;
pub fn stoi64(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : int64;
pub fn stou64(s : const uint8*, slen : usize, base : uint8, err : ConvError*?) : uint64;
 
// str -> float
pub fn stof32(s : const uint8*, slen : usize, err : ConvError*?) : float32;
pub fn stof64(s : const uint8*, slen : usize, err : ConvError*?) : float64;
 
// str -> bool
pub fn stobool(s : const uint8*, slen : usize, err : ConvError*?) : bool;
```

---

## std::fmt

Format spec
```
%[flags][width][.prec]type

flags: - (left align)  0 (zero pad)  # (alternate)  + (show sign)
width: decimal integer
prec: .decimal integer
type: u8 i8 u16 i16 u32 i32 u64 i64 uz iz
      f32 f64 e32 e64
      b (bool : true/false) B (bool: 1/0)
      s (const uint8*, nil-terminated)
      S (const uint8*, usize)
      p (pointer, hex, 0x-prefix)
      c (uint8, ASCII)
      % (literal %)
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

```vp
pub fn print(fmt : const uint8*, args : ...) : void;

pub fn fprint(f : FILE*, fmt : const uint8*, args : ...) : void;

pub fn sprint(buf : uint8*?, n : usize, fmt : const uint8*, args : ...) : usize;
```

---

## std::io

```vp
pub enum IOError : uint32
{
    OK,
    EOF,
    NOT_FOUND,
    ACCESS_DENIED,
    INVALID,
    UNKNOWN
}

pub enum OpenMode : uint32
{
    READ = 1 << 0,
    WRITE = 1 << 1,
    APPEND = 1 << 2,
    CREATE = 1 << 3,
    TRUNC = 1 << 4,
    BINARY = 1 << 5
}

pub enum SeekPos : uint32
{
    SET, CUR, END
}

pub struct FILE;

pub let stdin : FILE*;
pub let stdout : FILE*;
pub let stderr : FILE*;

pub fn open(path : const uint8*, mode : OpenMode, res : IOError*?) : FILE*?;
pub fn close(f : FILE*, res : IOError*?) : void;
pub fn write(f : FILE*, buf : const uint8*, n : usize, res : IOError*?) : usize;

pub fn read(f : FILE*, buf : uint8*, n : usize, res : IOError*?) : usize;
pub fn readln(f : FILE*, buf : uint8*, n : usize) : usize;

pub fn flush(f : FILE*, res : IOError*?) : void;
pub fn seek(f : FILE*, ofs : isize, pos : SeekPos, res : IOError*?) : usize;
pub fn tell(f : FILE*, res : IOError*?) : usize;

pub fn eof(f : FILE*) : bool;

pub fn putc(c : uint8) : void;
```

---

## std::os

```vp
pub noreturn fn exit(code : uint32) : void;
```

## std::math

```vp
pub def pi;
pub def e;
pub def tau;

pub fn absi32(x : int32) : int32;
pub fn absi64(x : int64) : int64;
pub fn absf32(x : float32) : float32;
pub fn absf64(x : float64) : float64;

pub fn sinf32(x : float32) : float32;
pub fn sinf64(x : float64) : float64;
pub fn cosf32(x : float32) : float32;
pub fn cosf64(x : float64) : float64;
pub fn tanf32(x : float32) : float32;
pub fn tanf64(x : float64) : float64;

pub fn sqrtf32(x : float32) : float32;
pub fn sqrtf64(x : float64) : float64;

pub fn powf32(base : float32, exp : float32) : float32;
pub fn powf64(base : float64, exp : float32) : float64;

pub fn floorf32(x : float32) : float32;
pub fn floorf64(x : float64) : float64;
pub fn ceilf32(x : float32) : float32;
pub fn ceilf64(x : float64) : float64;
pub fn roundf32(x : float32) : float32;
pub fn roundf64(x : float64) : float64;
```

---

## std::debug

```vp
pub fn assert(cond : bool, msg : const uint8*?) : void;
```
