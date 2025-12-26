# Cast rules

## Core principles

1. Type safety: prevent accidental data loss or misinterpretation

2. Context propagation: declaration type propagates to all literals within the initialization expression

3. Explicit conversion: require explicit casts for potentially unsafe conversions between variables

4. Consistency: provide predictable type conversion behavior

## Formal type system rules

### 1. Type categories

- boolean types: `bool`
- integer types: `int8`, `uint8`, `int16`, `uint16`, `int32`, `uint32`, `int64`, `uint64`
- platform integer types: `usize`, `isize`
- floating types: `float32`, `float64`
- numeric types: integer and floating types
- pointer types: any type of the form `T*`
- array types: any type of the form `T[N]` or `T[]`
- struct types: any user-defined `struct`
- union types: any user-defined `union`
- function types: any function pointer type
- scalar types: boolean, integer, floating, and pointer types
- incomplete types: struct or union types that have been declared but not defined

### 2. Type definition

Rule D1: `alias X = T`
- creates a transparent name for type `T`
- `X` and `T` are interchangeable in all contexts
- no cast is required between `X` and `T`

Rule D2: `type X = T`
- creates a distinct new type `X` based on the underlying representation of `T`
- `X` and `T` are not compatible
- an explicit `cast` is required to convert between `X` and `T`

### 3. Type qualifiers

Rule Q1: `const`
- a type qualifier that can be applied to any type `T`
- is idempotent: applying `const` to an already `const` type yields no changes
- propagates through pointer dereference: `const T*` points to constant data (dereferencing yields `const T`)

### 4. Type operators

Rule K1: `typeof(expr)`
- returns the type of the expression `expr`
- evaluation of `expr` occurs only at compile-time for type resolution

Rule K2: `sizeof(T)`
- returns the size of type `T` in bytes
- result type is `usize`

Rule K3: `alignof(T)`
- returns the alignment requirement of type `T` in bytes
- result type is `usize`

Rule K4: `offsetof(T, member)`
- returns the offset in bytes of member within struct `T`
- result type is `usize`

### 5. Literal type assignment

Rule L1: context-aware integer literals:
- in a declaration context `var x : T = expr`, all integer literals in expr adopt type `T`
- if no context is available, literals default to `int32`

Rule L2: context-aware floating literals:
- in a declaration context `var x : T = expr`, all floating literals in expr adopt type `T`
- if no context is available, literals default to `float`

Rule L3: context-aware string literals:
- in a declaration context `var x : T = expr` where `T` is a pointer type, string literals adopt type `const uint8*`
- if no context is available, literals default to `uint8[]`

Rule L4: character literals have type `uint8`

Rule L5: `nil` literal:
- `nil` has no intrinsic type
- `nil` is implicitly convertible to any pointer type, including `void*`

### 6. Literal type overflow

Rule LO1: integer overflow checking
- an error is raised if an integer literal exceeds the range of its context type or its explicitly suffixed type
- valid suffixes: `u8` `i8` `u16` `i16` `u32` `i32` `u64` `i64` `uz` (`usize`) `iz` (`isize`)

Rule LO2: floating literal overflow checking
- an error is raised if a floating literal exceeds the range of the context type
- valid suffixes: `f32` `f64`

Rule LO3: suffixed literals in context
- if a literal has an explicit suffix, it ignores the declaration context type

### 7. Aggregate and Array Initialization

Rule G1: aggregate context propagation

- in a declaration `var x : T = { ... }`, the type `T` propagates to all members of the initializer list
- for array types `U[N]`, every element in the initializer list adopts type `U`
- for `struct`/`union` types, each field in the initializer list adopts the type defined in the struct/union schema
- context propagation is recursive: it applies to nested arrays and nested aggregate initializations

### 8. Incomplete Type Handling

Rule I1: valid usage of incomplete types
- pointers to incomplete types (`T*` where `T` is incomplete) are valid types and can be declared, passed to functions, or returned

Rule I2: restricted usage of incomplete types
- dereferencing a pointer to an incomplete type is forbidden.
- incomplete types cannot be used as operands in any operation (arithmetic, comparison, logical, etc.)
- incomplete types cannot be used in `sizeof` or `alignof` operations
- variables cannot be declared with an incomplete type (except as pointers)

### 9. Expression type resolution in declaration context

Rule E1-C (binary numeric operators in declaration context): for operations `+` `-` `*` `/` `%` `&` `|` `^` `<<` `>>`
- when evaluating `var x : T = expr` where `T` is numeric:
    * all literals within expr are typed as `T`
    * the result type of the expression is `T`

Rule E2-C (comparison operators in declaration context): for operations `==` `!=` `<` `>` `<=` `>=`
- declaration context does not affect comparison result type (always `bool`)
- however, literals compared within the context adopt the declaration type

Rule E3-C (logical operators in declaration context): for operations `and` `or`
- both operands must have type `bool`
- result has type `bool`

Rule E4-C (unary operators in declaration context): for operations `-` `~`
- in declaration context, the result adopts the declaration type

### 10. Expression type resolution outside declaration context

Rule E1 (binary numeric operators): for operations `+` `-` `*` `/` `%` `&` `|` `^` `<<` `>>`
- for `+`:
    * for numeric types: both operands must have the same type after applying implicit conversion rules
    * for pointer types: allowed as `ptr + int` or `int + ptr`, resulting in a pointer of the same type as the original pointer
    * arithmetic on `void*` is forbidden
    * arithmetic on `nil` is forbidden
    * arithmetic on function types is forbidden

- for `-`:
    * for numeric types: both operands must have the same type after applying implicit conversion rules
    * for pointer types: allowed as `ptr - int` resulting in a pointer of the same type as the original pointer
    * for pointer types: allowed as `ptr - ptr` (same pointer type) resulting in an `int64` representing the difference in elements
    * arithmetic on `void*` is forbidden
    * arithmetic on `nil` is forbidden
    * arithmetic on function types is forbidden

- for `*` `/` `%`
    * only valid for numeric types, both operands must have the same type after applying implicit conversion rules

- for `&` `|` `^`
    * only valid for integer types, both operands must have the same type after applying implicit conversion rules

- for `<<` `>>`
    * only valid for integer types, both operands must have the same type after applying implicit conversion rules

- the result type follows the types specified above

Rule E2 (comparison operators): for operations `==` `!=` `<` `>` `<=` `>=`
- for `==` `!=`
    * both operands must have the same type after applying implicit conversion rules
    * one operand may be a pointer and the other may be `nil`

- for `<` `>` `<=` `>=`
    * both operands must have the same type after applying implicit conversion rules, or
    * both operands must be pointers of the same type

- the result has type `bool`

Rule E3 (logical operators): for operations `and` `or`
- both operands must be scalar types
- if operands are not `bool`, they are first converted to `bool` (non-zero is true, zero is false)
- the result has type `bool`

Rule E4 (unary operators): for operations `-` `~`
- for `-`
    * the operand must be a numeric type

- for `~`
    * the operand must be an integer type

- the result has the same type as the operand

Rule E5 (unary operator): for operations `!` `not`
- the operand must be a scalar type
- if the operand is not `bool`, it is first converted to `bool` (non-zero is true, zero is false)
- The result has type `bool`

Rule E6 (address-of operator): for operation `&`
- applies to any lvalue expression
- cannot take address of `nil`
- result is a pointer to the operand's type

Rule E7 (dereference operator): for operation `*`
- the operand must be a pointer type
- The operand must not be `void*` or `const void*`
- result is the type pointed to by the operand

### 11. Implicit conversion rules

Rule C1: a value of type `T` can be implicitly converted to type `T` (identity conversion)

Rule C2: integer widening with same signedness is allowed:
- `int8` -> `int16` -> `int32` -> `int64`
- `uint8` -> `uint16` -> `uint32` -> `uint64`

Rule C3: no implicit conversions between signed and unsigned integers

Rule C4: no implicit conversions from floating-point to integer types

Rule C5: integer to floating-point conversions are allowed only when:
- the integer type is small enough that all values can be exactly represented in the float type
- `int8`, `int16`, `uint8`, `uint16` -> `float32` or `float64`
- `int32`, `uint32` -> `float64` only

Rule C6: no implicit conversion between floating-point types (`float` <-> `double`)

Rule C7: no implicit conversions between pointer types except:
- `T*` -> `void*` (widening to generic pointer)
- `T*` -> `const T*` (widening to const-qualified pointer)
- `nil` -> `T*` (nil pointer conversion)
- `nil` -> `const T*`
- `nil` -> `void*`
- `nil` -> `const void*`
- `void*` -> `T*` requires explicit cast

Rule C8: no implicit conversions between array types

Rule C9: no implicit conversion from `const T*` to `T*` for any type category
- implicit stripping of `const` qualifier is forbidden
- explicit cast is required to remove `const`

Rule C10: implicit conversion from `T*` to `const T*` is allowed
- Adding `const` qualifier is allowed for any type

Rule C11: array decay in function argument context:
- `T[N]` decays to `T*` when passed as function argument
- `T[]` decays to `T*` when passed as function argument
- decay does not occur in other contexts (assignment, initialization, return)
- const qualification is preserved during decay: `const T[N]` -> `const T*`

Rule C12: no implicit conversions between `struct`/`union` types
- even if the internal layout is identical, struct/union types are distinct

### 12. Cast type hierarchy

Viper provides four levels of type conversions:

#### Level 1: implicit conversions (automatic)

- no cast required
- governed by implicit conversion rules C1-C8
- only fully safe conversions

#### Level 2: explicit conversions

Syntax: `cast(T, expr)` or `T(expr)`

Rule S1: `cast` performs well-defined conversions that are not safe enough to be implicit

Rule S2: `cast` is valid for:
- all implicit rules described in Section 11
- `void*` to any pointer type
- `bool` to numeric types (false = 0, true = 1)
- numeric types to `bool` (0 = false, non-zero = true)
- converting between a `type X = T` definition and its underlying type `T`

Rule S3: `cast` preserves value semantics where possible, with predictable behavior for edge cases

#### Level 3: specialized conversions

Syntax: `intcast(T, expr)`, `floatcast(T, expr)`, `bitcast(T, expr)`, `ptrcast(T, expr)`

Rule U1: `intcast` - for lossy integer conversions
- converts between any integer types
- handles signed ↔ unsigned conversions with wrap-around semantics
- target type must be an integer type
- source type must be an integer type

Rule U2: `floatcast` - for float ↔ integer conversions with truncation/overflow
- converts between floating-point and integer types with truncation
- handles overflow by platform-defined behavior
- target type must be numeric
- source type must be numeric
- at least one source or target must be a floating-point type

Rule U3: `ptrcast` - for pointer and address conversions
- converts between any pointer types
- converts between pointers and integer types
- converts between function pointer types
- target or source must be a pointer type or integer type used as address

Rule U4: `bitcast` - for raw bit reinterpretation
- reinterprets bit pattern without conversion
- source and target types must have identical size
- the source must satisfy the alignment requirements of the target type
- no value conversion is performed
- commonly used for type punning

#### Level 4: assignment and context rules

Rule A1: for variable declarations `var x : T = expr`
- the entire expr is evaluated in context of type `T`
- literals adopt type `T` (subject to overflow rules LO1-LO3)
- implicit conversions apply automatically
- explicit casts required for non-implicit conversions

Rule A2: for assignment x = expr where x has type T
- if expr's type is not `T`, an implicit conversion must be valid by rules C1-C8
- otherwise, an explicit cast of appropriate level is required