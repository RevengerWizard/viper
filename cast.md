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
- floating types: `float`, `double`
- numeric types: integer and floating types
- pointer types: any type of the form `T*`
- array types: any type of the form `T[N]` or `T[]`
- struct types: any user-defined struct
- function types: any function pointer type
- scalar types: boolean, integer, floating, and pointer types

### 2. Literal type assignment

Rule L1: context-aware integer literals:
- in a declaration context `var x : T = expr`, all integer literals in expr adopt type `T`
- if no context is available, literals default to `int32`

Rule L2: context-aware floating literals:
- in a declaration context `var x : T = expr`, all floating literals in expr adopt type `T`
- if no context is available, literals default to `float`

Rule L3: string literals have type `uint8[]`

Rule L4: character literals have type `uint8`

Rule L5: `nil` literal is convertible to any pointer type

### 3. Expression type resolution in declaration context

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

### 4. Expression type resolution outside declaration context

Rule E1 (binary numeric operators): for operations `+` `-` `*` `/` `%` `&` `|` `^` `<<` `>>`
- for `+`:
    * for numeric types: both operands must have the same type after applying implicit conversion rules
    * for pointer types: allowed as `ptr + int` or `int + ptr`, resulting in a pointer of the same type as the original pointer

- for `-`:
    * for numeric types: both operands must have the same type after applying implicit conversion rules
    * for pointer types: allowed as `ptr - int` resulting in a pointer of the same type as the original pointer
    * for pointer types: allowed as `ptr - ptr` (same pointer type) resulting in an `int64` representing the difference in elements

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
- result is a pointer to the operand's type

Rule E7 (dereference operator): for operation `*`
- the operand must be a pointer type
- result is the type pointed to by the operand

### 5. Implicit conversion rules

Rule C1: a value of type `T` can be implicitly converted to type `T` (identity conversion)

Rule C2: integer widening with same signedness is allowed:
- `int8` -> `int16` -> `int32` -> `int64`
- `uint8` -> `uint16` -> `uint32` -> `uint64`

Rule C3: no implicit conversions between signed and unsigned integers

Rule C4: no implicit conversions from floating-point to integer types

Rule C5: integer to floating-point conversions are allowed only when:
- the integer type is small enough that all values can be exactly represented in the float type
- `int8`, `int16`, `uint8`, `uint16` -> `float` or `double`
- `int32`, `uint32` -> `double` only

Rule C6: no implicit conversion between floating-point types (`float` <-> `double`)

Rule C7: no implicit conversions between pointer types except:
- `T*` -> `void*` (widening)
- `nil` -> `T*` (nil pointer conversion)

Rule C8: no implicit conversions between array types

### 6. Cast type hierarchy

Viper provides four levels of type conversions:

#### Level 1: implicit conversions (automatic)

- no cast required
- governed by implicit conversion rules C1-C8
- only fully safe conversions

#### Level 2: explicit conversions

Syntax: `cast(T, expr)` or `T(expr)`

Rule S1: `cast` performs well-defined conversions that are not safe enough to be implicit

Rule S2: `cast` is valid for:
- any numeric type to any other numeric type (with defined overflow/truncation behavior)
- `void*` to any pointer type
- `bool` to numeric types (false = 0, true = 1)
- numeric types to `bool` (0 = false, non-zero = true)

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

Rule U3: `bitcast` - for raw bit reinterpretation
- reinterprets bit pattern without conversion
- source and target types must have identical size
- no value conversion is performed
- commonly used for type punning

Rule U4: `ptrcast` - for pointer and address conversions
- converts between any pointer types
- converts between pointers and integer types
- converts between function pointer types
- target or source must be a pointer type or integer type used as address

#### Level 4: assignment and context rules

Rule A1: for variable declarations `var x : T = expr`
- the entire expr is evaluated in context of type `T`
- literals adopt type `T`
- implicit conversions apply automatically
- explicit casts required for non-implicit conversions

Rule A2: for assignment x = expr where x has type T
- if expr's type is not `T`, an implicit conversion must be valid by rules C1-C8
- otherwise, an explicit cast of appropriate level is required