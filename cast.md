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
- pointer types: any type of the form `T*`
- array types: any type of the form `T[N]` or `T[]`
- struct types: any user-defined struct
- function types: any function pointer type

### 2. Literal type assignment

Rule L1: context-aware integer literals:
- in a declaration context `var x : T = expr`, all integer literals in expr adopt type `T`
- if no context is available, literals default to `int32`

Rule L2: context-aware floating literals:
- in a declaration context `var x : T = expr`, all floating literals in expr adopt type `T`
- if no context is available, literals default to `float`

Rule L3: string literals have type `uint8*`

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
- both operands must have the same type after applying implicit conversion rules
- the result has the same type as the operands
  
Rule E2 (comparison operators): for operations `==` `!=` `<` `>` `<=` `>=`
- both operands must have the same type after applying implicit conversion rules
- the result has type `bool`
  
Rule E3 (logical operators): for operations `and` `or`
- both operands must have type `bool`
- the result has type `bool`
  
Rule E4 (unary operators): for operations `-` `~`
- the operand must be a numeric type
- the result has the same type as the operand
  
Rule E5 (unary operator): for operations `!` `not`
- The operand must have type `bool`
- The result has type `bool`

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

### 6. Explicit cast rules

Rule X1: explicit casts can convert between any numeric types

Rule X2: explicit casts can convert between any pointer types

Rule X3: explicit casts can convert between numeric and pointer types

Rule X4: explicit casts between incompatible types ?

### 7. Assignment rules

Rule A1: for variable declarations `var x : T = expr`
- the entire expr is evaluated in context of type `T`
- literals adopt type `T`
- no explicit cast is required
- if expr contains non-literal subexpressions of incompatible types, explicit casts are still required for those

Rule A2: for expression `x = expr` where `x` has type `T`
- if expr's type is not `T`, an implicit conversion must be valid by rules C1-C8
- otherwise, an explicit cast is required

## Examples

`var x : uint64 = 16 >> 1`

1. compiler identifies declaration type `uint64` for variable x
2. according to Rule A1, the expression `16 >> 1` is evaluated in context of type `uint64`:
- the literals `16` and `1` are typed as `uint64` (Rule L1)
- the shift operation `>>` produces a `uint64` result
3. the result is already `uint64`, no explicit cast needed

`var x : uint8 = (34 * 34) / 34`
- declaration context is `uint8`
- all literals are typed as `uint8`
- result is `uint8`, no cast needed