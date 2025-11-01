/*
** vp_sema.h
** Semantic analyser
*/

#ifndef _VP_SEMA_H
#define _VP_SEMA_H

#include "vp_ast.h"
#include "vp_str.h"
#include "vp_type.h"

typedef union Val
{
    bool b;
    uint8_t u8;
    int8_t i8;
    uint16_t u16;
    int16_t i16;
    uint32_t u32;
    int32_t i32;
    uint64_t u64;
    int64_t i64;
    float f;
    double d;
} Val;

typedef struct Operand
{
    Type* ty;
    Val val;
    bool islit;
    bool isconst;
    bool islval;
    bool untyped;
} Operand;

typedef enum SymKind
{
    SYM_NONE,
    SYM_VAR,
    SYM_FN,
    SYM_TYPE,
    SYM_ENUM,
} SymKind;

typedef enum SymState
{
    SYM_PENDING,
    SYM_PROGRESS,
    SYM_DONE
} SymState;

typedef struct Sym
{
    SymKind kind;
    SymState state;
    Str* name;
    Decl* decl;
    Type* type;
} Sym;

vec_t(Decl*) vp_sema(vec_t(Decl*) decls);

#endif