/*
** vp_sema.h
** Semantic analyser
*/

#ifndef _VP_SEMA_H
#define _VP_SEMA_H

#include "vp_ast.h"
#include "vp_str.h"
#include "vp_type.h"

typedef enum SymKind
{
    SYM_NONE,
    SYM_VAR,
    SYM_FN,
    SYM_TYPE
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

typedef union Val
{
    bool b;
    uint8_t u8;
    int8_t i8;
    uint16_t u16;
    int16_t i16;
    uint32_t u32;
    uint32_t i32;
    uint64_t u64;
    uint64_t i64;
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

void vp_sema(Decl** decls);

#endif