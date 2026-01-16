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
    float f32;
    double f64;
} Val;

/* Operand flags */
enum
{
    OPR_LIT = 1 << 0,       /* Literal value */
    OPR_CONST = 1 << 1,     /* Constexpr value */
    OPR_LVAL = 1 << 2,      /* lvalue */
    OPR_UNTYPED = 1 << 3    /* Untyped literal */
};

#define opr_islit(o)    ((o).flags & OPR_LIT)
#define opr_isconst(o)  ((o).flags & OPR_CONST)
#define opr_islval(o)   ((o).flags & OPR_LVAL)
#define opr_isuntyped(o) ((o).flags & OPR_UNTYPED)

typedef struct Operand
{
    Type* ty;
    Val val;
    uint8_t flags;
} Operand;

typedef enum SymKind
{
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_FN,
    SYM_DEF,
    SYM_ALIAS,
    SYM_TYPE,
    SYM_ENUM,
    SYM_MODULE,
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
    struct Module* mod;
    Str* name;
    Decl* decl;
    Type* type;
    Val val;
    bool local;
} Sym;

void vp_sema_decls(vec_t(Decl*) decls);
void vp_sema_imports(vec_t(Decl*) decls);
vec_t(Decl*) vp_sema(Str* name);

#endif