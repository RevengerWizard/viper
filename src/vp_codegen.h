/*
** vp_codegen.h
** Code generation (AST -> IR)
*/

#ifndef _VP_CODEGEN_H
#define _VP_CODEGEN_H

#include "vp_abi.h"
#include "vp_ast.h"
#include "vp_str.h"
#include "vp_target.h"
#include "vp_var.h"

/* Stack slot */
typedef struct Slot
{
    Type* type;
    FrameInfo* fi;
} Slot;

/* Function code flags */
enum
{
    FN_INLINE = 1 << 0,
    FN_SYSCALL = 1 << 1,
    FN_EXPORT = 1 << 2
};

/* Function code blocks */
typedef struct Code
{
    Str* name;  /* Function name */
    uint32_t flags;
    RegAlloc* ra;
    vec_t(BB*) bbs;
    vec_t(Scope*) scopes;
    vec_t(IR*) calls; /* Calls within current code */
    vec_t(Slot) slots; /* Stack frame offsets (vars and {}) */
    vec_t(ParamLoc) plocs;  /* Parameter locations */
    uint32_t numparams;
    int32_t ofs;    /* Code begin offset */
    uint32_t framesize; /* Stack frame size */
    uint32_t stacksize; /* Stack size used */
    BB* retbb;  /* Final return basic block */
    VReg* retvr;    /* Return vreg */
    Stmt* body;
    const ABIInfo* abi;
} Code;

vec_t(Code*) vp_codegen(vec_t(Decl*) decls);

#endif