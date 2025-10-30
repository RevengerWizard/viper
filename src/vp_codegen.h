/*
** vp_codegen.h
** Code generation (AST -> IR)
*/

#ifndef _VP_CODEGEN_H
#define _VP_CODEGEN_H

#include "vp_ast.h"
#include "vp_str.h"
#include "vp_var.h"

/* Stack slot */
typedef struct Slot
{
    Type* type;
    FrameInfo* fi;
} Slot;

/* Function code blocks */
typedef struct Code
{
    Str* name;  /* Function name */
    RegAlloc* ra;
    vec_t(BB*) bbs;
    vec_t(Scope*) scopes;
    vec_t(IR*) calls; /* Calls within current code */
    vec_t(Slot) slots; /* Stack frame offsets (vars and {}) */
    uint32_t numparams;
    int32_t ofs;    /* Code begin offset */
    uint32_t framesize; /* Stack frame size */
    uint32_t stacksize; /* Stack size used */
    BB* retbb;  /* Final return basic block */
    VReg* retvr;    /* Return vreg */
    bool export;
} Code;

vec_t(Code*) vp_codegen(vec_t(Decl*) decls);

/* Determine if type parameter needs to be allocated on the stack */
static VP_AINLINE bool param_isstack(Type* ty)
{
    if(ty_isaggr(ty))
    {
        uint32_t size = vp_type_sizeof(ty);
        return size > 8;
    }
    return false;
}

#endif