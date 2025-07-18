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
    Str* name;
    RegAlloc* ra;
    vec_t(BB*) bbs;
    vec_t(Scope*) scopes;
    vec_t(IR*) calls; /* Calls within current code */
    vec_t(Slot) slots; /* Stack frame offsets (vars and {}) */
    uint32_t numparams;
    int32_t ofs;    /* Code begin offset */
    uint32_t framesize; /* Stack frame size */
} Code;

vec_t(Code*) vp_codegen(vec_t(Decl*) decls);

#endif