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
    BB** bbs;
    Scope** scopes;
    IR** calls; /* Calls within current code */
    Slot* slots; /* Stack frame offsets (vars and {}) */
    uint32_t numparams;
    int32_t ofs;    /* Code begin offset */
    uint32_t framesize; /* Stack frame size */
} Code;

Code** vp_codegen(Decl** decls);

#endif