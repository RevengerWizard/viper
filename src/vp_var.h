/*
** vp_var.h
** Variable handling
*/

#ifndef _VP_VAR_H
#define _VP_VAR_H

#include "vp_ir.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_type.h"

/* Storage */
enum
{
    VS_GLOB = 1 << 0,
    VS_FN = 1 << 1,
    VS_PUB = 1 << 2,
};

#define vs_isglob(vi) ((vi)->storage & VS_GLOB)
#define vs_isfn(vi) ((vi)->storage & VS_FN)
#define vs_ispub(vi) ((vi)->storage & VS_PUB)
#define vs_isloc(vi) (!vs_isglob(vi))

typedef struct VarInfo
{
    uint8_t storage;
    Str* name;
    Type* type;
    VReg* vreg;
    FrameInfo* fi;
} VarInfo;

typedef struct Scope
{
    struct Scope* parent;
    vec_t(VarInfo*) vars;
} Scope;

/* Scopes */
Scope* vp_scope_new(Scope* parent);
VarInfo* vp_scope_find(Scope* scope, Str* name, vec_t(Scope*) p);
VarInfo* vp_scope_add(Scope* scope, Str* name, Type* ty);
Scope* vp_scope_begin();
void vp_scope_end();

/* Check if it's global scope */
static VP_AINLINE bool vp_scope_isglob(Scope* scope)
{
    vp_assertX(scope->parent || scope == V->globscope, "glob");
    return scope->parent == NULL;
}

/* Variables */
VarInfo* vp_var_new(Str* name, Type* ty);
VarInfo* vp_var_find(vec_t(VarInfo*) vars, Str* name);

#endif