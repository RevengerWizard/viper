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
    VS_GLOBAL = 1 << 0,
};

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

/* Check if variable is local storage */
static VP_AINLINE bool vp_var_isloc(VarInfo* vi)
{
    return !(vi->storage & VS_GLOBAL);
}

/* Check if it's global scope */
static VP_AINLINE bool vp_scope_isglob(Scope* scope)
{
    vp_assertX(scope->parent || scope == V->globscope, "glob");
    return scope->parent == NULL;
}

/* Variables */
VarInfo* vp_var_new(Str* name, Type* ty);
VarInfo* vp_var_find(VarInfo** vars, Str* name);

#endif