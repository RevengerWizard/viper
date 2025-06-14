/*
** vp_var.c
** Variable handling
*/

#include "vp_var.h"
#include "vp_mem.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_tab.h"
#include "vp_type.h"
#include "vp_vec.h"

/* Create a new scope */
Scope* vp_scope_new(Scope* parent)
{
    Scope* scope = vp_arena_alloc(&V->astarena, sizeof(*scope));
    scope->parent = parent;
    scope->vars = NULL;
    return scope;
}

VarInfo* vp_scope_find(Scope* scope, Str* name, Scope** p)
{
    VarInfo* vi = NULL;
    for(; scope != NULL; scope = scope->parent)
    {
        if(vp_scope_isglob(scope))
        {
            vi = vp_tab_get(&V->globtab, name);
            break;
        }
        if(scope->vars != NULL)
        {
            vi = vp_var_find(scope->vars, name);
            if(vi)
                break;
        }
    }
    if(p) *p = scope;
    return vi;
}

VarInfo* vp_scope_add(Scope* scope, Str* name, Type* ty)
{
    VarInfo* vi = vp_var_new(name, ty);
    if(vp_scope_isglob(scope))
    {
        vi->storage = VS_GLOBAL;
        vp_tab_set(&V->globtab, name, vi);
    }
    vec_push(scope->vars, vi);
    return vi;
}

Scope* vp_scope_begin()
{
    Scope* scope = vp_scope_new(V->currscope);
    V->currscope = scope;
    return scope;
}

void vp_scope_end()
{
    vp_assertX(!vp_scope_isglob(V->currscope), "broken scope");
    V->currscope = V->currscope->parent;
}

VarInfo* vp_var_new(Str* name, Type* ty)
{
    VarInfo* vi = vp_arena_alloc(&V->astarena, sizeof(*vi));
    vi->name = name;
    vi->type = ty;
    vi->storage = 0;
    vi->vreg = NULL;
    vi->fi = NULL;
    return vi;
}

VarInfo* vp_var_find(VarInfo** vars, Str* name)
{
    for(uint32_t i = 0; i < vec_len(vars); i++)
    {
        VarInfo* vi = vars[i];
        if(vi->name == name)
            return vi;
    }
    return NULL;
}