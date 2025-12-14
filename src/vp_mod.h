/*
** vp_mod.h
** Module handling
*/

#ifndef _VP_MOD_H
#define _VP_MOD_H

#include "vp_str.h"
#include "vp_tab.h"
#include "vp_vec.h"
#include "vp_ast.h"

/* Viper module */
typedef struct Module
{
    Str* name;
    Str* path;
    Tab symstab;
    vec_t(struct Sym*) syms;
    vec_t(Decl*) decls;
    vec_t(Decl*) sorted;
} Module;

Module* vp_mod_enter(Module* mod);
void vp_mod_leave(Module* mod);
Module* vp_mod_get(SrcLoc loc, Str* name);

#endif