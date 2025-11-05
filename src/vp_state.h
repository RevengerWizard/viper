/*
** vp_state.h
** State management
*/

#ifndef _VP_STATE_H
#define _VP_STATE_H

#include <stdio.h>

#include "vp_buf.h"
#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_regalloc.h"
#include "vp_target.h"
#include "vp_type.h"
#include "vp_tab.h"
#include "vp_map.h"

typedef const char* (*VpReader)(void* ud, size_t* size);

/* Viper State */
typedef struct VpState
{
    SBuf code;
    Map cacheconst;
    Map cacheptr;
    vec_t(Type*) cachefunc;
    vec_t(Type*) cachearr;
    Tab strtab;
    Arena instarena;
    Arena strarena;
    Arena astarena;
    Arena typearena;
    Arena symarena;
    Arena irarena;
    Tab globtab;
    struct Scope* globscope;
    struct Scope* currscope;
    BB* bb;
    struct Code* fncode;
    RegAlloc* ra;
    FILE* txtfile;
    SBuf tmpbuf;
    Tab funcs;
    vec_t(Str*) strs;
    vec_t(uint32_t) strofs;
    vec_t(struct PatchInfo) patches;
    const TargetInfo* target;
} VpState;

#define TARGET_PTR_SIZE (8)

extern VpState* V;

void vp_load(VpState* V, const char* filename);

VpState* vp_state_open();
void vp_state_close(VpState* V);

#endif