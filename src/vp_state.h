/*
** vp_state.h
** State management
*/

#ifndef _VP_STATE_H
#define _VP_STATE_H

#include "vp_buf.h"
#include "vp_mem.h"
#include "vp_type.h"
#include "vp_tab.h"
#include "vp_map.h"

typedef const char* (*VpReader)(void* ud, size_t* size);

typedef struct VpState
{
    SBuf code;

    Map cacheptr;
    Type** cachefunc;
    Type** cachearr;

    Tab strtab;

    Arena astarena;
    Arena typearena;
    Arena symarena;
} VpState;

extern VpState* V;

void vp_load(VpState* V, const char* filename);

VpState* vp_state_open();
void vp_state_close(VpState* V);

#endif