/*
** vp_state.h
** State management
*/

#ifndef _VP_STATE_H
#define _VP_STATE_H

#include "vp_def.h"
#include "vp_buf.h"

typedef const char* (*Reader)(void* ud, size_t* size);

typedef struct VpState
{
    SBuf code;
} VpState;

void vp_load(VpState* V, const char* filename);

VpState* vp_state_open();
void vp_state_close(VpState* V);

#endif