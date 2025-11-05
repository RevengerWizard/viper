/*
** vp_emit.h
** Instruction emitter interface
*/

#ifndef _VP_EMIT_H
#define _VP_EMIT_H

#include "vp_def.h"
#include "vp_buf.h"
#include "vp_state.h"

/* Emit a single byte */
static VP_AINLINE void emit_u8(VpState* V, uint8_t b)
{
    char* p = vp_buf_more(&V->code, 1);
    p[0] = (char)b;
    V->code.w = p + 1;
}

/* Emit a 16 bit immediate */
static VP_AINLINE void emit_im16(VpState* V, int16_t imm)
{
    char* p = vp_buf_more(&V->code, 2);
    p[0] = (char)(imm & 0xFF);
    p[1] = (char)((imm >> 8) & 0xFF);
    V->code.w = p + 2;
}

/* Emit a 32 bit immediate */
static VP_AINLINE void emit_im32(VpState* V, int32_t imm)
{
    char* p = vp_buf_more(&V->code, 4);
    p[0] = (char)(imm & 0xFF);
    p[1] = (char)((imm >> 8) & 0xFF);
    p[2] = (char)((imm >> 16) & 0xFF);
    p[3] = (char)((imm >> 24) & 0xFF);
    V->code.w = p + 4;
}

/* Emit a 64 bit immediate */
static VP_AINLINE void emit_im64(VpState* V, int64_t imm)
{
    char* p = vp_buf_more(&V->code, 8);
    p[0] = (char)(imm & 0xFF);
    p[1] = (char)((imm >> 8) & 0xFF);
    p[2] = (char)((imm >> 16) & 0xFF);
    p[3] = (char)((imm >> 24) & 0xFF);
    p[4] = (char)((imm >> 32) & 0xFF);
    p[5] = (char)((imm >> 40) & 0xFF);
    p[6] = (char)((imm >> 48) & 0xFF);
    p[7] = (char)((imm >> 56) & 0xFF);
    V->code.w = p + 8;
}

#endif