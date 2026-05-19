/*
** vp_decode_x64.h
** x64 instruction decoder
*/

#ifndef _VP_DECODE_X64_H
#define _VP_DECODE_X64_H

#include "vp_def.h"

#define DEC_MAX_OPS 3

typedef enum
{
    DOPR_NONE,
    DOPR_REG,   /* GPR */
    DOPR_XMM,   /* XMM register */
    DOPR_MEM,   /* [base + idx*scale + disp], base/idx = -1 when absent */
    DOPR_IMM,   /* immediate */
    DOPR_REL,   /* RIP-relative offset (branches) */
} DecOprKind;

typedef struct
{
    DecOprKind kind;
    uint8_t size;    /* operand size in bytes */
    union
    {
        struct { uint8_t reg; uint8_t hireg; } r;
        struct { int8_t base; int8_t idx; uint8_t scale; int32_t disp; uint8_t psize; } m;
        int64_t imm;
        int32_t rel;
    };
} DecOpr;

typedef struct
{
    const char* mnem;   /* static string, never NULL on success */
    uint8_t len;    /* total byte length of instruction */
    uint8_t nops;
    DecOpr ops[DEC_MAX_OPS];
} DecInst;

int vp_decodeX64(const uint8_t* p, uint32_t len, DecInst* inst);

#endif