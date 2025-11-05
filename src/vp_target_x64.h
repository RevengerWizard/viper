/*
** vp_target_x64.h
** x64 target configurations
*/

#ifndef _VP_TARGET_X64_H
#define _VP_TARGET_X64_H

#include "vp_def.h"

/* REX prefix bits */
#define REX_W 0x48  /* 64 bit operand size */
#define REX_R 0x44  /* Extension of ModR/M reg field */
#define REX_X 0x42  /* Extension of SIB index field */
#define REX_B 0x41  /* Extension of ModR/M r/m, SIB base, or opcode reg */

/* ModR/M encoding */
#define MODRM(mod, reg, rm) ((((mod) & 3) << 6) | (((reg) & 7) << 3) | ((rm) & 7))

#define RM_SIB 4    /* R/M = 4; SIB byte follows */

/* SIB encoding */
#define SIB(scale, idx, base) ((((scale) & 3) << 6) | (((idx) & 7) << 3) | ((base) & 7))

/* Check for R8-R15 registers (needs REX extension) */
#define regext(r) ((r) & R8)

/* Register encoding */
typedef enum
{
    RAX = 0, RCX = 1, RDX = 2, RBX = 3,
    RSP = 4, RBP = 5, RSI = 6, RDI = 7,
    R8 = 8, R9 = 9, R10 = 10, R11 = 11,
    R12 = 12, R13 = 13, R14 = 14, R15 = 15,

    XMM0 = 0, XMM1 = 1, XMM2 = 2, XMM3 = 3,
    XMM4 = 4, XMM5 = 5, XMM6 = 6, XMM7 = 7,
    XMM8 = 8, XMM9 = 9, XMM10 = 10, XMM11 = 11,
    XMM12 = 12, XMM13 = 13, XMM14 = 14, XMM15 = 15,

    RIP = 0xFFF
} X64Reg;

#define X64_IREG (16)
#define X64_FREG (16)

typedef int64_t X64Mem;

#define NOREG (0)

#define MEM(base, idx, scale, off) \
    (INT64_MIN | ((int64_t) ((base) & 0xfff) << 32) | ((int64_t) ((idx) & 0xfff) << 44) | ((int64_t) ((scale) & 0xf) << 56) | ((off) & 0xffffffff))

/* Condition codes */
typedef enum
{
    CC_O,   /* Overflow (OF=1) */
    CC_NO,  /* No overflow (OF=0) */
    CC_B,   /* Below (CF=1) */
    CC_AE,  /* Above or Equal (CF=0) */
    CC_E,   /* Equal (ZF=1) */
    CC_NE,  /* Not Equal (ZF=0) */
    CC_BE,  /* Below or Equal (CF=1 or ZF=1) */
    CC_A,   /* Above (CF=0 and ZF=0) */
    CC_S,   /* Sign (SF=1) */
    CC_NS,  /* No Sign (SF=1) */
    CC_P,   /* Parity (PF=1) */
    CC_NP,  /* No Parity (PF=0) */
    CC_L,   /* Less (SF != OF) */
    CC_GE,  /* Greater or Equal (SF = OF) */
    CC_LE,  /* Less or Equal (ZF=1 or SF != OF) */
    CC_G    /* Greater (ZF=0 and SF=OF) */
} X64CC;

#endif