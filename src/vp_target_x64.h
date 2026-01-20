/*
** vp_target_x64.h
** x64 target configurations
*/

#ifndef _VP_TARGET_X64_H
#define _VP_TARGET_X64_H

#include "vp_def.h"

/* Register encoding
**
** uint64_t: [reserved:48][sub:4][size:4][class:4][num:4]
**           63        16  15  12 11   8  7     4  3    0
*/

#define REG_NUM(r) ((r) & 0xF)
#define REG_CLASS(r) ((r >> 4) & 0XF)
#define REG_SIZE(r) ((r >> 8) & 0XF)
#define REG_SUB(r) ((r >> 12) & 0XF)

#define X64REG(num, cls, size, sub) \
    ((uint64_t)(num) | ((uint64_t)(cls) << 4) | \
    ((uint64_t)(size) << 8) | ((uint64_t)(sub) << 12))

/* Register class */
enum
{
    RC_GPR, /* General purpose register */
    RC_IP,  /* Instruction pointer */
    RC_XMM, /* SSE */
    RC_YMM, /* AVX */
    RC_ZMM, /* AVX-512 */
    RC_SEG, /* Segment register */
};

/* Subregister */
enum
{
    SUB_LO,
    SUB_HI,
};

/* 8-bit low */
#define AL X64REG(0, RC_GPR, 1, SUB_LO)
#define CL X64REG(1, RC_GPR, 1, SUB_LO)
#define DL X64REG(2, RC_GPR, 1, SUB_LO)
#define BL X64REG(3, RC_GPR, 1, SUB_LO)
#define SPL X64REG(4, RC_GPR, 1, SUB_LO)
#define BPL X64REG(5, RC_GPR, 1, SUB_LO)
#define SIL X64REG(6, RC_GPR, 1, SUB_LO)
#define DIL X64REG(7, RC_GPR, 1, SUB_LO)
#define R8B X64REG(8, RC_GPR, 1, SUB_LO)
#define R9B X64REG(9, RC_GPR, 1, SUB_LO)
#define R10B X64REG(10, RC_GPR, 1, SUB_LO)
#define R11B X64REG(11, RC_GPR, 1, SUB_LO)
#define R12B X64REG(12, RC_GPR, 1, SUB_LO)
#define R13B X64REG(13, RC_GPR, 1, SUB_LO)
#define R14B X64REG(14, RC_GPR, 1, SUB_LO)
#define R15B X64REG(15, RC_GPR, 1, SUB_LO)

/* 8-bit high */
#define AH X64REG(0, RC_GPR, 1, SUB_HI)
#define CH X64REG(1, RC_GPR, 1, SUB_HI)
#define DH X64REG(2, RC_GPR, 1, SUB_HI)
#define BH X64REG(3, RC_GPR, 1, SUB_HI)

/* 16-bit */
#define AX X64REG(0, RC_GPR, 2, SUB_LO)
#define CX X64REG(1, RC_GPR, 2, SUB_LO)
#define DX X64REG(2, RC_GPR, 2, SUB_LO)
#define BX X64REG(3, RC_GPR, 2, SUB_LO)
#define SP X64REG(4, RC_GPR, 2, SUB_LO)
#define BP X64REG(5, RC_GPR, 2, SUB_LO)
#define SI X64REG(6, RC_GPR, 2, SUB_LO)
#define DI X64REG(7, RC_GPR, 2, SUB_LO)
#define R8W X64REG(8, RC_GPR, 2, SUB_LO)
#define R9W X64REG(9, RC_GPR, 2, SUB_LO)
#define R10W X64REG(10, RC_GPR, 2, SUB_LO)
#define R11W X64REG(11, RC_GPR, 2, SUB_LO)
#define R12W X64REG(12, RC_GPR, 2, SUB_LO)
#define R13W X64REG(13, RC_GPR, 2, SUB_LO)
#define R14W X64REG(14, RC_GPR, 2, SUB_LO)
#define R15W X64REG(15, RC_GPR, 2, SUB_LO)

/* 32-bit */
#define EAX X64REG(0, RC_GPR, 4, SUB_LO)
#define ECX X64REG(1, RC_GPR, 4, SUB_LO)
#define EDX X64REG(2, RC_GPR, 4, SUB_LO)
#define EBX X64REG(3, RC_GPR, 4, SUB_LO)
#define ESP X64REG(4, RC_GPR, 4, SUB_LO)
#define EBP X64REG(5, RC_GPR, 4, SUB_LO)
#define ESI X64REG(6, RC_GPR, 4, SUB_LO)
#define EDI X64REG(7, RC_GPR, 4, SUB_LO)
#define R8D X64REG(8, RC_GPR, 4, SUB_LO)
#define R9D X64REG(9, RC_GPR, 4, SUB_LO)
#define R10D X64REG(10, RC_GPR, 4, SUB_LO)
#define R11D X64REG(11, RC_GPR, 4, SUB_LO)
#define R12D X64REG(12, RC_GPR, 4, SUB_LO)
#define R13D X64REG(13, RC_GPR, 4, SUB_LO)
#define R14D X64REG(14, RC_GPR, 4, SUB_LO)
#define R15D X64REG(15, RC_GPR, 4, SUB_LO)

/* 64-bit */
#define RAX X64REG(0, RC_GPR, 8, SUB_LO)
#define RCX X64REG(1, RC_GPR, 8, SUB_LO)
#define RDX X64REG(2, RC_GPR, 8, SUB_LO)
#define RBX X64REG(3, RC_GPR, 8, SUB_LO)
#define RSP X64REG(4, RC_GPR, 8, SUB_LO)
#define RBP X64REG(5, RC_GPR, 8, SUB_LO)
#define RSI X64REG(6, RC_GPR, 8, SUB_LO)
#define RDI X64REG(7, RC_GPR, 8, SUB_LO)
#define R8 X64REG(8, RC_GPR, 8, SUB_LO)
#define R9 X64REG(9, RC_GPR, 8, SUB_LO)
#define R10 X64REG(10, RC_GPR, 8, SUB_LO)
#define R11 X64REG(11, RC_GPR, 8, SUB_LO)
#define R12 X64REG(12, RC_GPR, 8, SUB_LO)
#define R13 X64REG(13, RC_GPR, 8, SUB_LO)
#define R14 X64REG(14, RC_GPR, 8, SUB_LO)
#define R15 X64REG(15, RC_GPR, 8, SUB_LO)

/* 128-bit */
#define XMM0 X64REG(0, RC_XMM, 16, SUB_LO)
#define XMM1 X64REG(1, RC_XMM, 16, SUB_LO)
#define XMM2 X64REG(2, RC_XMM, 16, SUB_LO)
#define XMM3 X64REG(3, RC_XMM, 16, SUB_LO)
#define XMM4 X64REG(4, RC_XMM, 16, SUB_LO)
#define XMM5 X64REG(5, RC_XMM, 16, SUB_LO)
#define XMM6 X64REG(6, RC_XMM, 16, SUB_LO)
#define XMM7 X64REG(7, RC_XMM, 16, SUB_LO)
#define XMM8 X64REG(8, RC_XMM, 16, SUB_LO)
#define XMM9 X64REG(9, RC_XMM, 16, SUB_LO)
#define XMM10 X64REG(10, RC_XMM, 16, SUB_LO)
#define XMM11 X64REG(11, RC_XMM, 16, SUB_LO)
#define XMM12 X64REG(12, RC_XMM, 16, SUB_LO)
#define XMM13 X64REG(13, RC_XMM, 16, SUB_LO)
#define XMM14 X64REG(14, RC_XMM, 16, SUB_LO)
#define XMM15 X64REG(15, RC_XMM, 16, SUB_LO)

#define RIP X64REG(0, RC_IP, 8, SUB_LO)

typedef uint64_t X64Reg;
typedef uint64_t X64Mem;

/* Register indices */
enum
{
    RN_AX,
    RN_CX,
    RN_DX,
    RN_BX,
    RN_SP,
    RN_BP,
    RN_SI,
    RN_DI,
    RN_8,
    RN_9,
    RN_10,
    RN_11,
    RN_12,
    RN_13,
    RN_14,
    RN_15
};

enum
{
    XN_0,
    XN_1,
    XN_2,
    XN_3,
    XN_4,
    XN_5,
    XN_6,
    XN_7,
    XN_8,
    XN_9,
    XN_10,
    XN_11,
    XN_12,
    XN_13,
    XN_14,
    XN_15
};

/* Memory addressing encoding
**
** uint64_t: [reserved:13][rip:1][size:4][seg:4][disp:32][scale:2][index:4][base:4]
**           63         51    50   49  46 45  42 41    10  9     8  7     4  3    0
*/

/* Segment override */
typedef enum
{
    SEG_NONE,
    SEG_ES,
    SEG_CS,
    SEG_SS,
    SEG_DS,
    SEG_FS,
    SEG_GS,
} X64Seg;

#define NOREG (RN_SP)

#define X64MEM(base, idx, scale, disp, size) \
    ((uint64_t)(base) | ((uint64_t)(idx) << 4) | \
    ((uint64_t)(__builtin_ctz(scale)) << 8) | \
    ((uint64_t)((uint32_t)(disp)) << 10) | \
    ((uint64_t)(size) << 46))

#define X64MEM_RIP(disp, size) \
    (X64MEM(NOREG, NOREG, 1, disp, size) | (1ULL << 50))

#define X64MEM_SEG(base, idx, scale, disp, size, seg) \
    (X64MEM(base, idx, scale, disp, size) | ((uint64_t)(seg) << 42))

#define MEM_BASE(m)  ((m) & 0xF)
#define MEM_INDEX(m) (((m) >> 4) & 0xF)
#define MEM_SCALE(m) (1u << (((m) >> 8) & 0x3))
#define MEM_DISP(m)  ((int32_t)(((m) >> 10) & 0xFFFFFFFF))
#define MEM_SIZE(m)  (((m) >> 46) & 0xF)
#define MEM_SEG(m)   (((m) >> 42) & 0xF)
#define MEM_ISRIP(m) (((m) >> 50) & 1)

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

#define X64_IREG (16)
#define X64_FREG (16)

#endif