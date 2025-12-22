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

#define REG_MAKE(num, cls, size, sub) \
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
#define AL REG_MAKE(0, RC_GPR, 1, SUB_LO)
#define CL REG_MAKE(1, RC_GPR, 1, SUB_LO)
#define DL REG_MAKE(2, RC_GPR, 1, SUB_LO)
#define BL REG_MAKE(3, RC_GPR, 1, SUB_LO)
#define SPL REG_MAKE(4, RC_GPR, 1, SUB_LO)
#define BPL REG_MAKE(5, RC_GPR, 1, SUB_LO)
#define SIL REG_MAKE(6, RC_GPR, 1, SUB_LO)
#define DIL REG_MAKE(7, RC_GPR, 1, SUB_LO)
#define R8B REG_MAKE(8, RC_GPR, 1, SUB_LO)
#define R9B REG_MAKE(9, RC_GPR, 1, SUB_LO)
#define R10B REG_MAKE(10, RC_GPR, 1, SUB_LO)
#define R11B REG_MAKE(11, RC_GPR, 1, SUB_LO)
#define R12B REG_MAKE(12, RC_GPR, 1, SUB_LO)
#define R13B REG_MAKE(13, RC_GPR, 1, SUB_LO)
#define R14B REG_MAKE(14, RC_GPR, 1, SUB_LO)
#define R15B REG_MAKE(15, RC_GPR, 1, SUB_LO)

/* 8-bit high */
#define AH REG_MAKE(0, RC_GPR, 1, SUB_HI)
#define CH REG_MAKE(1, RC_GPR, 1, SUB_HI)
#define DH REG_MAKE(2, RC_GPR, 1, SUB_HI)
#define BH REG_MAKE(3, RC_GPR, 1, SUB_HI)

/* 16-bit */
#define AX REG_MAKE(0, RC_GPR, 2, SUB_LO)
#define CX REG_MAKE(1, RC_GPR, 2, SUB_LO)
#define DX REG_MAKE(2, RC_GPR, 2, SUB_LO)
#define BX REG_MAKE(3, RC_GPR, 2, SUB_LO)
#define SP REG_MAKE(4, RC_GPR, 2, SUB_LO)
#define BP REG_MAKE(5, RC_GPR, 2, SUB_LO)
#define SI REG_MAKE(6, RC_GPR, 2, SUB_LO)
#define DI REG_MAKE(7, RC_GPR, 2, SUB_LO)
#define R8W REG_MAKE(8, RC_GPR, 2, SUB_LO)
#define R9W REG_MAKE(9, RC_GPR, 2, SUB_LO)
#define R10W REG_MAKE(10, RC_GPR, 2, SUB_LO)
#define R11W REG_MAKE(11, RC_GPR, 2, SUB_LO)
#define R12W REG_MAKE(12, RC_GPR, 2, SUB_LO)
#define R13W REG_MAKE(13, RC_GPR, 2, SUB_LO)
#define R14W REG_MAKE(14, RC_GPR, 2, SUB_LO)
#define R15W REG_MAKE(15, RC_GPR, 2, SUB_LO)

/* 32-bit */
#define EAX REG_MAKE(0, RC_GPR, 4, SUB_LO)
#define ECX REG_MAKE(1, RC_GPR, 4, SUB_LO)
#define EDX REG_MAKE(2, RC_GPR, 4, SUB_LO)
#define EBX REG_MAKE(3, RC_GPR, 4, SUB_LO)
#define ESP REG_MAKE(4, RC_GPR, 4, SUB_LO)
#define EBP REG_MAKE(5, RC_GPR, 4, SUB_LO)
#define ESI REG_MAKE(6, RC_GPR, 4, SUB_LO)
#define EDI REG_MAKE(7, RC_GPR, 4, SUB_LO)
#define R8D REG_MAKE(8, RC_GPR, 4, SUB_LO)
#define R9D REG_MAKE(9, RC_GPR, 4, SUB_LO)
#define R10D REG_MAKE(10, RC_GPR, 4, SUB_LO)
#define R11D REG_MAKE(11, RC_GPR, 4, SUB_LO)
#define R12D REG_MAKE(12, RC_GPR, 4, SUB_LO)
#define R13D REG_MAKE(13, RC_GPR, 4, SUB_LO)
#define R14D REG_MAKE(14, RC_GPR, 4, SUB_LO)
#define R15D REG_MAKE(15, RC_GPR, 4, SUB_LO)

/* 64-bit */
#define RAX REG_MAKE(0, RC_GPR, 8, SUB_LO)
#define RCX REG_MAKE(1, RC_GPR, 8, SUB_LO)
#define RDX REG_MAKE(2, RC_GPR, 8, SUB_LO)
#define RBX REG_MAKE(3, RC_GPR, 8, SUB_LO)
#define RSP REG_MAKE(4, RC_GPR, 8, SUB_LO)
#define RBP REG_MAKE(5, RC_GPR, 8, SUB_LO)
#define RSI REG_MAKE(6, RC_GPR, 8, SUB_LO)
#define RDI REG_MAKE(7, RC_GPR, 8, SUB_LO)
#define R8 REG_MAKE(8, RC_GPR, 8, SUB_LO)
#define R9 REG_MAKE(9, RC_GPR, 8, SUB_LO)
#define R10 REG_MAKE(10, RC_GPR, 8, SUB_LO)
#define R11 REG_MAKE(11, RC_GPR, 8, SUB_LO)
#define R12 REG_MAKE(12, RC_GPR, 8, SUB_LO)
#define R13 REG_MAKE(13, RC_GPR, 8, SUB_LO)
#define R14 REG_MAKE(14, RC_GPR, 8, SUB_LO)
#define R15 REG_MAKE(15, RC_GPR, 8, SUB_LO)

/* 128-bit */
#define XMM0 REG_MAKE(0, RC_XMM, 16, SUB_LO)
#define XMM1 REG_MAKE(1, RC_XMM, 16, SUB_LO)
#define XMM2 REG_MAKE(2, RC_XMM, 16, SUB_LO)
#define XMM3 REG_MAKE(3, RC_XMM, 16, SUB_LO)
#define XMM4 REG_MAKE(4, RC_XMM, 16, SUB_LO)
#define XMM5 REG_MAKE(5, RC_XMM, 16, SUB_LO)
#define XMM6 REG_MAKE(6, RC_XMM, 16, SUB_LO)
#define XMM7 REG_MAKE(7, RC_XMM, 16, SUB_LO)
#define XMM8 REG_MAKE(8, RC_XMM, 16, SUB_LO)
#define XMM9 REG_MAKE(9, RC_XMM, 16, SUB_LO)
#define XMM10 REG_MAKE(10, RC_XMM, 16, SUB_LO)
#define XMM11 REG_MAKE(11, RC_XMM, 16, SUB_LO)
#define XMM12 REG_MAKE(12, RC_XMM, 16, SUB_LO)
#define XMM13 REG_MAKE(13, RC_XMM, 16, SUB_LO)
#define XMM14 REG_MAKE(14, RC_XMM, 16, SUB_LO)
#define XMM15 REG_MAKE(15, RC_XMM, 16, SUB_LO)

#define RIP REG_MAKE(0, RC_IP, 8, SUB_LO)

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

#define MEM_MAKE(base, idx, scale, disp, size) \
    ((uint64_t)(base) | ((uint64_t)(idx) << 4) | \
    ((uint64_t)(__builtin_ctz(scale)) << 8) | \
    ((uint64_t)((uint32_t)(disp)) << 10) | \
    ((uint64_t)(size) << 46))

#define MEM_MAKE_RIP(disp, size) \
    (MEM_MAKE(NOREG, NOREG, 1, disp, size) | (1ULL << 50))

#define MAKE_MEM_SEG(base, idx, scale, disp, size, seg) \
    (MEM_MAKE(base, idx, scale, disp, size) | ((uint64_t)(seg) << 42))

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