/*
** vp_asm.h
** Inline assembler
*/

#ifndef _VP_ASM_H
#define _VP_ASM_H

#include "vp_lex.h"

typedef enum OpCode
{
    NONE,
    MOV_RR, MOV_RI, MOV_RM, MOV_MR,
    OR_RR,
    SHL_RR, SHL_RI,
    RDTSC,
    CPUID,
    RET,
} OpCode;

typedef enum
{
    /* 8-bit (low) */
    AL, CL, DL, BL,
    
    /* 8-bit (high) */
    AH, CH, DH, BH,

    /* 8-bit */
    R8B, R9B, R10B, R11B,
    R12B, R13B, R14B, R15B,

    /* 8-bit */
    SPL = R15B + 1 + 4,
    BPL, SIL, DIL,

    /* 16-bit */
    AX, CX, DX, BX, SP, BP, SI, DI,
    R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,

    /* 32-bit */
    EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
    R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,

    /* 64-bit */
    xRAX, xRCX, xRDX, xRBX, xRSP, xRBP, xRSI, xRDI,
    xR8, xR9, xR10, xR11, xR12, xR13, xR14, xR15,
    
    xRIP
} RegType;

typedef struct
{
    const char* name;
    RegType reg;
} RegInfo;

typedef enum
{
    ASM_NONE,
    ASM_REG,
    ASM_IMM,
    ASM_MEM,
} AsmOperandType;

typedef struct AsmOperand
{
    AsmOperandType type;
    union
    {
        uint8_t reg;
        int64_t imm;
        Str* ident;
        struct
        {
            uint8_t base;
            uint8_t idx;
            uint8_t scale;
            int32_t disp;
        } mem;
    };
} AsmOperand;

typedef struct Inst
{
    OpCode op;
    uint8_t count;
    AsmOperand oprs[];
} Inst;

#define inst_size(count) (sizeof(Inst) + (count * sizeof(AsmOperand)))

vec_t(Inst*) vp_asm_x64(LexState* ls);

#endif