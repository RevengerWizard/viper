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
    MOVSX_RR, MOVZX_RR,
    LEA_RM,
    PUSH_R, POP_R,
    CWDE, CDQ, CQO,

    ADD_RR, ADD_RI,
    SUB_RR, SUB_RI,
    MUL_R, DIV_R, IDIV_R,
    INC_R, DEC_R, NEG_R,

    AND_RR, AND_RI,
    OR_RR, OR_RI,
    XOR_RR, XOR_RI,
    NOT_R,
    TEST_RR,
    CMP_RR, CMP_RI,
    SHL_RR, SHL_RI,
    SHR_RI, SAR_RI,

    MOVSS_MX, MOVSS_XM, MOVSS_XX,
    MOVSD_MX, MOVSD_XM, MOVSD_XX,

    ADDSS_XX, ADDSD_XX,
    SUBSS_XX, SUBSD_XX,
    MULSS_XX, MULSD_XX,
    DIVSS_XX, DIVSD_XX,
    XORPS_XX, XORPD_XX,

    COMISS_XX, COMISD_XX,
    UCOMISS_XX, UCOMISD_XX,

    CVTTSS2SI_RX, CVTTSD2SI_RX,
    CVTSI2SS_XR, CVTSI2SD_XR,
    CVTSS2SD_XX, CVTSD2SS_XX,

    RDTSC, CPUID, RET, SYSCALL
} OpCode;

typedef uint64_t RegType;

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
        uint64_t reg;
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