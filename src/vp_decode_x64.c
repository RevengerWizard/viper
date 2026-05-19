/*
** vp_decode_x64.c
** x64 instruction decoder
*/

#include <string.h>

#include "vp_decode_x64.h"

/*
** A very limited and basic instruction decoder
** Covers:
**   - REX prefix (W/R/X/B)
**   - 66h operand-size prefix
**   - ModRM + SIB + displacement
**   - 8/16/32/64-bit GPRs, XMM registers
**   - Opcodes present in vp_asm.h plus common branches/calls
*/

/* Operand decoding kinds (Intel manual notation) */
typedef enum
{
    DEC_ZO,   /* no operands */
    DEC_RM,   /* reg <- modrm */
    DEC_MR,   /* modrm <- reg */
    DEC_MI,   /* modrm <- imm */
    DEC_M,    /* modrm only (single operand) */
    DEC_OI,   /* opcode+reg <- imm (MOV r, imm) */
    DEC_I,    /* imm only */
    DEC_D,    /* rel offset (Jcc, CALL, JMP) */
} DecKind;

/* Immediate width independent of operand size */
typedef enum
{
    IW_NONE,
    IW_8,     /* sign-extended 8-bit */
    IW_16,    /* 16-bit (for operand-size 16 instructions) */
    IW_32,    /* sign-extended 32-bit */
    IW_FULL,  /* same as operand size (8/16/32/64) */
} ImmWidth;

/* Opcode table entry */
typedef struct
{
    uint8_t op1;        /* primary opcode byte */
    uint8_t op2;        /* secondary byte (0F xx); 0 if unused */
    uint8_t reg_field;  /* /digit in ModRM.reg; 0xFF = unused */
    uint8_t need_rex_w; /* 1 = only valid with REX.W */
    uint8_t dec;        /* DecKind */
    uint8_t imm_width;  /* ImmWidth */
    uint8_t opr_size;   /* explicit operand size override; 0 = from prefix/REX */
    uint8_t src_size;   /* for MOVSX/MOVZX: source operand size; 0 = same as dst */
    uint8_t is_xmm;     /* operands are XMM (not GPR) */
    const char* mnem;
} OpcEntry;

#define OPC(op1, op2, reg, rexw, dec, iw, osz, ssz, xmm, mnem) \
    { op1, op2, reg, rexw, dec, iw, osz, ssz, xmm, mnem }

#define NO_REG 0xFF
#define NO_OP2 0x00

/* -- Decoder tables ------------------------------------------------ */

/* Primary (single-byte) opcode table */
static const OpcEntry opc1_table[] =
{
    /* MOV r/m, r */
    OPC(0x88, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "mov"),   /* 8-bit */
    OPC(0x89, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "mov"),
    /* MOV r, r/m */
    OPC(0x8A, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "mov"),   /* 8-bit */
    OPC(0x8B, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "mov"),
    /* MOV r/m, imm */
    OPC(0xC6, NO_OP2,      0, 0, DEC_MI, IW_8,    1, 0, 0, "mov"),   /* 8-bit */
    OPC(0xC7, NO_OP2,      0, 0, DEC_MI, IW_32,   0, 0, 0, "mov"),
    /* MOV r, imm64  (opcode+reg) */
    OPC(0xB0, NO_OP2, NO_REG, 0, DEC_OI, IW_FULL, 1, 0, 0, "mov"),   /* B0+r 8-bit */
    OPC(0xB8, NO_OP2, NO_REG, 0, DEC_OI, IW_FULL, 0, 0, 0, "mov"),   /* B8+r */

    /* LEA */
    OPC(0x8D, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "lea"),

    /* PUSH/POP r/m */
    OPC(0x50, NO_OP2, NO_REG, 0, DEC_OI, IW_NONE, 8, 0, 0, "push"),  /* 50+r */
    OPC(0x58, NO_OP2, NO_REG, 0, DEC_OI, IW_NONE, 8, 0, 0, "pop"),   /* 58+r */
    OPC(0xFF, NO_OP2,      6, 0, DEC_M,  IW_NONE, 8, 0, 0, "push"),
    OPC(0x8F, NO_OP2,      0, 0, DEC_M,  IW_NONE, 8, 0, 0, "pop"),

    /* Arithmetic r/m, r */
    OPC(0x00, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "add"),
    OPC(0x01, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "add"),
    OPC(0x02, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "add"),
    OPC(0x03, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "add"),
    OPC(0x08, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "or"),
    OPC(0x09, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "or"),
    OPC(0x0A, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "or"),
    OPC(0x0B, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "or"),
    OPC(0x18, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "sbb"),
    OPC(0x19, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "sbb"),
    OPC(0x1A, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "sbb"),
    OPC(0x1B, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "sbb"),
    OPC(0x20, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "and"),
    OPC(0x21, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "and"),
    OPC(0x22, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "and"),
    OPC(0x23, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "and"),
    OPC(0x28, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "sub"),
    OPC(0x29, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "sub"),
    OPC(0x2A, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "sub"),
    OPC(0x2B, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "sub"),
    OPC(0x30, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "xor"),
    OPC(0x31, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "xor"),
    OPC(0x32, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "xor"),
    OPC(0x33, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "xor"),
    OPC(0x38, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "cmp"),
    OPC(0x39, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "cmp"),
    OPC(0x3A, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 1, 0, 0, "cmp"),
    OPC(0x3B, NO_OP2, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "cmp"),

    /* Arithmetic r/m, imm  (GRP1) */
    OPC(0x80, NO_OP2,      0, 0, DEC_MI, IW_8,    1, 0, 0, "add"),
    OPC(0x81, NO_OP2,      0, 0, DEC_MI, IW_32,   0, 0, 0, "add"),
    OPC(0x83, NO_OP2,      0, 0, DEC_MI, IW_8,    0, 0, 0, "add"),
    OPC(0x80, NO_OP2,      1, 0, DEC_MI, IW_8,    1, 0, 0, "or"),
    OPC(0x81, NO_OP2,      1, 0, DEC_MI, IW_32,   0, 0, 0, "or"),
    OPC(0x83, NO_OP2,      1, 0, DEC_MI, IW_8,    0, 0, 0, "or"),
    OPC(0x80, NO_OP2,      4, 0, DEC_MI, IW_8,    1, 0, 0, "and"),
    OPC(0x81, NO_OP2,      4, 0, DEC_MI, IW_32,   0, 0, 0, "and"),
    OPC(0x83, NO_OP2,      4, 0, DEC_MI, IW_8,    0, 0, 0, "and"),
    OPC(0x80, NO_OP2,      5, 0, DEC_MI, IW_8,    1, 0, 0, "sub"),
    OPC(0x81, NO_OP2,      5, 0, DEC_MI, IW_32,   0, 0, 0, "sub"),
    OPC(0x83, NO_OP2,      5, 0, DEC_MI, IW_8,    0, 0, 0, "sub"),
    OPC(0x80, NO_OP2,      6, 0, DEC_MI, IW_8,    1, 0, 0, "xor"),
    OPC(0x81, NO_OP2,      6, 0, DEC_MI, IW_32,   0, 0, 0, "xor"),
    OPC(0x83, NO_OP2,      6, 0, DEC_MI, IW_8,    0, 0, 0, "xor"),
    OPC(0x80, NO_OP2,      7, 0, DEC_MI, IW_8,    1, 0, 0, "cmp"),
    OPC(0x81, NO_OP2,      7, 0, DEC_MI, IW_32,   0, 0, 0, "cmp"),
    OPC(0x83, NO_OP2,      7, 0, DEC_MI, IW_8,    0, 0, 0, "cmp"),

    /* TEST */
    OPC(0x84, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 1, 0, 0, "test"),
    OPC(0x85, NO_OP2, NO_REG, 0, DEC_MR, IW_NONE, 0, 0, 0, "test"),
    OPC(0xF6, NO_OP2,      0, 0, DEC_MI, IW_8,    1, 0, 0, "test"),
    OPC(0xF7, NO_OP2,      0, 0, DEC_MI, IW_32,   0, 0, 0, "test"),

    /* MUL/DIV/IDIV/NEG/NOT  (GRP3) */
    OPC(0xF6, NO_OP2,      2, 0, DEC_M,  IW_NONE, 1, 0, 0, "not"),
    OPC(0xF7, NO_OP2,      2, 0, DEC_M,  IW_NONE, 0, 0, 0, "not"),
    OPC(0xF6, NO_OP2,      3, 0, DEC_M,  IW_NONE, 1, 0, 0, "neg"),
    OPC(0xF7, NO_OP2,      3, 0, DEC_M,  IW_NONE, 0, 0, 0, "neg"),
    OPC(0xF6, NO_OP2,      4, 0, DEC_M,  IW_NONE, 1, 0, 0, "mul"),
    OPC(0xF7, NO_OP2,      4, 0, DEC_M,  IW_NONE, 0, 0, 0, "mul"),
    OPC(0xF6, NO_OP2,      5, 0, DEC_M,  IW_NONE, 1, 0, 0, "imul"),
    OPC(0xF7, NO_OP2,      5, 0, DEC_M,  IW_NONE, 0, 0, 0, "imul"),
    OPC(0xF6, NO_OP2,      6, 0, DEC_M,  IW_NONE, 1, 0, 0, "div"),
    OPC(0xF7, NO_OP2,      6, 0, DEC_M,  IW_NONE, 0, 0, 0, "div"),
    OPC(0xF6, NO_OP2,      7, 0, DEC_M,  IW_NONE, 1, 0, 0, "idiv"),
    OPC(0xF7, NO_OP2,      7, 0, DEC_M,  IW_NONE, 0, 0, 0, "idiv"),

    /* INC/DEC  (GRP4/5) */
    OPC(0xFE, NO_OP2,      0, 0, DEC_M,  IW_NONE, 1, 0, 0, "inc"),
    OPC(0xFF, NO_OP2,      0, 0, DEC_M,  IW_NONE, 0, 0, 0, "inc"),
    OPC(0xFE, NO_OP2,      1, 0, DEC_M,  IW_NONE, 1, 0, 0, "dec"),
    OPC(0xFF, NO_OP2,      1, 0, DEC_M,  IW_NONE, 0, 0, 0, "dec"),

    /* Shifts  (GRP2)  — CL variant: treat as MI with special IW */
    OPC(0xD0, NO_OP2,      4, 0, DEC_M,  IW_NONE, 1, 0, 0, "shl"),   /* shl r/m8, 1 */
    OPC(0xD1, NO_OP2,      4, 0, DEC_M,  IW_NONE, 0, 0, 0, "shl"),
    OPC(0xC0, NO_OP2,      4, 0, DEC_MI, IW_8,    1, 0, 0, "shl"),
    OPC(0xC1, NO_OP2,      4, 0, DEC_MI, IW_8,    0, 0, 0, "shl"),
    OPC(0xD2, NO_OP2,      4, 0, DEC_M,  IW_NONE, 1, 0, 0, "shl"),   /* shl r/m8, cl */
    OPC(0xD3, NO_OP2,      4, 0, DEC_M,  IW_NONE, 0, 0, 0, "shl"),
    OPC(0xD0, NO_OP2,      5, 0, DEC_M,  IW_NONE, 1, 0, 0, "shr"),
    OPC(0xD1, NO_OP2,      5, 0, DEC_M,  IW_NONE, 0, 0, 0, "shr"),
    OPC(0xC0, NO_OP2,      5, 0, DEC_MI, IW_8,    1, 0, 0, "shr"),
    OPC(0xC1, NO_OP2,      5, 0, DEC_MI, IW_8,    0, 0, 0, "shr"),
    OPC(0xD2, NO_OP2,      5, 0, DEC_M,  IW_NONE, 1, 0, 0, "shr"),
    OPC(0xD3, NO_OP2,      5, 0, DEC_M,  IW_NONE, 0, 0, 0, "shr"),
    OPC(0xD0, NO_OP2,      7, 0, DEC_M,  IW_NONE, 1, 0, 0, "sar"),
    OPC(0xD1, NO_OP2,      7, 0, DEC_M,  IW_NONE, 0, 0, 0, "sar"),
    OPC(0xC0, NO_OP2,      7, 0, DEC_MI, IW_8,    1, 0, 0, "sar"),
    OPC(0xC1, NO_OP2,      7, 0, DEC_MI, IW_8,    0, 0, 0, "sar"),
    OPC(0xD2, NO_OP2,      7, 0, DEC_M,  IW_NONE, 1, 0, 0, "sar"),
    OPC(0xD3, NO_OP2,      7, 0, DEC_M,  IW_NONE, 0, 0, 0, "sar"),

    /* CALL/JMP */
    OPC(0xE8, NO_OP2, NO_REG, 0, DEC_D,  IW_32,   4, 0, 0, "call"),
    OPC(0xFF, NO_OP2,      2, 0, DEC_M,  IW_NONE, 8, 0, 0, "call"),
    OPC(0xE9, NO_OP2, NO_REG, 0, DEC_D,  IW_32,   4, 0, 0, "jmp"),
    OPC(0xEB, NO_OP2, NO_REG, 0, DEC_D,  IW_8,    1, 0, 0, "jmp"),
    OPC(0xFF, NO_OP2,      4, 0, DEC_M,  IW_NONE, 8, 0, 0, "jmp"),

    /* Jcc short */
    OPC(0x70, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jo"),
    OPC(0x71, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jno"),
    OPC(0x72, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jb"),
    OPC(0x73, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jae"),
    OPC(0x74, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "je"),
    OPC(0x75, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jne"),
    OPC(0x76, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jbe"),
    OPC(0x77, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "ja"),
    OPC(0x78, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "js"),
    OPC(0x79, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jns"),
    OPC(0x7A, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jp"),
    OPC(0x7B, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jnp"),
    OPC(0x7C, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jl"),
    OPC(0x7D, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jge"),
    OPC(0x7E, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jle"),
    OPC(0x7F, NO_OP2, NO_REG, 0, DEC_D, IW_8,  1, 0, 0, "jg"),

    /* CWDE/CDQ/CQO */
    OPC(0x98, NO_OP2, NO_REG, 0, DEC_ZO, IW_NONE, 0, 0, 0, "cwde"),
    OPC(0x99, NO_OP2, NO_REG, 0, DEC_ZO, IW_NONE, 0, 0, 0, "cdq"),   /* also cqo with REX.W */

    /* RET */
    OPC(0xC3, NO_OP2, NO_REG, 0, DEC_ZO, IW_NONE, 0, 0, 0, "ret"),
    OPC(0xC2, NO_OP2, NO_REG, 0, DEC_I,  IW_16,   2, 0, 0, "ret"),

    /* MOVSX 64 <- 32 (REX.W 63) */
    OPC(0x63, NO_OP2, NO_REG, 1, DEC_RM, IW_NONE, 4, 0, 0, "movsxd"),
};

/* Two-byte (0F xx) opcode table */
static const OpcEntry opc2_table[] =
{
    /* Jcc near */
    OPC(0x80, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jo"),
    OPC(0x81, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jno"),
    OPC(0x82, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jb"),
    OPC(0x83, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jae"),
    OPC(0x84, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "je"),
    OPC(0x85, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jne"),
    OPC(0x86, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jbe"),
    OPC(0x87, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "ja"),
    OPC(0x88, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "js"),
    OPC(0x89, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jns"),
    OPC(0x8A, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jp"),
    OPC(0x8B, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jnp"),
    OPC(0x8C, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jl"),
    OPC(0x8D, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jge"),
    OPC(0x8E, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jle"),
    OPC(0x8F, 0x0F, NO_REG, 0, DEC_D, IW_32, 4, 0, 0, "jg"),

    /* MOVSX / MOVZX */
    OPC(0xBE, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 0, 1, 0, "movsx"),  /* src 8-bit */
    OPC(0xBF, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 0, 2, 0, "movsx"),  /* src 16-bit */
    OPC(0xB6, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 0, 1, 0, "movzx"),
    OPC(0xB7, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 0, 2, 0, "movzx"),

    /* IMUL r, r/m  (two operand) */
    OPC(0xAF, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 0, 0, 0, "imul"),

    /* SYSCALL / RDTSC / CPUID */
    OPC(0x05, 0x0F, NO_REG, 0, DEC_ZO, IW_NONE, 0, 0, 0, "syscall"),
    OPC(0x31, 0x0F, NO_REG, 0, DEC_ZO, IW_NONE, 0, 0, 0, "rdtsc"),
    OPC(0xA2, 0x0F, NO_REG, 0, DEC_ZO, IW_NONE, 0, 0, 0, "cpuid"),

    /* SSE scalar moves (F3/F2 prefix handled by caller) */
    /* MOVSS xmm, xmm/m32  (F3 0F 10) */
    OPC(0x10, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "movss"),
    /* MOVSS xmm/m32, xmm  (F3 0F 11) */
    OPC(0x11, 0x0F, NO_REG, 0, DEC_MR, IW_NONE, 4, 0, 1, "movss"),
    /* MOVSD xmm, xmm/m64  (F2 0F 10) */
    OPC(0x10, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "movsd"),
    /* MOVSD xmm/m64, xmm  (F2 0F 11) */
    OPC(0x11, 0x0F, NO_REG, 0, DEC_MR, IW_NONE, 8, 0, 1, "movsd"),

    /* SSE arithmetic */
    OPC(0x58, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "addss"),
    OPC(0x58, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "addsd"),
    OPC(0x5C, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "subss"),
    OPC(0x5C, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "subsd"),
    OPC(0x59, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "mulss"),
    OPC(0x59, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "mulsd"),
    OPC(0x5E, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "divss"),
    OPC(0x5E, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "divsd"),

    /* XORPS / XORPD  (no mandatory prefix; identified by prior 66h or none) */
    OPC(0x57, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 16, 0, 1, "xorps"),
    OPC(0x57, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 16, 0, 1, "xorpd"),

    /* COMISS / COMISD / UCOMISS / UCOMISD */
    OPC(0x2F, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "comiss"),
    OPC(0x2F, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "comisd"),
    OPC(0x2E, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "ucomiss"),
    OPC(0x2E, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "ucomisd"),

    /* CVT* */
    OPC(0x2C, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 0, "cvttss2si"),  /* dst=GPR, src=XMM/m32 */
    OPC(0x2C, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 0, "cvttsd2si"),
    OPC(0x2A, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "cvtsi2ss"),   /* dst=XMM, src=GPR */
    OPC(0x2A, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "cvtsi2sd"),
    OPC(0x5A, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 4, 0, 1, "cvtss2sd"),
    OPC(0x5A, 0x0F, NO_REG, 0, DEC_RM, IW_NONE, 8, 0, 1, "cvtsd2ss"),
};

#define OPC1_COUNT (ARRSIZE(opc1_table))
#define OPC2_COUNT (ARRSIZE(opc2_table))

/* Cursor over the input byte stream */
typedef struct
{
    const uint8_t* p;
    uint32_t len;
    uint32_t pos;
} Cur;

static VP_AINLINE int cur_ok(const Cur* c, uint32_t n) { return (c->pos + n) <= c->len; }
static VP_AINLINE uint8_t cur_u8(Cur* c) { return c->p[c->pos++]; }
static VP_AINLINE uint16_t cur_u16(Cur* c) { uint16_t v; memcpy(&v, c->p + c->pos, 2); c->pos += 2; return v; }
static VP_AINLINE uint32_t cur_u32(Cur* c) { uint32_t v; memcpy(&v, c->p + c->pos, 4); c->pos += 4; return v; }
static VP_AINLINE int8_t cur_i8(Cur* c) { return (int8_t)cur_u8(c); }
static VP_AINLINE int32_t cur_i32(Cur* c) { return (int32_t)cur_u32(c); }

/* Decode SIB byte + displacement given mod and base fields.
** Fills m->base, m->idx, m->scale, m->disp.
** Returns 0 on truncation. */
static int decode_sib_disp(Cur* c, uint8_t mod, uint8_t rm,
                            int8_t* out_base, int8_t* out_idx,
                            uint8_t* out_scale, int32_t* out_disp)
{
    int8_t base = -1;
    int8_t idx = -1;
    uint8_t scale = 1;
    int32_t disp = 0;

    if(rm == 4 && mod != 3) /* SIB present */
    {
        if(!cur_ok(c, 1)) return 0;
        uint8_t sib   = cur_u8(c);
        uint8_t ss    = (sib >> 6) & 3;
        uint8_t sidx  = (sib >> 3) & 7;
        uint8_t sbase =  sib       & 7;

        scale = (uint8_t)(1 << ss);
        if(sidx != 4) idx = (int8_t)sidx; /* RSP = no index */

        if(sbase == 5 && mod == 0)
        {
            /* disp32 only, no base */
            if(!cur_ok(c, 4)) return 0;
            disp = cur_i32(c);
        }
        else
        {
            base = (int8_t)sbase;
        }
    }
    else if(mod == 0 && rm == 5)
    {
        /* RIP-relative disp32 */
        if(!cur_ok(c, 4)) return 0;
        disp = cur_i32(c);
        base = -1; /* signal: RIP */
        idx = -1;
        scale = 1;
        *out_base = base; *out_idx = idx; *out_scale = scale; *out_disp = disp;
        return 1;
    }
    else
    {
        base = (int8_t)rm;
    }

    if(mod == 1)
    {
        if(!cur_ok(c, 1)) return 0;
        disp = (int32_t)cur_i8(c);
    }
    else if(mod == 2)
    {
        if(!cur_ok(c, 4)) return 0;
        disp = cur_i32(c);
    }

    *out_base = base;
    *out_idx = idx;
    *out_scale = scale;
    *out_disp = disp;
    return 1;
}

/* Resolve register number applying REX extension bits */
static VP_AINLINE uint8_t apply_rex(uint8_t num, uint8_t rex_bit) { return num | (rex_bit ? 8 : 0); }

static VP_AINLINE int is_hireg(uint8_t reg, uint8_t size, uint8_t rex)
{
    return (size == 1 && rex == 0 && reg >= 4 && reg <= 7);
}

/* Build a DOPR_REG operand */
static VP_AINLINE DecOpr dopr_reg(uint8_t reg, uint8_t size, uint8_t rex)
{
    DecOpr o;
    o.kind = DOPR_REG;
    o.size = size;
    o.r.reg = reg;
    o.r.hireg = is_hireg(reg, size, rex);
    return o;
}

/* Build a DOPR_XMM operand */
static VP_AINLINE DecOpr dopr_xmm(uint8_t reg, uint8_t size)
{
    DecOpr o; o.kind = DOPR_XMM; o.size = size; o.r.reg = reg; return o;
}

/* Build a DOPR_MEM operand */
static DecOpr dopr_mem(int8_t base, int8_t idx, uint8_t scale, int32_t disp,
                       uint8_t psize, uint8_t osize)
{
    DecOpr o;
    o.kind = DOPR_MEM;
    o.size = osize;
    o.m.base = base;
    o.m.idx = idx;
    o.m.scale = scale;
    o.m.disp = disp;
    o.m.psize = psize;
    return o;
}

/* Decode the ModRM byte and produce up to two operands (reg, r/m).
** is_xmm controls whether reg-class operands use XMM.
** src_size: if non-zero, the r/m operand uses this size instead of opsz. */
static int decode_modrm(Cur* c,
                        uint8_t rex,
                        uint8_t rex_r, uint8_t rex_b, uint8_t rex_x,
                        uint8_t opsz, uint8_t src_size, int is_xmm,
                        DecOpr* dst_opr, DecOpr* src_opr)
{
    if(!cur_ok(c, 1)) return 0;
    uint8_t modrm = cur_u8(c);
    uint8_t mod = (modrm >> 6) & 3;
    uint8_t reg = (modrm >> 3) & 7;
    uint8_t rm = modrm & 7;

    uint8_t rnum = apply_rex(reg, rex_r);
    uint8_t rmnum = apply_rex(rm, rex_b);

    uint8_t rsz = opsz;
    uint8_t msz = src_size ? src_size : opsz;

    if(is_xmm)
        *dst_opr = dopr_xmm(rnum, rsz);
    else
        *dst_opr = dopr_reg(rnum, rsz, rex);

    if(mod == 3)
    {
        /* register operand */
        if(is_xmm)
            *src_opr = dopr_xmm(rmnum, msz);
        else
            *src_opr = dopr_reg(rmnum, msz, rex);
    }
    else
    {
        int8_t   base; int8_t idx; uint8_t scale; int32_t disp;
        if(!decode_sib_disp(c, mod, rm, &base, &idx, &scale, &disp)) return 0;
        /* Apply REX.B to base, REX.X to index */
        if(base >= 0) base = (int8_t)apply_rex((uint8_t)base, rex_b);
        if(idx >= 0) idx = (int8_t)apply_rex((uint8_t)idx,  rex_x);
        *src_opr = dopr_mem(base, idx, scale, disp, msz, msz);
    }
    return 1;
}

/* Match entry: opcode + optional /digit.
** For GRP entries (reg_field != NO_REG) we peek at ModRM.reg. */
static const OpcEntry* opc_lookup(const OpcEntry* table, uint32_t count,
                                  uint8_t op, const uint8_t* rest, uint32_t rest_len)
{
    uint8_t modrm_reg = 0xFF;
    if(rest_len > 0) modrm_reg = (rest[0] >> 3) & 7;

    for(uint32_t i = 0; i < count; i++)
    {
        const OpcEntry* e = &table[i];
        /* +r encodings (OI/O): match base opcode against op with low 3 bits masked */
        uint8_t cmp_op = (e->dec == DEC_OI && e->reg_field == NO_REG)
                            ? (op & 0xF8) : op;
        if(e->op1 != cmp_op) continue;
        if(e->reg_field != NO_REG && e->reg_field != modrm_reg) continue;
        return e;
    }
    return NULL;
}

/*
** Decode one instruction at p[0..len-1].
** Returns 1 on success, 0 on unknown opcode or truncated input.
** On failure inst->len == 1 so the caller can advance and continue.
*/
int vp_decodeX64(const uint8_t* p, uint32_t len, DecInst* inst)
{
    memset(inst, 0, sizeof(*inst));
    inst->len = 1;

    if(len == 0) return 0;

    Cur c = { p, len, 0 };

    /* --- prefix scanning --- */
    uint8_t rex = 0;    /* full REX byte or 0 */
    uint8_t pfx66 = 0;  /* operand-size override */
    uint8_t pfxF2 = 0;  /* REPNE / SSE scalar double */
    uint8_t pfxF3 = 0;  /* REP   / SSE scalar single */

    while(true)
    {
        if(!cur_ok(&c, 1)) return 0;
        uint8_t b = c.p[c.pos];
        if(b == 0x66) { pfx66 = 1; c.pos++; }
        else if(b == 0xF2) { pfxF2 = 1; c.pos++; }
        else if(b == 0xF3) { pfxF3 = 1; c.pos++; }
        else if((b & 0xF0) == 0x40) { rex = b; c.pos++; } /* REX */
        else break;
    }

    uint8_t rex_w = (rex >> 3) & 1;
    uint8_t rex_r = (rex >> 2) & 1;
    uint8_t rex_x = (rex >> 1) & 1;
    uint8_t rex_b = rex & 1;

    if(!cur_ok(&c, 1)) return 0;
    uint8_t op = cur_u8(&c);

    /* Two-byte escape */
    bool is2byte = (op == 0x0F);
    if(is2byte)
    {
        if(!cur_ok(&c, 1)) return 0;
        op = cur_u8(&c);
    }

    /* --- operand size --- */
    /* default: 32-bit; REX.W -> 64-bit; 66h prefix -> 16-bit */
    uint8_t opsz = rex_w ? 8 : (pfx66 ? 2 : 4);

    /* --- lookup --- */
    const OpcEntry* e = NULL;
    if(is2byte)
    {
        e = opc_lookup(opc2_table, OPC2_COUNT, op, c.p + c.pos, len - c.pos);
        /* For SSE, disambiguate MOVSS vs MOVSD, addss vs addsd, etc. by F2/F3 prefix.
        ** F3 -> single (ss), F2 -> double (sd), neither -> packed variant.
        ** We've already recorded mnem from table; just verify opr_size matches prefix. */
        if(e && e->is_xmm)
        {
            uint8_t want_sz = pfxF3 ? 4 : pfxF2 ? 8 : 16;
            /* Find the matching entry for this size */
            for(uint32_t i = 0; i < OPC2_COUNT; i++)
            {
                const OpcEntry* t = &opc2_table[i];
                if(t->op1 == op && t->is_xmm && t->opr_size == want_sz)
                {
                    e = t; break;
                }
            }
        }
    }
    else
    {
        /* movsxd: 0x63 with REX.W */
        if(op == 0x63 && rex_w)
        {
            e = opc_lookup(opc2_table, OPC2_COUNT, 0x63, c.p + c.pos, len - c.pos);
        }
        else
        {
            e = opc_lookup(opc1_table, OPC1_COUNT, op, c.p + c.pos, len - c.pos);
            /* CDQ vs CQO: same opcode 0x99, distinguished by REX.W */
            if(e && op == 0x99 && rex_w) { static const char cqo[] = "cqo"; inst->mnem = cqo; }
        }
    }

    if(!e) return 0;

    /* Override opsz from table if entry has explicit size */
    if(e->opr_size) opsz = e->opr_size;
    /* REX.W always wins for 64-bit unless opr_size is fixed */
    else if(rex_w) opsz = 8;
    else if(pfx66) opsz = 2;

    inst->mnem = e->mnem;

    /* --- decode operands based on encoding kind --- */
    switch((DecKind)e->dec)
    {
    case DEC_ZO:
        inst->nops = 0;
        break;
    case DEC_RM:
    {
        DecOpr dst, src;
        if(!decode_modrm(&c, rex, rex_r, rex_b, rex_x, opsz, e->src_size, e->is_xmm, &dst, &src))
            return 0;
        inst->ops[0] = dst;
        inst->ops[1] = src;
        inst->nops = 2;
        break;
    }
    case DEC_MR:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, rex_r, rex_b, rex_x, opsz, e->src_size, e->is_xmm, &reg_opr, &rm_opr))
            return 0;
        inst->ops[0] = rm_opr;
        inst->ops[1] = reg_opr;
        inst->nops = 2;
        break;
    }
    case DEC_MI:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, 0, rex_b, rex_x, opsz, 0, 0, &reg_opr, &rm_opr))
            return 0;
        inst->ops[0] = rm_opr;

        /* Read immediate */
        int64_t imm = 0;
        switch((ImmWidth)e->imm_width)
        {
        case IW_8:
            if(!cur_ok(&c, 1)) return 0;
            imm = (int64_t)cur_i8(&c);
            break;
        case IW_16:
            if(!cur_ok(&c, 2)) return 0;
            imm = (int64_t)(int16_t)cur_u16(&c);
            break;
        case IW_32:
            if(!cur_ok(&c, 4)) return 0;
            imm = (int64_t)cur_i32(&c);
            break;
        case IW_FULL:
            switch(opsz)
            {
            case 1: if(!cur_ok(&c,1)) return 0; imm=(int64_t)cur_i8(&c);  break;
            case 2: if(!cur_ok(&c,2)) return 0; imm=(int64_t)(int16_t)cur_u16(&c); break;
            case 4: if(!cur_ok(&c,4)) return 0; imm=(int64_t)cur_i32(&c); break;
            case 8: if(!cur_ok(&c,8)) return 0;
                    { uint64_t v; memcpy(&v,c.p+c.pos,8); c.pos+=8; imm=(int64_t)v; } break;
            }
            break;
        default: break;
        }
        DecOpr io; io.kind = DOPR_IMM; io.size = e->opr_size ? e->opr_size : opsz; io.imm = imm;
        inst->ops[1] = io;
        inst->nops = 2;
        break;
    }
    case DEC_M:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, 0, rex_b, rex_x, opsz, 0, e->is_xmm, &reg_opr, &rm_opr))
            return 0;
        inst->ops[0] = rm_opr;
        inst->nops = 1;
        break;
    }
    case DEC_OI:
    {
        /* Low 3 bits of opcode decode register; REX.B extends it */
        if(e->imm_width == IW_NONE)
        {
            /* PUSH/POP  50+r / 58+r */
            uint8_t rnum = apply_rex(op & 7, rex_b);
            inst->ops[0] = dopr_reg(rnum, opsz, rex);
            inst->nops = 1;
        }
        else
        {
            /* MOV r, imm */
            uint8_t rnum = apply_rex(op & 7, rex_b);
            inst->ops[0] = dopr_reg(rnum, opsz, rex);
            int64_t imm = 0;
            switch((ImmWidth)e->imm_width)
            {
            case IW_FULL:
                switch(opsz)
                {
                case 1: if(!cur_ok(&c,1)) return 0; imm=(int64_t)(uint8_t)cur_u8(&c); break;
                case 2: if(!cur_ok(&c,2)) return 0; imm=(int64_t)(uint16_t)cur_u16(&c); break;
                case 4: if(!cur_ok(&c,4)) return 0; imm=(int64_t)(uint32_t)cur_u32(&c); break;
                case 8: if(!cur_ok(&c,8)) return 0;
                        { uint64_t v; memcpy(&v,c.p+c.pos,8); c.pos+=8; imm=(int64_t)v; } break;
                }
                break;
            default: break;
            }
            DecOpr io; io.kind = DOPR_IMM; io.size = opsz; io.imm = imm;
            inst->ops[1] = io;
            inst->nops = 2;
        }
        break;
    }
    case DEC_I:
    {
        int64_t imm = 0;
        switch((ImmWidth)e->imm_width)
        {
        case IW_8: if(!cur_ok(&c,1)) return 0; imm=(int64_t)cur_i8(&c); break;
        case IW_16: if(!cur_ok(&c,2)) return 0; imm=(int64_t)(int16_t)cur_u16(&c); break;
        case IW_32: if(!cur_ok(&c,4)) return 0; imm=(int64_t)cur_i32(&c); break;
        default: break;
        }
        DecOpr io; io.kind = DOPR_IMM; io.size = e->opr_size; io.imm = imm;
        inst->ops[0] = io;
        inst->nops = 1;
        break;
    }
    case DEC_D:
    {
        int32_t rel = 0;
        switch((ImmWidth)e->imm_width)
        {
        case IW_8: if(!cur_ok(&c,1)) return 0; rel=(int32_t)cur_i8(&c); break;
        case IW_32: if(!cur_ok(&c,4)) return 0; rel=cur_i32(&c); break;
        default: break;
        }
        DecOpr ro; ro.kind = DOPR_REL; ro.size = 4; ro.rel = rel;
        inst->ops[0] = ro;
        inst->nops = 1;
        break;
    }
    default:
        vp_assertX(false, "bad operand decoder");
        return 0;
    }

    inst->len = (uint8_t)c.pos;
    return 1;
}