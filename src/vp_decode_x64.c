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
    DEC_AI,   /* AL/rAX, imm */
    DEC_M1,   /* r/m, 1 */
    DEC_MC,   /* r/m, cl */
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

/* Opcode flags */
enum
{
    OPCF_XMM_DST = 1 << 0,  /* dst register is XMM */
    OPCF_XMM_SRC = 1 << 1,  /* src register is XMM */
    OPCF_SSE = 1 << 2,  /* entry is SSE, use pfx */
    OPCF_REXW = 1 << 3, /* only valid with REX.W */
    OPCF_XMM = OPCF_XMM_DST | OPCF_XMM_SRC, /* dst + src = XMM */
};

/* Opcode prefixes */
typedef enum
{
    PFX_NONE = 0,
    PFX_F3 = 1,  /* REP / SSE scalar single */
    PFX_F2 = 2,  /* REPNE / SSE scalar double */
    PFX_66 = 3,  /* operand-size / SSE packed double */
} OpcPfx;

/* Opcode table entry */
typedef struct
{
    uint8_t op1;        /* primary opcode byte */
    uint8_t op2;        /* secondary byte (0F xx); 0 if unused */
    uint8_t reg_field;  /* /digit in ModRM.reg; 0xFF = unused */
    uint8_t dec;        /* DecKind */
    uint8_t imm_width;  /* ImmWidth */
    uint8_t opr_size;   /* explicit operand size override; 0 = from prefix/REX */
    uint8_t src_size;   /* for MOVSX/MOVZX: source operand size; 0 = same as dst */
    uint8_t flags;      /* flags */
    uint8_t pfx;        /* OpcPfx */
    const char* mnem;
} OpcEntry;

#define OPC(op1, op2, reg, dec, iw, osz, ssz, flags, pfx, mnem) \
    { op1, op2, reg, dec, iw, osz, ssz, flags, pfx, mnem }

#define NO_REG 0xFF
#define NO_OP2 0x00

/* -- Decoder tables ------------------------------------------------ */

/* Primary (single-byte) opcode table */
static const OpcEntry opc1_table[] =
{
    OPC(0x88, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "mov"),/* MOV r/m8, r8 */
    OPC(0x89, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "mov"),/* MOV r/m, r */
    OPC(0x8A, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "mov"),/* MOV r8, r/m8 */
    OPC(0x8B, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "mov"),/* MOV r, r/m */
    OPC(0xC6, NO_OP2,      0, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "mov"),/* MOV r/m8, imm8 */
    OPC(0xC7, NO_OP2,      0, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "mov"),/* MOV r/m, imm */
    OPC(0xB0, NO_OP2, NO_REG, DEC_OI, IW_FULL, 1, 0, 0, PFX_NONE, "mov"),/* MOV r8, imm8 */
    OPC(0xB8, NO_OP2, NO_REG, DEC_OI, IW_FULL, 0, 0, 0, PFX_NONE, "mov"),/* MOV r, imm */

    OPC(0x8D, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "lea"),/* LEA r, m */

    OPC(0x50, NO_OP2, NO_REG, DEC_OI, IW_NONE, 8, 0, 0, PFX_NONE, "push"),/* PUSH r */
    OPC(0xFF, NO_OP2,      6, DEC_M,  IW_NONE, 8, 0, 0, PFX_NONE, "push"),/* PUSH r/m */
    OPC(0x58, NO_OP2, NO_REG, DEC_OI, IW_NONE, 8, 0, 0, PFX_NONE, "pop"),/* POP r */
    OPC(0x8F, NO_OP2,      0, DEC_M,  IW_NONE, 8, 0, 0, PFX_NONE, "pop"),/* POP r/m */

    OPC(0x00, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "add"),/* ADD r/m8, r8 */
    OPC(0x01, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "add"),/* ADD r/m, r */
    OPC(0x02, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "add"),/* ADD r8, r/m8 */
    OPC(0x03, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "add"),/* ADD r, r/m */
    OPC(0x04, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "add"),/* ADD AL, imm8 */
    OPC(0x05, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "add"),/* ADD rAX, imm */
    OPC(0x80, NO_OP2,      0, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "add"),/* ADD r/m8, imm8 */
    OPC(0x81, NO_OP2,      0, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "add"),/* ADD r/m, imm */
    OPC(0x83, NO_OP2,      0, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "add"),/* ADD r/m, imm8 */

    OPC(0x08, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "or"),/* OR r/m8, r8 */
    OPC(0x09, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "or"),/* OR r/m, r */
    OPC(0x0A, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "or"),/* OR r8, r/m8 */
    OPC(0x0B, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "or"),/* OR r, r/m */
    OPC(0x0C, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "or"),/* OR AL, imm8 */
    OPC(0x0D, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "or"),/* OR rAX, imm */
    OPC(0x80, NO_OP2,      1, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "or"),/* OR r/m8, imm8 */
    OPC(0x81, NO_OP2,      1, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "or"),/* OR r/m, imm */
    OPC(0x83, NO_OP2,      1, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "or"),/* OR r/m, imm8 */

    OPC(0x14, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "adc"),/* ADC AL, imm8 */
    OPC(0x15, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "adc"),/* ADC rAX, imm */

    OPC(0x18, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "sbb"),/* SBB r/m8, r8 */
    OPC(0x19, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "sbb"),/* SBB r/m, r */
    OPC(0x1A, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "sbb"),/* SBB r8, r/m8 */
    OPC(0x1B, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "sbb"),/* SBB r, r/m */
    OPC(0x1C, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "sbb"),/* SBB AL, imm8 */
    OPC(0x1D, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "sbb"),/* SBB rAX, imm */

    OPC(0x20, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "and"),/* AND r/m8, r8 */
    OPC(0x21, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "and"),/* AND r/m, r */
    OPC(0x22, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "and"),/* AND r8, r/m8 */
    OPC(0x23, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "and"),/* AND r, r/m */
    OPC(0x24, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "and"),/* AND AL, imm8 */
    OPC(0x25, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "and"),/* AND rAX, imm */
    OPC(0x80, NO_OP2,      4, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "and"),/* AND r/m8, imm8 */
    OPC(0x81, NO_OP2,      4, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "and"),/* AND r/m, imm */
    OPC(0x83, NO_OP2,      4, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "and"),/* AND r/m, imm8 */

    OPC(0x28, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "sub"),/* SUB r/m8, r8 */
    OPC(0x29, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "sub"),/* SUB r/m, r */
    OPC(0x2A, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "sub"),/* SUB r8, r/m8 */
    OPC(0x2B, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "sub"),/* SUB r, r/m */
    OPC(0x2C, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "sub"),/* SUB AL, imm8 */
    OPC(0x2D, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "sub"),/* SUB rAX, imm */
    OPC(0x80, NO_OP2,      5, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "sub"),/* SUB r/m8, imm8 */
    OPC(0x81, NO_OP2,      5, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "sub"),/* SUB r/m, imm */
    OPC(0x83, NO_OP2,      5, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "sub"),/* SUB r/m, imm8 */

    OPC(0x30, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "xor"),/* XOR r/m8, r8 */
    OPC(0x31, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "xor"),/* XOR r/m, r */
    OPC(0x32, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "xor"),/* XOR r8, r/m8 */
    OPC(0x33, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "xor"),/* XOR r, r/m */
    OPC(0x34, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "xor"),/* XOR AL, imm8 */
    OPC(0x35, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "xor"),/* XOR rAX, imm */
    OPC(0x80, NO_OP2,      6, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "xor"),/* XOR r/m8, imm8 */
    OPC(0x81, NO_OP2,      6, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "xor"),/* XOR r/m, imm */
    OPC(0x83, NO_OP2,      6, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "xor"),/* XOR r/m, imm8 */

    OPC(0x38, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "cmp"),/* CMP r/m8, r8 */
    OPC(0x39, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "cmp"),/* CMP r/m, r */
    OPC(0x3A, NO_OP2, NO_REG, DEC_RM, IW_NONE, 1, 0, 0, PFX_NONE, "cmp"),/* CMP r8, r/m8 */
    OPC(0x3B, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "cmp"),/* CMP r, r/m */
    OPC(0x3C, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "cmp"),/* CMP AL, imm8 */
    OPC(0x3D, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "cmp"),/* CMP rAX, imm */
    OPC(0x80, NO_OP2,      7, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "cmp"),/* CMP r/m8, imm8 */
    OPC(0x81, NO_OP2,      7, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "cmp"),/* CMP r/m, imm */
    OPC(0x83, NO_OP2,      7, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "cmp"),/* CMP r/m, imm8 */

    /* TEST */
    OPC(0x84, NO_OP2, NO_REG, DEC_MR, IW_NONE, 1, 0, 0, PFX_NONE, "test"),/* TEST r/m8, r8 */
    OPC(0x85, NO_OP2, NO_REG, DEC_MR, IW_NONE, 0, 0, 0, PFX_NONE, "test"),/* TEST r/m, r */
    OPC(0xF6, NO_OP2,      0, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "test"),/* TEST r/m8, imm8 */
    OPC(0xF7, NO_OP2,      0, DEC_MI, IW_32,   0, 0, 0, PFX_NONE, "test"),/* TEST r/m, imm */
    OPC(0xA8, NO_OP2, NO_REG, DEC_AI, IW_8,    1, 0, 0, PFX_NONE, "test"),/* TEST AL, imm8 */
    OPC(0xA9, NO_OP2, NO_REG, DEC_AI, IW_FULL, 0, 0, 0, PFX_NONE, "test"),/* TEST rAX, imm */

    OPC(0xF6, NO_OP2,      2, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "not"),/* NOT r/m8 */
    OPC(0xF7, NO_OP2,      2, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "not"),/* NOT r/m */

    OPC(0xF6, NO_OP2,      3, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "neg"),/* NEG r/m8 */
    OPC(0xF7, NO_OP2,      3, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "neg"),/* NEG r/m */

    OPC(0xF6, NO_OP2,      4, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "mul"),/* MUL r/m8 */
    OPC(0xF7, NO_OP2,      4, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "mul"),/* MUL r/m */

    OPC(0xF6, NO_OP2,      5, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "imul"),/* IMUL r/m8 */
    OPC(0xF7, NO_OP2,      5, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "imul"),/* IMUL r/m */

    OPC(0xF6, NO_OP2,      6, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "div"),/* DIV r/m8 */
    OPC(0xF7, NO_OP2,      6, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "div"),/* DIV r/m */

    OPC(0xF6, NO_OP2,      7, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "idiv"),/* IDIV r/m8 */
    OPC(0xF7, NO_OP2,      7, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "idiv"),/* IDIV r/m */

    OPC(0xFE, NO_OP2,      0, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "inc"),/* INC r/m8 */
    OPC(0xFF, NO_OP2,      0, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "inc"),/* INC r/m */

    OPC(0xFE, NO_OP2,      1, DEC_M,  IW_NONE, 1, 0, 0, PFX_NONE, "dec"),/* DEC r/m8 */
    OPC(0xFF, NO_OP2,      1, DEC_M,  IW_NONE, 0, 0, 0, PFX_NONE, "dec"),/* DEC r/m */

    OPC(0xD0, NO_OP2,      4, DEC_M1, IW_NONE, 1, 0, 0, PFX_NONE, "shl"),/* SHL r/m8, 1 */
    OPC(0xD1, NO_OP2,      4, DEC_M1, IW_NONE, 0, 0, 0, PFX_NONE, "shl"),/* SHL r/m, 1 */
    OPC(0xC0, NO_OP2,      4, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "shl"),/* SHL r/m8, imm8 */
    OPC(0xC1, NO_OP2,      4, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "shl"),/* SHL r/m, imm8 */
    OPC(0xD2, NO_OP2,      4, DEC_MC, IW_NONE, 1, 0, 0, PFX_NONE, "shl"),/* SHL r/m8, CL */
    OPC(0xD3, NO_OP2,      4, DEC_MC, IW_NONE, 0, 0, 0, PFX_NONE, "shl"),/* SHL r/m, CL */

    OPC(0xD0, NO_OP2,      5, DEC_M1, IW_NONE, 1, 0, 0, PFX_NONE, "shr"),/* SHR r/m8, 1 */
    OPC(0xD1, NO_OP2,      5, DEC_M1, IW_NONE, 0, 0, 0, PFX_NONE, "shr"),/* SHR r/m, 1 */
    OPC(0xC0, NO_OP2,      5, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "shr"),/* SHR r/m8, imm8 */
    OPC(0xC1, NO_OP2,      5, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "shr"),/* SHR r/m, imm8 */
    OPC(0xD2, NO_OP2,      5, DEC_MC, IW_NONE, 1, 0, 0, PFX_NONE, "shr"),/* SHR r/m8, CL */
    OPC(0xD3, NO_OP2,      5, DEC_MC, IW_NONE, 0, 0, 0, PFX_NONE, "shr"),/* SHR r/m, CL */

    OPC(0xD0, NO_OP2,      7, DEC_M1, IW_NONE, 1, 0, 0, PFX_NONE, "sar"),/* SAR r/m8, 1 */
    OPC(0xD1, NO_OP2,      7, DEC_M1, IW_NONE, 0, 0, 0, PFX_NONE, "sar"),/* SAR r/m, 1 */
    OPC(0xC0, NO_OP2,      7, DEC_MI, IW_8,    1, 0, 0, PFX_NONE, "sar"),/* SAR r/m8, imm8 */
    OPC(0xC1, NO_OP2,      7, DEC_MI, IW_8,    0, 0, 0, PFX_NONE, "sar"),/* SAR r/m, imm8 */
    OPC(0xD2, NO_OP2,      7, DEC_MC, IW_NONE, 1, 0, 0, PFX_NONE, "sar"),/* SAR r/m8, CL */
    OPC(0xD3, NO_OP2,      7, DEC_MC, IW_NONE, 0, 0, 0, PFX_NONE, "sar"),/* SAR r/m, CL */

    OPC(0xE8, NO_OP2, NO_REG, DEC_D,  IW_32,   4, 0, 0, PFX_NONE, "call"),/* CALL rel32 */
    OPC(0xFF, NO_OP2,      2, DEC_M,  IW_NONE, 8, 0, 0, PFX_NONE, "call"),/* CALL r/m */

    OPC(0xE9, NO_OP2, NO_REG, DEC_D,  IW_32,   4, 0, 0, PFX_NONE, "jmp"),/* JMP rel32 */
    OPC(0xEB, NO_OP2, NO_REG, DEC_D,  IW_8,    1, 0, 0, PFX_NONE, "jmp"),/* JMP rel8 */
    OPC(0xFF, NO_OP2,      4, DEC_M,  IW_NONE, 8, 0, 0, PFX_NONE, "jmp"),/* JMP r/m */

    OPC(0x70, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jo"),  /* JO rel8 */
    OPC(0x71, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jno"), /* JNO rel8 */
    OPC(0x72, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jb"),  /* JB rel8 */
    OPC(0x73, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jae"), /* JAE rel8 */
    OPC(0x74, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "je"),  /* JE rel8 */
    OPC(0x75, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jne"), /* JNE rel8 */
    OPC(0x76, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jbe"), /* JBE rel8 */
    OPC(0x77, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "ja"),  /* JA rel8 */
    OPC(0x78, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "js"),  /* JS rel8 */
    OPC(0x79, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jns"), /* JNS rel8 */
    OPC(0x7A, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jp"),  /* JP rel8 */
    OPC(0x7B, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jnp"), /* JNP rel8 */
    OPC(0x7C, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jl"),  /* JL rel8 */
    OPC(0x7D, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jge"), /* JGE rel8 */
    OPC(0x7E, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jle"), /* JLE rel8 */
    OPC(0x7F, NO_OP2, NO_REG, DEC_D, IW_8,  1, 0, 0, PFX_NONE, "jg"),  /* JG rel8 */

    OPC(0x98, NO_OP2, NO_REG, DEC_ZO, IW_NONE, 0, 0, 0, PFX_NONE, "cwde"), /* CWDE/CDQE */
    OPC(0x99, NO_OP2, NO_REG, DEC_ZO, IW_NONE, 0, 0, 0, PFX_NONE, "cdq"),  /* CDQ */
    OPC(0x99, NO_OP2, NO_REG, DEC_ZO, IW_NONE, 0, 0, OPCF_REXW, PFX_NONE, "cqo"),  /* CQO (REX.W) */

    OPC(0xC3, NO_OP2, NO_REG, DEC_ZO, IW_NONE, 0, 0, 0, PFX_NONE, "ret"),/* RET */
    OPC(0xC2, NO_OP2, NO_REG, DEC_I,  IW_16,   2, 0, 0, PFX_NONE, "ret"),/* RET imm16 */

    OPC(0x63, NO_OP2, NO_REG, DEC_RM, IW_NONE, 0, 4, OPCF_REXW, PFX_NONE, "movsx"),/* MOVSX r64, r/m32 */
};

/* Two-byte (0F xx) opcode table */
static const OpcEntry opc2_table[] =
{
    OPC(0x80, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jo"),    /* JO rel32 */
    OPC(0x81, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jno"),   /* JNO rel32 */
    OPC(0x82, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jb"),    /* JB rel32 */
    OPC(0x83, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jae"),   /* JAE rel32 */
    OPC(0x84, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "je"),    /* JE rel32 */
    OPC(0x85, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jne"),   /* JNE rel32 */
    OPC(0x86, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jbe"),   /* JBE rel32 */
    OPC(0x87, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "ja"),    /* JA rel32 */
    OPC(0x88, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "js"),    /* JS rel32 */
    OPC(0x89, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jns"),   /* JNS rel32 */
    OPC(0x8A, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jp"),    /* JP rel32 */
    OPC(0x8B, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jnp"),   /* JNP rel32 */
    OPC(0x8C, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jl"),    /* JL rel32 */
    OPC(0x8D, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jge"),   /* JGE rel32 */
    OPC(0x8E, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jle"),   /* JLE rel32 */
    OPC(0x8F, 0x0F, NO_REG, DEC_D, IW_32, 4, 0, 0, PFX_NONE, "jg"),    /* JG rel32 */

    OPC(0x90, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "seto"),    /* SETO r/m8 */
    OPC(0x91, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setno"),   /* SETNO r/m8 */
    OPC(0x92, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setb"),    /* SETB r/m8 */
    OPC(0x93, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setae"),   /* SETAE r/m8 */
    OPC(0x94, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "sete"),    /* SETE r/m8 */
    OPC(0x95, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setne"),   /* SETNE r/m8 */
    OPC(0x96, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setbe"),   /* SETBE r/m8 */
    OPC(0x97, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "seta"),    /* SETA r/m8 */
    OPC(0x98, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "sets"),    /* SETS r/m8 */
    OPC(0x99, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setns"),   /* SETNS r/m8 */
    OPC(0x9A, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setp"),    /* SETP r/m8 */
    OPC(0x9B, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setnp"),   /* SETNP r/m8 */
    OPC(0x9C, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setl"),    /* SETL r/m8 */
    OPC(0x9D, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setge"),   /* SETGE r/m8 */
    OPC(0x9E, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setle"),   /* SETLE r/m8 */
    OPC(0x9F, 0x0F, NO_REG, DEC_M, IW_NONE, 1, 0, 0, PFX_NONE, "setg"),    /* SETG r/m8 */

    OPC(0xBE, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 1, 0, PFX_NONE, "movsx"),/* MOVSX r, r/m8 */
    OPC(0xBF, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 2, 0, PFX_NONE, "movsx"),/* MOVSX r, r/m16 */

    OPC(0xB6, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 1, 0, PFX_NONE, "movzx"),/* MOVZX r, r/m8 */
    OPC(0xB7, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 2, 0, PFX_NONE, "movzx"),/* MOVZX r, r/m16 */

    OPC(0xAF, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 0, 0, PFX_NONE, "imul"),/* IMUL r, r/m */

    OPC(0x05, 0x0F, NO_REG, DEC_ZO, IW_NONE, 0, 0, 0, PFX_NONE, "syscall"),/* SYSCALL */
    OPC(0x31, 0x0F, NO_REG, DEC_ZO, IW_NONE, 0, 0, 0, PFX_NONE, "rdtsc"),  /* RDTSC */
    OPC(0xA2, 0x0F, NO_REG, DEC_ZO, IW_NONE, 0, 0, 0, PFX_NONE, "cpuid"),  /* CPUID */

    OPC(0x10, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "movss"),/* MOVSS xmm, xmm/m32 */
    OPC(0x11, 0x0F, NO_REG, DEC_MR, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "movss"),/* MOVSS xmm/m32, xmm */

    OPC(0x10, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "movsd"),/* MOVSD xmm, xmm/m64 */
    OPC(0x11, 0x0F, NO_REG, DEC_MR, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "movsd"),/* MOVSD xmm/m64, xmm */

    OPC(0x58, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "addss"),/* ADDSS xmm, xmm/m32 */
    OPC(0x58, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "addsd"),/* ADDSS xmm, xmm/m64 */
    OPC(0x5C, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "subss"),/* SUBSS xmm, xmm/m32 */
    OPC(0x5C, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "subsd"),/* SUBSD xmm, xmm/m64 */
    OPC(0x59, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "mulss"),/* MULSS xmm, xmm/m32 */
    OPC(0x59, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "mulsd"),/* MULSD xmm, xmm/m64 */
    OPC(0x5E, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "divss"),/* DIVSS xmm, xmm/m32 */
    OPC(0x5E, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "divsd"),/* DIVSD xmm, xmm/m64 */

    OPC(0x57, 0x0F, NO_REG, DEC_RM, IW_NONE, 16, 0, OPCF_XMM|OPCF_SSE, PFX_NONE, "xorps"),  /* XORPS xmm, xmm/m128 */
    OPC(0x57, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM|OPCF_SSE, PFX_66, "xorpd"),/* XORPD xmm, xmm/m12 */

    OPC(0x2F, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM|OPCF_SSE, PFX_NONE, "comiss"),      /* COMISS xmm, xmm/m32 */
    OPC(0x2F, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM|OPCF_SSE, PFX_66, "comisd"),   /* COMISD xmm, xmm/m64 */
    OPC(0x2E, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM|OPCF_SSE, PFX_NONE, "ucomiss"),     /* UCOMISS xmm, xmm/m32 */
    OPC(0x2E, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM|OPCF_SSE, PFX_66, "ucomisd"),  /* UCOMISD xmm, xmm/m64 */

    OPC(0x2C, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM_SRC, PFX_F3, "cvttss2si"),/* CVTTSS2SI r, xmm/m32 */
    OPC(0x2C, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM_SRC, PFX_F2, "cvttsd2si"),/* CVTTSD2SI r, xmm/m64 */
    OPC(0x2A, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM_DST, PFX_F3, "cvtsi2ss"), /* CVTSI2SS xmm, r/m32 */
    OPC(0x2A, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM_DST, PFX_F2, "cvtsi2sd"), /* CVTSI2SD xmm, r/m64 */
    OPC(0x5A, 0x0F, NO_REG, DEC_RM, IW_NONE, 4, 0, OPCF_XMM, PFX_F3, "cvtss2sd"),     /* CVTSS2SD xmm, xmm/m32 */
    OPC(0x5A, 0x0F, NO_REG, DEC_RM, IW_NONE, 8, 0, OPCF_XMM, PFX_F2, "cvtsd2ss"),     /* CVTSD2SS xmm, xmm/m64 */

    OPC(0x6E, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 0, OPCF_XMM_DST, PFX_66, "movd"),/* MOVD xmm, r/m32 */
    OPC(0x6E, 0x0F, NO_REG, DEC_RM, IW_NONE, 0, 0, OPCF_XMM_DST|OPCF_REXW, PFX_66, "movq"), /* MOVQ xmm, r/m64 */
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
        uint8_t sib = cur_u8(c);
        uint8_t ss = (sib >> 6) & 3;
        uint8_t sidx = (sib >> 3) & 7;
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
                        uint8_t opsz, uint8_t src_size, uint8_t flags,
                        uint8_t xmm_size,
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

    if(flags & OPCF_XMM_DST)
        *dst_opr = dopr_xmm(rnum, xmm_size ? xmm_size : rsz);
    else
        *dst_opr = dopr_reg(rnum, rsz, rex);

    if(mod == 3)
    {
        /* register operand */
        if(flags & OPCF_XMM_SRC)
            *src_opr = dopr_xmm(rmnum, xmm_size ? xmm_size : msz);
        else
            *src_opr = dopr_reg(rmnum, msz, rex);
    }
    else
    {
        int8_t base; int8_t idx; uint8_t scale; int32_t disp;
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
                                  uint8_t op, const uint8_t* rest, uint32_t rest_len,
                                  uint8_t rex_w, uint8_t pfx)
{
    uint8_t modrm_reg = 0xFF;
    if(rest_len > 0) modrm_reg = (rest[0] >> 3) & 7;

    const OpcEntry* fallback = NULL;
    for(uint32_t i = 0; i < count; i++)
    {
        const OpcEntry* e = &table[i];
        uint8_t cmp_op = (e->dec == DEC_OI && e->reg_field == NO_REG)
                            ? (op & 0xF8) : op;
        if(e->op1 != cmp_op) continue;
        if(e->reg_field != NO_REG && e->reg_field != modrm_reg) continue;
        if(e->flags & OPCF_SSE)
        {
            if(e->pfx != pfx) continue;  /* SSE: exact prefix match */
        }
        else
        {
            if(e->pfx != PFX_NONE && e->pfx != pfx) continue;  /* non-SSE: ignore irrelevant prefix */
        }
        uint8_t need_rexw = (e->flags & OPCF_REXW) ? 1 : 0;
        if(need_rexw == rex_w) return e;    /* exact match */
        if(!need_rexw) fallback = e;        /* rex_w=0 entry usable as fallback */
    }
    return fallback;
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
    uint8_t pfxF3 = 0;  /* REP / SSE scalar single */

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
        /* Compute mandatory prefix for lookup */
        uint8_t pfx = pfxF3 ? PFX_F3 : pfxF2 ? PFX_F2 : pfx66 ? PFX_66 : PFX_NONE;

        e = opc_lookup(opc2_table, OPC2_COUNT, op, c.p + c.pos, len - c.pos, rex_w, pfx);
    }
    else
    {
        e = opc_lookup(opc1_table, OPC1_COUNT, op, c.p + c.pos, len - c.pos, rex_w, PFX_NONE);
    }

    if(!e) return 0;

    /* Override opsz from table if entry has explicit size */
    if(e->opr_size) opsz = e->opr_size;
    /* REX.W always wins for 64-bit unless opr_size is fixed */
    else if(rex_w) opsz = 8;
    else if(pfx66) opsz = 2;

    /* For mixed GPR/XMM CVT instructions (exactly one XMM side), opr_size encodes the
    ** XMM scalar width (4/8), while the GPR size is governed by REX.W independently.
    ** Separate them: opsz -> GPR size, xmm_size -> XMM size. */
    uint8_t xmm_size = 0;
    if(e->flags == OPCF_XMM_DST || e->flags == OPCF_XMM_SRC)
    {
        xmm_size = e->opr_size; /* 4 = ss, 8 = sd */
        opsz = rex_w ? 8 : 4;  /* GPR size from REX.W; default 32-bit */
    }

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
        if(!decode_modrm(&c, rex, rex_r, rex_b, rex_x, opsz, e->src_size, e->flags, xmm_size, &dst, &src))
            return 0;
        inst->ops[0] = dst;
        inst->ops[1] = src;
        inst->nops = 2;
        break;
    }
    case DEC_MR:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, rex_r, rex_b, rex_x, opsz, e->src_size, e->flags, xmm_size, &reg_opr, &rm_opr))
            return 0;
        inst->ops[0] = rm_opr;
        inst->ops[1] = reg_opr;
        inst->nops = 2;
        break;
    }
    case DEC_MI:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, 0, rex_b, rex_x, opsz, 0, 0, 0, &reg_opr, &rm_opr))
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
            if(opsz == 2)
            {
            if(!cur_ok(&c, 2)) return 0;
                imm = (int64_t)(int16_t)cur_u16(&c);
            }
            else if(opsz == 8)
            {
                /* REX.W: sign-extend 32-bit immediate to 64 bits */
                if(!cur_ok(&c, 4)) return 0;
                    imm = (int64_t)cur_i32(&c);
            }
            else
            {
                /* 32-bit operand size: zero-extend; no sign extension into imm */
                if(!cur_ok(&c, 4)) return 0;
                imm = (int64_t)(uint32_t)cur_i32(&c);
            }
            break;
        case IW_FULL:
            switch(opsz)
            {
            case 1: if(!cur_ok(&c,1)) return 0; imm=(int64_t)cur_i8(&c); break;
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
        if(!decode_modrm(&c, rex, 0, rex_b, rex_x, opsz, 0, e->flags, 0, &reg_opr, &rm_opr))
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
    case DEC_AI:
    {
        /* dst is always AL/AX/EAX/RAX (reg 0), no ModRM */
        inst->ops[0] = dopr_reg(0, opsz, rex);

        int64_t imm = 0;
        switch((ImmWidth)e->imm_width)
        {
        case IW_8:
            if(!cur_ok(&c, 1)) return 0;
            imm = (int64_t)cur_i8(&c);
            break;
        case IW_FULL:
            switch(opsz)
            {
            case 1: if(!cur_ok(&c,1)) return 0; imm=(int64_t)cur_i8(&c);  break;
            case 2: if(!cur_ok(&c,2)) return 0; imm=(int64_t)(int16_t)cur_u16(&c); break;
            case 4: if(!cur_ok(&c,4)) return 0; imm=(int64_t)cur_i32(&c); break;
            /* Note: even with REX.W the immediate is still sign-extended 32-bit */
            case 8: if(!cur_ok(&c,4)) return 0; imm=(int64_t)cur_i32(&c); break;
            }
            break;
        default: break;
        }
        DecOpr io; io.kind = DOPR_IMM; io.size = opsz; io.imm = imm;
        inst->ops[1] = io;
        inst->nops = 2;
        break;
    }
    case DEC_M1:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, 0, rex_b, rex_x, opsz, 0, 0, 0, &reg_opr, &rm_opr))
            return 0;
        inst->ops[0] = rm_opr;
        DecOpr io; io.kind = DOPR_IMM; io.size = 1; io.imm = 1;
        inst->ops[1] = io;
        inst->nops = 2;
        break;
    }
    case DEC_MC:
    {
        DecOpr reg_opr, rm_opr;
        if(!decode_modrm(&c, rex, 0, rex_b, rex_x, opsz, 0, 0, 0, &reg_opr, &rm_opr))
            return 0;
        inst->ops[0] = rm_opr;
        DecOpr cl; cl.kind = DOPR_REG; cl.size = 1; cl.r.reg = 1; cl.r.hireg = 0;
        inst->ops[1] = cl;
        inst->nops = 2;
        break;
    }
    default:
        vp_assertX(false, "bad operand decoder");
        return 0;
    }

    inst->len = (uint8_t)c.pos;
    return 1;
}