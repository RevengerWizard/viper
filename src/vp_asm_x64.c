/*
** vp_asm_x64.c
** x64 inline assembler
*/

#include "vp_asm.h"
#include "vp_err.h"
#include "vp_lex.h"
#include "vp_str.h"
#include "vp_target_x64.h"

typedef enum RawOpCode
{
    R_NONE,
    R_MOV, R_MOVSX, R_MOVZX, R_LEA,
    R_PUSH, R_POP,
    R_CWDE, R_CDQ, R_CQO,
    R_ADD, R_SUB, R_MUL, R_DIV, R_IDIV,
    R_INC, R_DEC, R_NEG,
    R_AND, R_OR, R_XOR, R_NOT,
    R_TEST, R_CMP,
    R_SHL, R_SHR, R_SAR,
    /* SSE */
    R_MOVSS, R_MOVSD,
    R_ADDSS, R_ADDSD, R_SUBSS, R_SUBSD,
    R_MULSS, R_MULSD, R_DIVSS, R_DIVSD,
    R_XORPS, R_XORPD,
    R_COMISS, R_COMISD, R_UCOMISS, R_UCOMISD,
    R_CVTTSS2SI, R_CVTTSD2SI,
    R_CVTSI2SS, R_CVTSI2SD,
    R_CVTSS2SD, R_CVTSD2SS,
    R_RDTSC, R_CPUID, R_RET, R_SYSCALL
} RawOpCode;

const char* opcode[] = {
    "mov", "movsx", "movzx", "lea",
    "push", "pop",
    "cwde", "cdq", "cqo",
    "add", "sub", "mul", "div", "idiv",
    "inc", "dec", "neg",
    "and", "or", "xor", "not",
    "test", "cmp",
    "shl", "shr", "sar",
    /* SSE */
    "movss", "movsd",
    "addss", "addsd", "subss", "subsd",
    "mulss", "mulsd", "divss", "divsd",
    "xorps", "xorpd",
    "comiss", "comisd", "ucomiss", "ucomisd",
    "cvttss2si", "cvttsd2si",
    "cvtsi2ss", "cvtsi2sd",
    "cvtss2sd", "cvtsd2ss",
    "rdtsc", "cpuid", "ret", "syscall"
};

const RegInfo regs[] = {
    /* 8-bit */
    {"al", AL}, {"cl", CL}, {"dl", DL}, {"bl", BL},
    {"ah", AH}, {"ch", CH}, {"dh", DH}, {"bh", BH},
    {"r8b", R8B}, {"r9b", R9B}, {"r10b", R10B}, { "r11b", R11B},
    {"r12b", R12B}, {"r13b", R13B}, {"r14b", R14B}, {"r15b", R15B},
    {"spl", SPL }, {"bpl", BPL}, {"sil", SIL}, {"dil", DIL},

    /* 16-bit */
    {"ax", AX}, {"cx", CX}, {"dx", DX}, {"bx", BX},
    {"sp", SP}, {"bp", BP}, {"si", SI}, { "di", DI},
    {"r8w", R8W}, {"r9w", R9W}, {"r10w", R10W}, {"r11w", R11W},
    {"r12w", R12W}, {"r13w", R13W}, {"r14w", R14W}, {"r15w", R15W},

    /* 32-bit */
    {"eax", EAX}, {"ecx", ECX}, {"edx",EDX}, {"ebx", EBX},
    {"esp", ESP}, {"ebp", EBP}, {"esi", ESI}, {"edi", EDI},
    {"r8d", R8D}, {"r9d", R9D}, {"r10d", R10D}, { "r11d", R11D},
    {"r12d", R12D}, {"r13d", R13D}, {"r14d", R14D}, {"r15d", R15D},

    /* 64-bit */
    {"rax", RAX}, {"rcx", RCX}, {"rdx", RDX}, {"rbx", RBX},
    {"rsp", RSP}, {"rbp", RBP}, {"rsi", RSI}, {"rdi", RDI},
    {"r8", R8}, {"r9", R9}, {"r10", R10}, {"r11", R11},
    {"r12", R12}, {"r13", R13}, {"r14", R14}, {"r15", R15},

    {"rip", RIP},
};

static RegInfo xregs[] = {
    /* 128-bit */
    {"xmm0", XMM0}, {"xmm1", XMM1}, {"xmm2", XMM2}, {"xmm3", XMM3},
    {"xmm4", XMM4}, {"xmm5", XMM5}, {"xmm6", XMM6}, {"xmm7", XMM7},
    {"xmm8", XMM8}, {"xmm9", XMM9}, {"xmm10", XMM10}, {"xmm11", XMM11},
    {"xmm12", XMM12}, {"xmm13", XMM13}, {"xmm14", XMM14}, {"xmm15", XMM15}
};

/* Flags */
enum
{
    F_R8 = 1 << 0,
    F_R16 = 1 << 1,
    F_R32 = 1 << 2,
    F_R64 = 1 << 3,
    F_IMM = 1 << 4,    /* Immediate */
    F_IND = 1 << 5,    /* Indirect */
    F_XMM = 1 << 6,
};

#define F_GP (F_R8 | F_R16 | F_R32 | F_R64)

typedef struct
{
    uint32_t op;
    uint32_t flags[4 + 1];  /* +1 for terminator */
} OpArray;

typedef struct
{
    uint32_t count;
    const OpArray** arr;
} InstTable;

const InstTable insttab[] = {
    [R_MOV] = {6, (const OpArray*[]){
        &(OpArray){MOV_RR, {F_R8, F_R8}},    /* mov r8, r8 */
        &(OpArray){MOV_RR, {F_R16, F_R16}},  /* mov r16, r16 */
        &(OpArray){MOV_RR, {F_R32, F_R32}},  /* mov r32, r32 */
        &(OpArray){MOV_RR, {F_R64, F_R64}},  /* mov r64, r64 */
        &(OpArray){MOV_RI, {F_GP, F_IMM}},  /* mov reg, imm */
        &(OpArray){MOV_RM, {F_GP, F_IND}},  /* mov reg, [mem] */
        &(OpArray){MOV_MR, {F_GP}},  /* mov [mem], gpr */
        }
    },
    [R_MOVSX] = {1, (const OpArray*[]){
        &(OpArray){MOVSX_RR, {F_GP, F_GP}}  /* movsx gpr, gpr */
        }
    },
    [R_MOVZX] = {1, (const OpArray*[]){
        &(OpArray){MOVZX_RR, {F_GP, F_GP}}  /* movzx gpr, gpr */
        }
    },
    [R_LEA] = {1, (const OpArray*[]){
        &(OpArray){LEA_RM, {F_R64, F_IND}}  /* lea r64, [gpr] */
        }
    },
    [R_PUSH] = {1, (const OpArray*[]){
        &(OpArray){PUSH_R, {F_R64}}     /* push r64 */
        }
    },
    [R_POP] = {1, (const OpArray*[]){
        &(OpArray){POP_R, {F_R64}}  /* pop r64 */
        }
    },
    [R_CWDE] = {1, (const OpArray*[]){ &(OpArray){CWDE, {}} }},
    [R_CDQ] = {1, (const OpArray*[]){ &(OpArray){CDQ, {}} }},
    [R_CQO] = {1, (const OpArray*[]){ &(OpArray){CQO, {}} }},
    [R_ADD] = {2, (const OpArray*[]){
        &(OpArray){ADD_RR, {F_R8, F_R8}},   /* add r8, r8 */
        &(OpArray){ADD_RR, {F_R16, F_R16}},   /* add r16, r16 */
        &(OpArray){ADD_RR, {F_R32, F_R32}},   /* add r32, r32 */
        &(OpArray){ADD_RR, {F_R64, F_R64}},   /* add r64, r64 */
        &(OpArray){ADD_RI, {F_GP, F_IMM}},  /* add gpr, imm */
    }},
    [R_SUB] = {2, (const OpArray*[]){
        &(OpArray){SUB_RR, {F_R8, F_R8}},   /* sub r8, r8 */
        &(OpArray){SUB_RR, {F_R16, F_R16}},   /* sub r16, r16 */
        &(OpArray){SUB_RR, {F_R32, F_R32}},   /* sub r32, r32 */
        &(OpArray){SUB_RR, {F_R64, F_R64}},   /* sub r64, r64 */
        &(OpArray){SUB_RI, {F_GP, F_IMM}},  /* sub gpr, imm */
    }},
    [R_MUL] = {1, (const OpArray*[]){ &(OpArray){MUL_R, {F_GP}} }},
    [R_DIV] = {1, (const OpArray*[]){ &(OpArray){DIV_R, {F_GP}} }},
    [R_IDIV] = {1, (const OpArray*[]){ &(OpArray){IDIV_R, {F_GP}} }},
    [R_INC] = {1, (const OpArray*[]){ &(OpArray){INC_R, {F_GP}} }},
    [R_DEC] = {1, (const OpArray*[]){ &(OpArray){DEC_R, {F_GP}} }},
    [R_NEG] = {1, (const OpArray*[]){ &(OpArray){NEG_R, {F_GP}} }},
    [R_AND] = {2, (const OpArray*[]){
        &(OpArray){AND_RR, {F_GP, F_GP}},
        &(OpArray){AND_RI, {F_GP, F_IMM}},
    }},
    [R_OR] = {2, (const OpArray*[]){
        &(OpArray){OR_RR, {F_GP, F_GP}},
        &(OpArray){OR_RI, {F_GP, F_IMM}},
    }},
    [R_XOR] = {2, (const OpArray*[]){
        &(OpArray){XOR_RR, {F_GP, F_GP}},
        &(OpArray){XOR_RI, {F_GP, F_IMM}},
    }},
    [R_NOT] = {1, (const OpArray*[]){ &(OpArray){NOT_R, {F_GP}} }},
    [R_TEST] = {1, (const OpArray*[]){ &(OpArray){TEST_RR, {F_GP, F_GP}} }},
    [R_CMP] = {2, (const OpArray*[]){
        &(OpArray){CMP_RR, {F_GP, F_GP}},
        &(OpArray){CMP_RI, {F_GP, F_IMM}},
    }},
    [R_SHL] = {2, (const OpArray*[]){
        &(OpArray){SHL_RI, {F_GP, F_IMM}},
        &(OpArray){SHL_RR, {F_GP, F_R8}},
    }},
    [R_SHR] = {1, (const OpArray*[]){ &(OpArray){SHR_RI, {F_GP, F_IMM}} }},
    [R_SAR] = {1, (const OpArray*[]){ &(OpArray){SAR_RI, {F_GP, F_IMM}} }},
    /* SSE */
    [R_MOVSS] = {3, (const OpArray*[]){
        &(OpArray){MOVSS_XX, {F_XMM, F_XMM}},
        &(OpArray){MOVSS_XM, {F_XMM, F_IND}},
        &(OpArray){MOVSS_MX, {F_IND, F_XMM}},
    }},
    [R_MOVSD] = {3, (const OpArray*[]){
        &(OpArray){MOVSD_XX, {F_XMM, F_XMM}},
        &(OpArray){MOVSD_XM, {F_XMM, F_IND}},
        &(OpArray){MOVSD_MX, {F_IND, F_XMM}},
    }},
    [R_ADDSS] = {1, (const OpArray*[]){ &(OpArray){ADDSS_XX, {F_XMM, F_XMM}} }},
    [R_ADDSD] = {1, (const OpArray*[]){ &(OpArray){ADDSD_XX, {F_XMM, F_XMM}} }},
    [R_SUBSS] = {1, (const OpArray*[]){ &(OpArray){SUBSS_XX, {F_XMM, F_XMM}} }},
    [R_SUBSD] = {1, (const OpArray*[]){ &(OpArray){SUBSD_XX, {F_XMM, F_XMM}} }},
    [R_MULSS] = {1, (const OpArray*[]){ &(OpArray){MULSS_XX, {F_XMM, F_XMM}} }},
    [R_MULSD] = {1, (const OpArray*[]){ &(OpArray){MULSD_XX, {F_XMM, F_XMM}} }},
    [R_DIVSS] = {1, (const OpArray*[]){ &(OpArray){DIVSS_XX, {F_XMM, F_XMM}} }},
    [R_DIVSD] = {1, (const OpArray*[]){ &(OpArray){DIVSD_XX, {F_XMM, F_XMM}} }},
    [R_XORPS] = {1, (const OpArray*[]){ &(OpArray){XORPS_XX, {F_XMM, F_XMM}} }},
    [R_XORPD] = {1, (const OpArray*[]){ &(OpArray){XORPD_XX, {F_XMM, F_XMM}} }},
    [R_COMISS] = {1, (const OpArray*[]){ &(OpArray){COMISS_XX, {F_XMM, F_XMM}} }},
    [R_COMISD] = {1, (const OpArray*[]){ &(OpArray){COMISD_XX, {F_XMM, F_XMM}} }},
    [R_UCOMISS] = {1, (const OpArray*[]){ &(OpArray){UCOMISS_XX, {F_XMM, F_XMM}} }},
    [R_UCOMISD] = {1, (const OpArray*[]){ &(OpArray){UCOMISD_XX, {F_XMM, F_XMM}} }},
    [R_CVTTSS2SI] = {1, (const OpArray*[]){ &(OpArray){CVTTSS2SI_RX, {F_GP, F_XMM}} }},
    [R_CVTTSD2SI] = {1, (const OpArray*[]){ &(OpArray){CVTTSD2SI_RX, {F_GP, F_XMM}} }},
    [R_CVTSI2SS] = {1, (const OpArray*[]){ &(OpArray){CVTSI2SS_XR, {F_XMM, F_GP}} }},
    [R_CVTSI2SD] = {1, (const OpArray*[]){ &(OpArray){CVTSI2SD_XR, {F_XMM, F_GP}} }},
    [R_CVTSS2SD] = {1, (const OpArray*[]){ &(OpArray){CVTSS2SD_XX, {F_XMM, F_XMM}} }},
    [R_CVTSD2SS] = {1, (const OpArray*[]){ &(OpArray){CVTSD2SS_XX, {F_XMM, F_XMM}} }},
    [R_RDTSC] = {1, (const OpArray*[]){ &(OpArray){RDTSC, {}} }},
    [R_CPUID] = {1, (const OpArray*[]){ &(OpArray){CPUID, {}} }},
    [R_RET] = {1, (const OpArray*[]){ &(OpArray){RET, {}} }},
    [R_SYSCALL] = {1, (const OpArray*[]){ &(OpArray){SYSCALL, {}} }},
};

/* Allocate new asm instruction */
static Inst* inst_new(OpCode op, AsmOperand* oprs, uint32_t count)
{
    Inst* inst = vp_arena_alloc(&V->instarena, inst_size(count));
    inst->op = op;
    inst->count = (uint8_t)count;
    memcpy(inst->oprs, oprs, count * sizeof(AsmOperand));
    return inst;
}

/* Get register size from register type */
static uint32_t reg_flags(RegType reg)
{
    switch(REG_SIZE(reg))
    {
    case 1: return F_R8;
    case 2: return F_R16;
    case 4: return F_R32;
    case 8: return F_R64;
    case 16: return F_XMM;
    default: vp_assertX(0, "?"); return 0;
    }
}

/* Find register by name */
static RegType reg_find(const char* name, size_t len)
{
    for(uint32_t i = 0; i < ARRSIZE(regs); i++)
    {
        const RegInfo r = regs[i];
        size_t rlen = strlen(r.name);
        if(rlen == len && memcmp(r.name, name, len) == 0)
        {
            return r.reg;
        }
    }
    return (RegType)-1;
}

/* Find matching instruction variant */
static const OpArray* asm_variant(RawOpCode op, uint32_t* flags, uint32_t count)
{
    uint32_t len = insttab[op].count;
    const OpArray** variants = insttab[op].arr;
    for(uint32_t i = 0; i < len; i++)
    {
        const OpArray* inst = variants[i];
        bool match = true;

        for(uint32_t j = 0; j < count && j < 4; j++)
        {
            if(inst->flags[j] == 0) break;
            if(flags[j] != 0 && (flags[j] & inst->flags[j]) == 0)
            {
                match = false;
                break;
            }
        }
        if(match) return inst;
    }
    return NULL;
}

/* Parse scale factor (1, 2, 4, 8) */
static uint8_t asm_scale(LexState* ls)
{
    if(!lex_match(ls, TK_integer))
    {
        vp_err_error(lex_srcloc(ls), "expected scale factor (1, 2, 4, 8)");
    }

    int64_t scale = ls->val.i64;
    switch(scale)
    {
        case 1: case 2: case 4: case 8:
            return (uint8_t)scale;
        default:
            vp_err_error(lex_srcloc(ls), "invalid scale factor %lld (must be 1, 2, 4, 8)", scale);
    }
    return (uint8_t)-1;
}

/* Parse x64 memory operand */
static void asm_mem(LexState* ls, AsmOperand* opr)
{
    uint8_t base = (uint8_t)-1;
    uint8_t idx = (uint8_t)-1;
    uint8_t scale = 1;
    int32_t disp = 0;

    vp_lex_next(ls);    /* Skip '[' */
    if(lex_check(ls, TK_name))
    {
        Str* name = lex_name(ls);
        RegType reg = reg_find(str_data(name), name->len);
        if(reg == (RegType)-1)
        {
            vp_err_error(lex_srcloc(ls), "unknown register '%s'", str_data(name));
        }

        /* Only 64-bit and 32-bit registers for addressing */
        if(!((REG_SIZE(reg) == 8) && (REG_SIZE(reg) == 4)))
        {
            vp_err_error(lex_srcloc(ls), "register '%s' cannot be used for addressing", str_data(name));
        }

        base = (uint8_t)reg;

        if(reg == RIP)
        {
            /* Parse displacement */
            if(lex_match(ls, '+') || lex_match(ls, '-'))
            {
                bool neg = ls->prev == '-';
                if(!lex_match(ls, TK_integer))
                {
                    vp_err_error(lex_srcloc(ls), "expected displacement value");
                }
                disp = (int32_t)ls->val.i64;
                if(neg) disp = -disp;
            }
            goto end;
        }

        if(lex_match(ls, '+'))
        {
            if(lex_match(ls, TK_name))
            {
                RegType ireg = reg_find(str_data(name), name->len);
                if(ireg != (RegType)-1)
                {
                    vp_err_error(lex_srcloc(ls), "unknown register '%s'", str_data(name));
                }

                /* Only 64-bit and 32-bit registers for addressing */
                if(!((REG_SIZE(reg) == 8) && (REG_SIZE(reg) == 4)))
                {
                    vp_err_error(lex_srcloc(ls), "register '%s' cannot be used for addressing", str_data(name));
                }

                /* Cannot use RSP as index register */
                if(ireg == RSP || ireg == ESP)
                {
                    vp_err_error(lex_srcloc(ls), "RSP/ESP cannot be used as index register");
                }

                idx = (uint8_t)ireg;
            }

            /* Parse scale */
            if(lex_match(ls, '*'))
            {
                scale = asm_scale(ls);
            }

            /* Parse displacement */
            if(lex_match(ls, '+') || lex_match(ls, '-'))
            {
                bool neg = ls->prev == '-';
                if(!lex_match(ls, TK_integer))
                {
                    vp_err_error(lex_srcloc(ls), "expected displacement value");
                }
                disp = (int32_t)ls->val.i64;
                if(neg) disp = -disp;
            }
            else if(lex_match(ls, TK_integer))
            {
                disp = (int32_t)ls->val.i64;
            }
            else
            {
                vp_err_error(lex_srcloc(ls), "expected index register or displacement");
            }
        }
    }
    else
    {
        vp_lex_consume(ls, TK_integer);
        disp = (int32_t)ls->val.i64;
    }
end:
    vp_lex_consume(ls, ']');

    opr->type = ASM_MEM;
    opr->mem.base = base;
    opr->mem.idx = idx;
    opr->mem.scale = scale;
    opr->mem.disp = disp;
}

/* Parse an x64 operand */
static void asm_opr(LexState* ls, AsmOperand* opr, uint32_t* flags)
{
    switch(ls->curr)
    {
        case TK_name:
        {
            Str* name = lex_name(ls);
            /* Register name? */
            RegType reg = reg_find(str_data(name), name->len);
            if(reg != (RegType)-1)
            {
                opr->type = ASM_REG;
                opr->reg = (uint8_t)reg;
                *flags = reg_flags(reg);
                return;
            }
            /* Viper variable */
            opr->type = ASM_NONE; /* For now */
            vp_err_error(lex_srcloc(ls), "unknown operand '%s'", str_data(name));
            break;
        }
        case TK_integer:
        {
            vp_lex_next(ls);
            /* Immediate value */
            opr->type = ASM_IMM;
            opr->imm = ls->val.i64;
            *flags = F_IMM;
            break;
        }
        case '[':
        {
            /* Memory operand */
            asm_mem(ls, opr);
            *flags = F_IND;
            break;
        }
        default:
        {
            vp_err_error(lex_srcloc(ls), "expected operand");
        }
    }
}

/* Find raw x64 opcode */
static RawOpCode asm_opcode(Str* s)
{
    for(uint32_t i = 0; i < ARRSIZE(opcode); i++)
    {
        const char* name = opcode[i];
        size_t len = strlen(name);
        if(s->len == len && memcmp(str_data(s), name, s->len) == 0)
        {
            return i + 1;
        }
    }
    return R_NONE;
}

static Str* asm_consume(LexState* ls)
{
    if(!lex_check(ls, TK_name) &&
        !lex_check(ls, TK_and) && !lex_check(ls, TK_or))
    {
        vp_lex_error(ls, "'<name>' expected");
    }
    vp_lex_next(ls);
    return ls->val.name;
}

/* Parse an x64 instruction */
static Inst* asm_inst(LexState* ls)
{
    Str* s = asm_consume(ls);
    SrcLoc loc = lex_srcloc(ls);
    RawOpCode op = asm_opcode(s);
    if(VP_LIKELY(op != R_NONE))
    {
        AsmOperand oprs[4];
        uint32_t flags[4] = {0};
        uint32_t count = 0;

        /* Parse operands */
        if(!lex_check(ls, ';'))
        {
            do
            {
                if(count >= 4)
                {
                    vp_err_error(loc, "too many operands for instruction '%s'", str_data(s));
                }

                loc = lex_srcloc(ls);
                asm_opr(ls, &oprs[count], &flags[count]);
                count++;
            }
            while(lex_match(ls, ','));
        }
        vp_lex_consume(ls, ';');

        /* Find matching instruction variant */
        const OpArray* variant = asm_variant(op, flags, count);
        if(!variant)
        {
            vp_err_error(loc, "no matching instruction for '%s'", str_data(s));
        }
        return inst_new(variant->op, oprs, count);
    }
    vp_err_error(loc, "unknown opcode '%s'", str_data(s));
    return NULL;
}

vec_t(Inst*) vp_asm_x64(LexState* ls)
{
    vec_t(Inst*) vec = vec_init(Inst*);
    while(!lex_check(ls, '}') && !lex_check(ls, TK_eof))
    {
        Inst* inst = asm_inst(ls);
        vec_push(vec, inst);
    }
    return vec;
}