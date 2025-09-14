/*
** vp_asm_x64.c
** x64 inline assembler
*/

#include "vp_asm.h"
#include "vp_err.h"
#include "vp_lex.h"
#include "vp_str.h"

typedef enum RawOpCode
{
    R_NONE,
    R_MOV,
    R_RDTSC,
    R_CPUID,
    R_RET,
} RawOpCode;

const char* opcode[] = {
    "mov",
    "rdtsc",
    "cpuid",
    "ret",
};

const RegInfo regs[] = {
    /* 8-bit */
    { "al", AL }, { "cl", CL }, { "dl", DL }, { "bl", BL },
    { "ah", AH }, { "ch", CH }, { "dh", DH }, { "bh", BH },
    { "r8b", R8B }, { "r9b", R9B }, { "r10b", R10B }, { "r11b", R11B },
    { "r12b", R12B }, { "r13b", R13B }, { "r14b", R14B }, { "r15b", R15B },
    { "spl", SPL }, { "bpl", BPL }, { "sil", SIL }, { "dil", DIL },

    /* 16-bit */
    { "ax", AX }, { "cx", CX }, { "dx", DX }, { "bx", BX },
    { "sp", SP }, { "bp", BP }, { "si", SI }, { "di", DI },
    { "r8w", R8W }, { "r9w", R9W }, { "r10w", R10W }, { "r11w", R11W },
    { "r12w", R12W }, { "r13w", R13W }, { "r14w", R14W }, { "r15w", R15W },

    /* 32-bit */
    { "eax", EAX }, { "ecx", ECX }, { "edx",EDX }, { "ebx", EBX },
    { "esp", ESP }, { "ebp", EBP }, { "esi", ESI }, { "edi", EDI },
    { "r8d", R8D }, { "r9d", R9D }, { "r10d", R10D }, { "r11d", R11D },
    { "r12d", R12D }, { "r13d", R13D }, { "r14d", R14D }, { "r15d", R15D },

    /* 64-bit */
    { "rax", xRAX }, { "rcx", xRCX }, { "rdx", xRDX }, { "rbx", xRBX },
    { "rsp", xRSP }, { "rbp", xRBP }, { "rsi", xRSI }, { "rdi", xRDI },
    { "r8", xR8 }, { "r9", xR9 }, { "r10", xR10 }, { "r11", xR11 },
    { "r12", xR12 }, { "r13", xR13 }, { "r14", xR14 }, { "r15", xR15 },

    { "rip", xRIP },
};

#define R8 (1 << 0)
#define R16 (1 << 1)
#define R32 (1 << 2)
#define R64 (1 << 3)
#define IMM (1 << 4)    /* Immediate */
#define IND (1 << 5)    /* Indirect */

typedef struct
{
    uint32_t op;
    uint32_t flags[4 + 1];
} OpArray;

typedef struct
{
    uint32_t count;
    const OpArray** arr;
} InstTable;

const InstTable insttab[] = {
    [R_MOV] = {6, (const OpArray*[]){
        &(OpArray){MOV_RR, {R8, R8}},    /* mov reg8, reg8 */
        &(OpArray){MOV_RR, {R16, R16}},  /* mov reg16, reg16 */
        &(OpArray){MOV_RR, {R32, R32}},  /* mov reg32, reg32 */
        &(OpArray){MOV_RR, {R64, R64}},  /* mov reg64, reg64 */
        &(OpArray){MOV_RI, {R8 | R16 | R32 | R64, IMM}},  /* mov reg, imm */
        &(OpArray){MOV_RM, {R8 | R16 | R32 | R64, IND}},  /* mov reg, [mem] */
        &(OpArray){MOV_MR, {IND, R8 | R16 | R32 | R64}},  /* mov [mem], reg */
        }
    },
    [R_RDTSC] = { 1, (const OpArray*[]){
        &(OpArray){RDTSC, {}},  /* rdtsc */
        }
    },
    [R_CPUID] = { 1, (const OpArray*[]){
        &(OpArray){CPUID, {}},  /* cpuid */
        }
    },
    [R_RET] = { 1, (const OpArray*[]){
        &(OpArray){RET, {}},  /* ret */
        }
    },
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
    if(reg >= AL && reg <= DIL) return R8;
    if(reg >= AX && reg <= R15W) return R16;
    if(reg >= EAX && reg <= R15D) return R32;
    if(reg >= xRAX && reg <= xR15) return R64;
    if(reg == xRIP) return R64;
    vp_assertX(0, "?");
    return 0;
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
        if(!((reg >= xRAX && reg <= xR15) || (reg >= EAX && reg <= R15D)))
        {
            vp_err_error(lex_srcloc(ls), "register '%s' cannot be used for addressing", str_data(name));
        }

        base = (uint8_t)reg;

        if(reg == xRIP)
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
                if(!((ireg >= xRAX && ireg <= xR15) || (ireg >= EAX && ireg <= R15D)))
                {
                    vp_err_error(lex_srcloc(ls), "register '%s' cannot be used for addressing", str_data(name));
                }

                /* Cannot use RSP as index register */
                if(ireg == xRSP || ireg == ESP)
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
            *flags = IMM;
            break;
        }
        case '[':
        {
            /* Memory operand */
            asm_mem(ls, opr);
            *flags = IND;
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

/* Parse an x64 instruction */
static Inst* asm_inst(LexState* ls)
{
    Str* s = lex_name(ls);
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
    vec_t(Inst*) vec = NULL;
    while(!lex_check(ls, '}') && !lex_check(ls, TK_eof))
    {
        Inst* inst = asm_inst(ls);
        vec_push(vec, inst);
    }
    return vec;
}