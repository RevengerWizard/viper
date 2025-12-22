/*
** vp_inst_x64.c
** x64 asm instruction handling
*/

#include "vp_asm.h"
#include "vp_emit_x64.h"
#include "vp_target_x64.h"
#include "vp_low.h"

static void inst_mov_rr(Inst* inst)
{
    RegType dst = inst->oprs[0].reg;
    RegType src = inst->oprs[1].reg;
    EMITX64(movRR)(dst, src);
}

static void inst_or_rr(Inst* inst)
{
    RegType dst = inst->oprs[0].reg;
    RegType src = inst->oprs[1].reg;
    EMITX64(orRR)(dst, src);
}

static void inst_shl_ri(Inst* inst)
{
    RegType reg = inst->oprs[0].reg;
    int64_t imm = inst->oprs[1].imm;
    EMITX64(shlRI)(reg, imm);
}

static void inst_rdtsc(Inst* inst)
{
    UNUSED(inst);
    EMITX64(rdtsc)();
}

static void inst_cpuid(Inst* inst)
{
    UNUSED(inst);
    EMITX64(cpuid)();
}

static void inst_ret(Inst* inst)
{
    UNUSED(inst);
    EMITX64(ret)();
}

typedef void (*EmitAsmFn)(Inst* inst);
static const EmitAsmFn emitinsttab[] = {
    [MOV_RR] = inst_mov_rr,
    [OR_RR] = inst_or_rr,
    [SHL_RI] = inst_shl_ri,
    [RDTSC] = inst_rdtsc,
    [CPUID] = inst_cpuid,
    [RET] = inst_ret,
};

void vp_inst_x64(Inst* inst)
{
    vp_assertX(inst->op < (int)ARRSIZE(emitinsttab), "out of bounds opcode");
    vp_assertX(emitinsttab[inst->op], "empty entry %d", inst->op);
    (*emitinsttab[inst->op])(inst);
}