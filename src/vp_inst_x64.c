/*
** vp_inst_x64.c
** x64 asm instruction handling
*/

#include "vp_asm.h"
#include "vp_emit_x64.h"
#include "vp_target_x64.h"
#include "vp_low.h"

static X64Reg regtype_to_x64reg(RegType reg)
{
    if(reg <= R15B)
    {
        return (X64Reg)((reg > BL) ? (reg - R8B + 8) : reg);
    }
    else if(reg >= SPL && reg <= DIL)
    {
        switch(reg)
        {
            case SPL: return RSP;
            case BPL: return RBP;
            case SIL: return RSI;
            case DIL: return RDI;
            default: vp_assertX(0, "?");
        }
    }
    else if(reg >= AX && reg <= R15W)
    {
        return (X64Reg)(reg - AX);
    }
    else if(reg >= EAX && reg <= R15D)
    {
        return (X64Reg)(reg - EAX);
    }
    else if(reg >= xRAX && reg <= xR15)
    {
        return (X64Reg)(reg - xRAX);
    }
    else if(reg == xRIP)
    {
        return RIP;
    }
    vp_assertX(0, "invalid RegType value");
    return -1;
}

static VRSize regtype_to_vrsize(RegType reg)
{
    if((reg >= AL && reg <= BH) || (reg >= R8B && reg <= DIL)) return VRSize1;
    if((reg >= AX && reg <= DI) || (reg >= R8W && reg <= R15W)) return VRSize2;
    if((reg >= EAX && reg <= EDI) || (reg >= R8D && reg <= R15D)) return VRSize4;
    if((reg >= xRAX && reg <= xR15) || reg == xRIP) return VRSize8;
    vp_assertX(0, "unknown register size for %d", reg);
    return VRSize8;
}

static void inst_mov_rr(Inst* inst)
{
    RegType dst = inst->oprs[0].reg;
    RegType src = inst->oprs[1].reg;
    VRSize size = regtype_to_vrsize(dst);
    vp_assertX(size == regtype_to_vrsize(src), "register size mismatch");

    X64Reg x64_dst = regtype_to_x64reg(dst);
    X64Reg x64_src = regtype_to_x64reg(src);
    emit_mov_rr(size, x64_dst, x64_src);
}

static void inst_or_rr(Inst* inst)
{
    RegType dst = inst->oprs[0].reg;
    RegType src = inst->oprs[1].reg;
    VRSize size = regtype_to_vrsize(dst);
    vp_assertX(size == regtype_to_vrsize(src), "register size mismatch");
    X64Reg x64_dst = regtype_to_x64reg(dst);
    X64Reg x64_src = regtype_to_x64reg(src);
    emit_or_rr(size, x64_dst, x64_src);
}

static void inst_shl_ri(Inst* inst)
{
    RegType reg = inst->oprs[0].reg;
    int64_t imm = inst->oprs[1].imm;
    VRSize size = regtype_to_vrsize(reg);
    X64Reg x64_reg = regtype_to_x64reg(reg);
    emit_shl_ri(size, x64_reg, imm);
}

static void inst_rdtsc(Inst* inst)
{
    UNUSED(inst);
    emit_rdtsc(V);
}

static void inst_cpuid(Inst* inst)
{
    UNUSED(inst);
    emit_cpuid(V);
}

static void inst_ret(Inst* inst)
{
    UNUSED(inst);
    emit_ret(V);
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