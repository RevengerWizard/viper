/*
** vp_ir_x64.c
** IR instruction emitting (IR -> x64)
*/

#include "vp_asm.h"
#include "vp_emit_x64.h"

#include "vp_link.h"
#include "vp_ir.h"
#include "vp_regalloc.h"
#include "vp_state.h"
#include "vp_target.h"
#include "vp_target_x64.h"
#include "vp_vec.h"
#include "vp_low.h"

static const X64Reg irx64R[][X64_IREG] = {
    {AL,  CL,  DL,  BL,  SPL, BPL, SIL, DIL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B},
    {AX,  CX,  DX,  BX,  SP,  BP,  SI,  DI,  R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W},
    {EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D},
    {RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8,  R9,  R10,  R11,  R12,  R13,  R14,  R15}
};

static const X64Reg irx64X[] = {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15
};

static void emit_mov(VReg* dst, VReg* src)
{
    if(vrf_flo(dst))
    {
        vp_assertX(!vrf_const(src), "const src1");
        if(!vrf_flo(src))
        {
            switch(src->vsize)
            {
            case VRSize4: EMITX64(movdXR)(irx64X[dst->phys], irx64X[src->phys]); break;
            case VRSize8: EMITX64(movqXR)(irx64X[dst->phys], irx64X[src->phys]); break;
            default: vp_assertX(0, "?"); break;
            }
        }
        else
        {
            if(src->phys != dst->phys)
            {
                switch(src->vsize)
                {
                case VRSize4: EMITX64(movssXX)(irx64X[dst->phys], irx64X[src->phys]); break;
                case VRSize8: EMITX64(movsdXX)(irx64X[dst->phys], irx64X[src->phys]); break;
                default: vp_assertX(0, "?"); break;
                }
            }
        }
    }
    else
    {
        VRSize p = src->vsize;
        if(vrf_const(src))
        {
            EMITX64(movRI)(irx64R[p][dst->phys], src->i64);
        }
        else
        {
            if(dst->phys != src->phys)
            {
                EMITX64(movRR)(irx64R[p][dst->phys], irx64R[p][src->phys]);
            }
        }
    }
}

static void ir_x64_bofs(IR* ir)
{
    int64_t ofs = ir->bofs.fi->ofs + ir->bofs.ofs;
    EMITX64(leaRM)(irx64R[VRSize8][ir->dst->phys], MEM_MAKE(RN_BP, NOREG, 1, ofs, 8));
}

static void ir_x64_iofs(IR* ir)
{
    Str* label = ir->iofs.label;
    int64_t ofs = ir->iofs.ofs;
    EMITX64(leaRM)(irx64R[VRSize8][ir->dst->phys], MEM_MAKE_RIP(ofs, 8));
    uint32_t patch = sbuf_len(&V->code) - 4;
    if(ir->iofs.isfn)
    {
        Code* code = vp_tab_get(&V->funcs, label);
        vp_assertX(code, "?");
        patchinfo_learel(code, patch);
    }
    else if(ir->iofs.isstr)
    {
        patchinfo_leaabs(label, patch);
    }
}

static void ir_x64_sofs(IR* ir)
{
    int64_t ofs = ir->sofs.ofs;
    EMITX64(leaRM)(irx64R[VRSize8][ir->dst->phys], MEM_MAKE(RN_SP, NOREG, 1, ofs, 8));
}

static void ir_x64_mov(IR* ir)
{
    emit_mov(ir->dst, ir->src1);
}

static void ir_x64_store(IR* ir)
{
    X64Mem dst;
    if(ir->kind == IR_STORE)
    {
        vp_assertX(!vrf_spill(ir->src2), "src2 spilled");
        dst = MEM_MAKE(ir->src2->phys, NOREG, 1, 0, 8);
    }
    else
    {
        vp_assertX(ir->kind == IR_STORE_S, "not IR_STORE_S");
        vp_assertX(!vrf_const(ir->src2), "const src2");
        vp_assertX(vrf_spill(ir->src2), "src2 not spilled");
        int32_t disp = ir->src2->fi.ofs;
        dst = MEM_MAKE(RN_BP, NOREG, 1, disp, 8);
    }

    if(vrf_flo(ir->src1))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        X64Reg src = irx64X[ir->src1->phys];
        switch(ir->src1->vsize)
        {
        case VRSize4: EMITX64(movssMX)(dst, src); break;
        case VRSize8: EMITX64(movsdMX)(dst, src); break;
        default: vp_assertX(0, "?");
        }
    }
    else
    {
        VRSize p = ir->src1->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(vrf_const(ir->src1))
        {
            EMITX64(movMI)(dst, ir->src1->i64);
        }
        else
        {
            X64Reg src = irx64R[p][ir->src1->phys];
            EMITX64(movMR)(dst, src);
        }
    }
}

static void ir_x64_load(IR* ir)
{
    X64Mem mem;
    if(ir->kind == IR_LOAD)
    {
        vp_assertX(!vrf_spill(ir->src1), "src1 spilled");
        mem = MEM_MAKE(ir->src1->phys, NOREG, 1, 0, 8);
    }
    else
    {
        vp_assertX(ir->kind == IR_LOAD_S, "not IR_LOAD_S");
        vp_assertX(!vrf_const(ir->src1), "const src1");
        vp_assertX(vrf_spill(ir->src1), "src1 not spilled");

        int32_t disp = ir->src1->fi.ofs;
        mem = MEM_MAKE(RN_BP, NOREG, 1, disp, 8);
    }

    if(vrf_flo(ir->dst))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        switch(ir->src1->vsize)
        {
        case VRSize4: EMITX64(movssXM)(irx64X[ir->dst->phys], mem); break;
        case VRSize8: EMITX64(movsdXM)(irx64X[ir->dst->phys], mem); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(vrf_const(ir->src1))
        {
            EMITX64(movMI)(mem, ir->src1->i64);
        }
        else
        {
            EMITX64(movRM)(irx64R[p][ir->dst->phys], mem);
        }
    }
}

static void ir_x64_ret(IR* ir)
{
    VReg dst = {.phys = vrf_flo(ir->src1) ? XN_0 : RN_AX, .vsize = VRSize8, .flag = ir->src1->flag};
    emit_mov(&dst, ir->src1);
}

static void ir_x64_pusharg(IR* ir)
{
    uint32_t dst = ir->arg.idx;
    if(vrf_flo(ir->src1))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        if(dst != ir->src1->phys)
        {
            switch(ir->src1->vsize)
            {
            case VRSize4: EMITX64(movssXX)(irx64X[dst], irx64X[ir->src1->phys]); break;
            case VRSize8: EMITX64(movsdXX)(irx64X[dst], irx64X[ir->src1->phys]); break;
            default: vp_assertX(0, "?"); break;
            }
        }
    }
    else
    {
        VRSize p = ir->src1->vsize;
        if(vrf_const(ir->src1))
        {
            EMITX64(movRI)(irx64R[p][dst], ir->src1->i64);
        }
        else if(dst != ir->src1->phys)
        {
            EMITX64(movRR)(irx64R[p][dst], irx64R[p][ir->src1->phys]);
        }
    }
}

static void ir_x64_call(IR* ir)
{
    uint32_t total = ir->call->stacksize;
    /* Save caller registers */
    push_caller_save(ir->call->saves, total);

    if(ir->call->label)
    {
        if(ir->call->export)
        {
            uint32_t ofs = sbuf_len(&V->code) + 2;
            EMITX64(callRIP)(0);
            patchinfo_callabs(ir->call->label, ofs);
        }
        else
        {
            uint32_t ofs = sbuf_len(&V->code) + 1;
            EMITX64(callREL32)(0);
            patchinfo_callrel(ir->call->fn, ofs);
        }
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        EMITX64(callR)(irx64R[VRSize8][ir->src1->phys]);
    }

    if(ir->dst)
    {
        if(vrf_flo(ir->dst))
        {
            if(ir->dst->phys != XN_0)
            {
                switch(ir->dst->vsize)
                {
                case VRSize4: EMITX64(movssXX)(irx64X[ir->dst->phys], irx64X[XN_0]); break;
                case VRSize8: EMITX64(movsdXX)(irx64X[ir->dst->phys], irx64X[XN_0]); break;
                default: vp_assertX(0, "?");
                }
            }
        }
        else
        {
            if(ir->dst->phys != RN_AX)
            {
                VRSize p = ir->dst->vsize;
                vp_assertVSize(p, VRSize1, VRSize8);
                EMITX64(movRR)(irx64R[p][ir->dst->phys], irx64R[p][RN_AX]);
            }
        }
    }

    /* Restore caller registers */
    pop_caller_save(ir->call->saves, total);
}

static void ir_x64_cast(IR* ir)
{
    vp_assertX(!vrf_const(ir->src1), "const src1");
    if(vrf_flo(ir->dst))
    {
        if(vrf_flo(ir->src1))
        {
            /* float -> float */
            vp_assertX(!vrf_const(ir->src1), "const src1");
            vp_assertX(ir->dst->vsize != ir->src1->vsize, "dst == src1");
            X64Reg dst = irx64X[ir->dst->phys];
            X64Reg src = irx64X[ir->src1->phys];
            switch(ir->dst->vsize)
            {
            case VRSize4: EMITX64(cvtsd2ssXX)(dst, src); break;
            case VRSize8: EMITX64(cvtss2sdXX)(dst, src); break;
            default: vp_assertX(0, "?"); break;
            }
        }
        else
        {
            /* int -> float */
            VRSize ps = ir->src1->vsize;
            if(ps < VRSize4)
            {
                if(ir->cast.srcunsigned)
                {
                    EMITX64(movzxRR)(irx64R[VRSize4][ir->src1->phys], irx64R[ps][ir->src1->phys]);
                }
                else
                {
                    EMITX64(movsxRR)(irx64R[VRSize4][ir->src1->phys], irx64R[ps][ir->src1->phys]);
                }
                ps = VRSize4;
            }
            X64Reg dst = irx64X[ir->dst->phys];
            X64Reg src = irx64R[ps][ir->src1->phys];
            if(!ir->cast.srcunsigned)
            {
                switch(ir->dst->vsize)
                {
                case VRSize4: EMITX64(cvtsi2ssXR)(dst, src); break;
                case VRSize8: EMITX64(cvtsi2sdXR)(dst, src); break;
                default: vp_assertX(0, "?"); break;
                }
            }
            else if(ps < VRSize8)
            {
                switch(ir->dst->vsize)
                {
                case VRSize4: EMITX64(cvtsi2ssXR)(dst, irx64R[VRSize8][ir->src1->phys]); break;
                case VRSize8: EMITX64(cvtsi2sdXR)(dst, irx64R[VRSize8][ir->src1->phys]); break;
                default: vp_assertX(0, "?"); break;
                }
            }
            else
            {
                vp_assertX(0, "not implemented");
            }
        }
    }
    else if(vrf_flo(ir->src1))
    {
        /* float -> int */
        vp_assertX(!vrf_const(ir->src1), "const src1");
        VRSize pd = ir->dst->vsize;
        if(pd < VRSize4)
        {
            pd = VRSize4;
        }
        switch(ir->src1->vsize)
        {
        case VRSize4: EMITX64(cvttss2siRX)(irx64R[pd][ir->dst->phys], irx64X[ir->src1->phys]); break;
        case VRSize8: EMITX64(cvttsd2siRX)(irx64R[pd][ir->dst->phys], irx64X[ir->src1->phys]); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        /* int -> int */
        if(ir->dst->vsize <= ir->src1->vsize)
        {
            if(ir->dst->phys != ir->src1->phys)
            {
                VRSize p = ir->dst->vsize;
                vp_assertVSize(p, VRSize1, VRSize8);
                EMITX64(movRR)(irx64R[p][ir->dst->phys], irx64R[p][ir->src1->phys]);
            }
        }
        else
        {
            VRSize ps = ir->src1->vsize;
            VRSize pd = ir->dst->vsize;
            vp_assertVSize(ps, VRSize1, VRSize8);
            vp_assertVSize(pd, VRSize1, VRSize8);
            if(ir->cast.srcunsigned)
            {
                if(ps == VRSize4)
                {
                    /* MOVZX 64bit, 32bit doesn't exist! */
                    EMITX64(movRR)(irx64R[ps][ir->dst->phys], irx64R[ps][ir->src1->phys]);
                }
                else
                {
                    EMITX64(movzxRR)(irx64R[pd][ir->dst->phys], irx64R[ps][ir->src1->phys]);
                }
            }
            else
            {
                EMITX64(movsxRR)(irx64R[pd][ir->dst->phys], irx64R[ps][ir->src1->phys]);
            }
        }
    }
}

static void ir_x64_keep(IR* ir)
{
    UNUSED(ir);
}

static void ir_x64_asm(IR* ir)
{
    Inst* inst = ir->asm_.inst;
    vp_inst_x64(inst);
}

static void cmp_vregs(VReg* src1, VReg* src2, CondKind cond)
{
    if(vrf_flo(src1))
    {
        vp_assertX((src2->flag & (VRF_FLO | VRF_CONST)) == VRF_FLO, "?");
        X64Reg s1 = irx64X[src1->phys];
        X64Reg s2 = irx64X[src2->phys];
        if((cond & COND_MASK) <= COND_NEQ)
        {
            switch(src1->vsize)
            {
            case VRSize4: EMITX64(ucomissXX)(s1, s2); break;
            case VRSize8: EMITX64(ucomisdXX)(s1, s2); break;
            default: vp_assertX(0, "?"); break;
            }
        }
        else
        {
            switch(src1->vsize)
            {
            case VRSize4: EMITX64(comissXX)(s1, s2); break;
            case VRSize8: EMITX64(comisdXX)(s1, s2); break;
            default: vp_assertX(0, "?"); break;
            }
        }
    }
    else
    {
        vp_assertX(!vrf_const(src1), "const src");
        VRSize p = src1->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        X64Reg s1 = irx64R[p][src1->phys];
        X64Reg s2 = irx64R[p][src2->phys];
        if(vrf_const(src2) && src2->i64 == 0)
        {
            EMITX64(testRR)(s1, s1);
        }
        else
        {
            if(vrf_const(src2))
            {
                EMITX64(cmpRI)(s1, src2->i64);
            }
            else
            {
                EMITX64(cmpRR)(s1, s2);
            }
        }
    }
}

static X64CC cond2cc(CondKind cond)
{
    switch(cond)
    {
    case COND_EQ | COND_UNSIGNED:   /* Fall */
    case COND_EQ: return CC_E;

    case COND_NEQ | COND_UNSIGNED:  /* Fall */
    case COND_NEQ: return CC_NE;

    case COND_LT: return CC_L;
    case COND_GT: return CC_G;
    case COND_LE: return CC_LE;
    case COND_GE: return CC_GE;

    case COND_LT | COND_UNSIGNED: return CC_B;
    case COND_GT | COND_UNSIGNED: return CC_A;
    case COND_LE | COND_UNSIGNED: return CC_BE;
    case COND_GE | COND_UNSIGNED: return CC_AE;
    default: vp_assertX(0, "?"); break;
    }
    return 0;
}

static void ir_x64_cond(IR* ir)
{
    VReg* src1 = ir->src1, *src2 = ir->src2;
    vp_assertX(!vrf_const(ir->dst), "const dst");
    CondKind cond = ir->cond;
    if(vrf_flo(ir->dst))
    {
        vp_assertX(0, "not implemented");
    }

    cmp_vregs(src1, src2, cond);

    X64CC cc = cond2cc(cond);
    EMITX64(setcc)(cc, irx64R[VRSize1][ir->dst->phys]);
    EMITX64(movsxRR)(irx64R[VRSize4][ir->dst->phys], irx64R[VRSize1][ir->dst->phys]);
}

static void ir_x64_jmp(IR* ir)
{
    CondKind cond = ir->jmp.cond;
    vp_assertX(cond != COND_NONE, "unconditional jump");

    VReg* src1 = ir->src1, *src2 = ir->src2;
    if(cond & COND_FLO)
    {
        cond &= COND_MASK;
        switch(cond)
        {
        case COND_EQ:
            cmp_vregs(src1, src2, cond);
            EMITX64(jccREL32)(CC_P, 0);
            EMITX64(jccREL32)(CC_E, 0);
            return;
        case COND_NEQ:
            cmp_vregs(src1, src2, cond);
            EMITX64(jccREL32)(CC_P, 0);
            EMITX64(jccREL32)(CC_NE, 0);
            return;
        case COND_LT: case COND_LE:
        {
            VReg* tmp = src1;
            src1 = src2;
            src2 = tmp;
            cond = vp_cond_swap(cond);
            break;
        }
        default: break;
        }
        cond |= COND_UNSIGNED;
    }

    if(cond == COND_ANY)
    {
        uint32_t patch = sbuf_len(&V->code) + 1;
        EMITX64(jmpREL32)(0);
        patchinfo_jmprel(ir->jmp.bb, patch);
        return;
    }

    cmp_vregs(src1, src2, cond);

    uint32_t patch = sbuf_len(&V->code) + 2;
    X64CC cc = cond2cc(cond);
    EMITX64(jccREL32)(cc, 0);
    patchinfo_jmprel(ir->jmp.bb, patch);
}

static void ir_x64_add(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src");
    if(vrf_flo(ir->dst))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        vp_assertX(!vrf_const(ir->src2), "const src2");
        X64Reg dst = irx64X[ir->dst->phys];
        X64Reg src = irx64X[ir->src2->phys];
        switch(ir->dst->vsize)
        {
        case VRSize4: EMITX64(addssXX)(dst, src); break;
        case VRSize8: EMITX64(addsdXX)(dst, src); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        X64Reg dst = irx64R[p][ir->dst->phys];
        if(vrf_const(ir->src2))
        {
            switch(ir->src2->i64)
            {
            case 0: break;
            case 1: EMITX64(incR)(dst); break;
            case -1: EMITX64(decR)(dst); break;
            default: EMITX64(addRI)(dst, ir->src2->i64); break;
            }
        }
        else
        {
            EMITX64(addRR)(dst, irx64R[p][ir->src2->phys]);
        }
    }
}

static void ir_x64_sub(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    if(vrf_flo(ir->dst))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        vp_assertX(!vrf_const(ir->src2), "const src2");
        X64Reg dst = irx64X[ir->dst->phys];
        X64Reg src = irx64X[ir->src2->phys];
        switch(ir->dst->vsize)
        {
        case VRSize4: EMITX64(subssXX)(dst, src); break;
        case VRSize8: EMITX64(subsdXX)(dst, src); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        X64Reg dst = irx64R[p][ir->dst->phys];
        if(vrf_const(ir->src2))
        {
            switch(ir->src2->i64)
            {
            case 0: break;
            case 1: EMITX64(decR)(dst); break;
            case -1: EMITX64(incR)(dst); break;
            default: EMITX64(subRI)(dst, ir->src2->i64); break;
            }
        }
        else
        {
            EMITX64(subRR)(dst, irx64R[p][ir->src2->phys]);
        }
    }
}

static void ir_x64_mul(IR* ir)
{
    vp_assertX(!vrf_const(ir->src1) &&
            !vrf_const(ir->src2), "const src1 and const src2");
    if(vrf_flo(ir->dst))
    {
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        X64Reg dst = irx64X[ir->dst->phys];
        X64Reg src = irx64X[ir->src2->phys];
        switch(ir->dst->vsize)
        {
        case VRSize4: EMITX64(mulssXX)(dst, src); break;
        case VRSize8: EMITX64(mulsdXX)(dst, src); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        /* Break RAX, RDX */
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        vp_assertX(ir->src2->phys != RN_AX, "src2 == RAX");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        X64Reg a = irx64R[p][RN_AX];
        if(ir->src1->phys != RN_AX)
        {
            EMITX64(movRR)(a, irx64R[p][ir->src1->phys]);
        }
        EMITX64(mulR)(irx64R[p][ir->src2->phys]);
        if(ir->dst->phys != RN_AX)
        {
            EMITX64(movRR)(irx64R[p][ir->dst->phys], a);
        }
    }
}

static void ir_x64_div(IR* ir)
{
    vp_assertX(!vrf_const(ir->src1) &&
            !vrf_const(ir->src2), "const src1 and src2");
    if(vrf_flo(ir->dst))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        vp_assertX(!vrf_const(ir->src2), "const src2");
        X64Reg dst = irx64X[ir->dst->phys];
        X64Reg src = irx64X[ir->src2->phys];
        switch(ir->dst->vsize)
        {
        case VRSize4: EMITX64(divssXX)(dst, src); break;
        case VRSize8: EMITX64(divsdXX)(dst, src); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        vp_assertX(ir->src2->phys != RN_AX, "src2 == RAX");
        /* Break RAX, RDX */
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        X64Reg a = irx64R[p][RN_AX];
        if(ir->src1->phys != RN_AX)
        {
            EMITX64(movRR)(a, irx64R[p][ir->src1->phys]);
        }
        if(!irf_unsigned(ir))
        {
            switch(p)
            {
            case VRSize2: EMITX64(cwde)(); break;
            case VRSize4: EMITX64(cdq)(); break;
            case VRSize8: EMITX64(cqo)(); break;
            default: vp_assertX(0, "?");
            }
            EMITX64(idivR)(irx64R[p][ir->src2->phys]);
        }
        else
        {
            vp_assertX(p != VRSize1, "r8?");
            X64Reg d = irx64R[p][RN_DX];
            EMITX64(xorRR)(d, d);
            EMITX64(divR)(irx64R[p][ir->src2->phys]);
        }
        if(ir->dst->phys != RN_AX)
        {
            EMITX64(movRR)(irx64R[p][ir->dst->phys], a);
        }
    }
}

static void ir_x64_mod(IR* ir)
{
    vp_assertX(!vrf_const(ir->src1) &&
            !vrf_const(ir->src2), "const src1 and src2");
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(ir->src2->phys != 0, "dst == RAX");
    /* Break RAX, RDX */
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    X64Reg a = irx64R[p][RN_AX];
    X64Reg d = irx64R[p][RN_DX];
    if(ir->src1->phys != 0)
    {
        EMITX64(movRR)(a, irx64R[p][ir->src1->phys]);
    }
    if(!irf_unsigned(ir))
    {
        switch(p)
        {
        case VRSize2: EMITX64(cwde)(); break;
        case VRSize4: EMITX64(cdq)(); break;
        case VRSize8: EMITX64(cqo)(); break;
        default: vp_assertX(0, "?");
        }
        EMITX64(idivR)(ir->src2->phys);
    }
    else
    {
        vp_assertX(p != VRSize1, "r8?");
        EMITX64(xorRR)(d, d);
        EMITX64(divR)(ir->src2->phys);
    }
    if(ir->dst->phys != RN_DX)
    {
        EMITX64(movRR)(irx64R[p][ir->dst->phys], d);
    }
}

static void ir_x64_band(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    X64Reg dst = irx64R[p][ir->dst->phys];
    if(vrf_const(ir->src2))
    {
        EMITX64(andRI)(dst, ir->src2->i64);
    }
    else
    {
        EMITX64(andRR)(dst, irx64R[p][ir->src2->phys]);
    }
}

static void ir_x64_bor(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    X64Reg dst = irx64R[p][ir->dst->phys];
    if(vrf_const(ir->src2))
    {
        EMITX64(orRI)(dst, ir->src2->i64);
    }
    else
    {
        EMITX64(orRR)(dst, irx64R[p][ir->src2->phys]);
    }
}

static void ir_x64_bxor(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    X64Reg dst = irx64R[p][ir->dst->phys];
    if(vrf_const(ir->src2))
    {
        EMITX64(xorRI)(dst, ir->src2->i64);
    }
    else
    {
        EMITX64(xorRR)(dst, irx64R[p][ir->src2->phys]);
    }
}

static void ir_x64_lshift(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    X64Reg dst = irx64R[p][ir->dst->phys];
    if(vrf_const(ir->src2))
    {
        EMITX64(shlRI)(dst, ir->src2->i64 & 255);
    }
    else
    {
        vp_assertX(ir->src2->phys != RN_CX, "src2 == RCX");
        vp_assertX(ir->dst->phys != RN_CX, "dst == RCX");
        EMITX64(movRR)(CL, irx64R[VRSize1][ir->src2->phys]);
        EMITX64(shlRCL)(dst);
    }
}

static void ir_x64_rshift(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    X64Reg dst = irx64R[p][ir->dst->phys];
    if(vrf_const(ir->src2))
    {
        if(ir->flag & IRF_UNSIGNED)
        {
            EMITX64(shrRI)(dst, ir->src2->i64 & 255);
        }
        else
        {
            EMITX64(sarRI)(dst, ir->src2->i64 & 255);
        }
    }
    else
    {
        vp_assertX(ir->src2->phys != RN_CX, "src2 == RCX");
        vp_assertX(ir->dst->phys != RN_CX, "dst == RCX");
        EMITX64(movRR)(CL, irx64R[VRSize1][ir->src2->phys]);
        if(ir->flag & IRF_UNSIGNED)
        {
            EMITX64(shrRCL)(dst);
        }
        else
        {
            EMITX64(sarRCL)(dst);
        }
    }
}

static void ir_x64_neg(IR* ir)
{
    vp_assertX(!vrf_const(ir->dst), "const dst");
    if(vrf_flo(ir->src1))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        vp_assertX(ir->dst->phys != ir->src1->phys, "dst == src1");
        switch(ir->src1->vsize)
        {
            case VRSize4:
            case VRSize8:
            {
                bool single = ir->src1->vsize == VRSize4;
                EMITX64(pushR)(RAX);
                uint64_t mask = single ? (1ULL << 31) : (1ULL << 63);
                X64Reg dst = irx64X[ir->dst->phys];
                EMITX64(movRI)(RAX, mask);
                if(single)
                {
                    EMITX64(movdXR)(dst, EAX);
                }
                else
                {
                    EMITX64(movqXR)(dst, RAX);
                }
                EMITX64(popR)(RAX);
                X64Reg src = irx64X[ir->src1->phys];
                if(single)
                {
                    EMITX64(xorpsXX)(dst, src);
                }
                else
                {
                    EMITX64(xorpdXX)(dst, src);
                }
                break;
            }
            default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        EMITX64(negR)(irx64R[p][ir->dst->phys]);
    }
}

static void ir_x64_bnot(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->dst), "const dst");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    EMITX64(notR)(irx64R[p][ir->dst->phys]);
}

typedef void (*IRFn)(IR* ir);
static const IRFn irx64tab[] = {
    [IR_BOFS] = ir_x64_bofs,
    [IR_IOFS] = ir_x64_iofs,
    [IR_SOFS] = ir_x64_sofs,
    [IR_MOV] = ir_x64_mov,
    [IR_STORE] = ir_x64_store, [IR_LOAD] = ir_x64_load,
    [IR_STORE_S] = ir_x64_store, [IR_LOAD_S] = ir_x64_load,
    [IR_RET] = ir_x64_ret,
    [IR_COND] = ir_x64_cond,
    [IR_JMP] = ir_x64_jmp,
    [IR_PUSHARG] = ir_x64_pusharg,
    [IR_CALL] = ir_x64_call,
    [IR_CAST] = ir_x64_cast,
    [IR_KEEP] = ir_x64_keep,
    [IR_ASM] = ir_x64_asm,
    [IR_ADD] = ir_x64_add, [IR_SUB] = ir_x64_sub,
    [IR_MUL] = ir_x64_mul, [IR_DIV] = ir_x64_div, [IR_MOD] = ir_x64_mod,
    [IR_BAND] = ir_x64_band, [IR_BOR] = ir_x64_bor, [IR_BXOR] = ir_x64_bxor,
    [IR_LSHIFT] = ir_x64_lshift, [IR_RSHIFT] = ir_x64_rshift,
    [IR_NEG] = ir_x64_neg, [IR_BNOT] = ir_x64_bnot,
};

void vp_ir_x64(IR* ir)
{
    vp_assertX(ir->kind < (int)ARRSIZE(irx64tab), "out of bounds ir kind");
    vp_assertX(irx64tab[ir->kind], "empty entry %d", ir->kind);
    return (*irx64tab[ir->kind])(ir);
}

/* x64 register allocator constraints */
RegSet vp_ir_x64_extra(RegAlloc* ra, IR* ir)
{
    RegSet ioccupy = 0;
    switch(ir->kind)
    {
        case IR_MUL: case IR_DIV: case IR_MOD:
            if(!vrf_flo(ir->dst))
                ioccupy = (1ULL << RN_DX) | (1ULL << RN_AX);
            break;
        case IR_LSHIFT: case IR_RSHIFT:
            if(!vrf_flo(ir->src2))
                ioccupy = 1ULL << RN_CX;
            break;
        default: break;
    }
    if(ra->flag & RAF_STACK_FRAME)
        ioccupy |= 1ULL << RN_BP;
    ioccupy |= 1ULL << RN_SP;
    return ioccupy;
}

/* Convert `A = B op C` to `A = B; A = A op C` */
static void ir_x64_conv3to2(vec_t(BB*) bbs)
{
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            switch(ir->kind)
            {
                case IR_NEG:
                {
                    if(vrf_flo(ir->dst))
                    {
                        vp_assertX(ir->dst->virt != ir->src1->virt, "dst == src1");
                        ir_tmp_mov(&ir->src1, &bb->irs, j++, ir->flag);

                        IR* keep = vp_ir_keep(NULL, ir->src1, NULL);
                        j++; vec_insert(bb->irs, j, keep);
                        break;
                    }
                }
                /* Fallthrough */
                case IR_ADD:
                case IR_SUB:
                case IR_MUL:
                case IR_DIV:
                case IR_MOD:
                case IR_BAND:
                case IR_BOR:
                case IR_BXOR:
                case IR_LSHIFT:
                case IR_RSHIFT:
                case IR_BNOT:
                {
                    vp_assertX(!vrf_const(ir->dst), "const dst");
                    IR* mov = vp_ir_mov(ir->dst, ir->src1, ir->flag);
                    vec_insert(bb->irs, j, mov); j++;
                    ir->src1 = ir->dst;
                    break;
                }
                default:
                    break;
            }
        }
    }
}

/* Tweak the IR for x64 */
void vp_ir_x64_tweak(Code* code)
{
    vec_t(BB*) bbs = code->bbs;
    ir_x64_conv3to2(bbs);
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            /* Constant floats */
            {
                VReg** vregs[] = {&ir->src1, &ir->src2};
                for(uint32_t k = 0; k < 2; k++)
                {
                    VReg** vp = vregs[k], *vr = *vp;
                    if(vr && vrf_const(vr) && ((vr->flag & (VRF_FLO | VRF_CONST)) == (VRF_FLO | VRF_CONST)))
                    {
                        j = ir_tmp_fmov(vp, &bb->irs, j);
                    }
                }
            }

            /* Only MOV can embed a 64 bit immediate */
            if(ir->kind != IR_MOV)
            {
                VReg** vregs[] = {&ir->src1, &ir->src2};
                for(uint32_t k = 0; k < 2; k++)
                {
                    VReg** vp = vregs[k], *vr = *vp;
                    if(vr && vrf_const(vr) && !vp_isimm32(vr->i64))
                    {
                        ir_tmp_mov(vp, &bb->irs, j++, ir->flag);
                    }
                }
            }

            switch(ir->kind)
            {
            case IR_MUL:
            case IR_DIV:
            case IR_MOD:
                vp_assertX(!vrf_const(ir->src1), "const src1");
                if(vrf_const(ir->src2))
                {
                    ir_tmp_mov(&ir->src2, &bb->irs, j++, ir->flag);
                }
                break;
            case IR_CALL:
                if(ir->src1 && vrf_const(ir->src1))
                {
                    ir_tmp_mov(&ir->src1, &bb->irs, j++, ir->flag);
                }
                break;
            default: break;
            }
        }
    }
}