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
#include "vp_vec.h"
#include "vp_low.h"

static void emit_mov(VReg* dst, VReg* src)
{
    if(vrf_flo(dst))
    {
        vp_assertX(!vrf_const(src), "const src1");
        if(!vrf_flo(src))
        {
            switch(src->vsize)
            {
            case VRSize4: emit_movd_xr(V, dst->phys, src->phys); break;
            case VRSize8: emit_movq_xr(V, dst->phys, src->phys); break;
            default: vp_assertX(0, "?"); break;
            }
        }
        else
        {
            if(src->phys != dst->phys)
            {
                switch(src->vsize)
                {
                case VRSize4: emit_movss_rr(V, dst->phys, src->phys); break;
                case VRSize8: emit_movsd_rr(V, dst->phys, src->phys); break;
                default: vp_assertX(0, "?"); break;
                }
            }
        }
    }
    else
    {
        VRSize pow = src->vsize;
        if(vrf_const(src))
        {
            emit_mov_ri(pow, dst->phys, src->i64);
        }
        else
        {
            if(dst->phys != src->phys)
            {
                emit_mov_rr(pow, dst->phys, src->phys);
            }
        }
    }
}

static void ir_x64_bofs(IR* ir)
{
    int64_t ofs = ir->bofs.fi->ofs + ir->bofs.ofs;
    emit_lea64_rm(V, ir->dst->phys, MEM(RBP, NOREG, 1, ofs));
}

static void ir_x64_iofs(IR* ir)
{
    Str* label = ir->iofs.label;
    int64_t ofs = ir->iofs.ofs;
    emit_lea64_rm(V, ir->dst->phys, MEM(RIP, NOREG, 1, ofs));
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
    emit_lea64_rm(V, ir->dst->phys, MEM(RSP, NOREG, 1, ofs));
}

static void ir_x64_mov(IR* ir)
{
    emit_mov(ir->dst, ir->src1);
}

static void ir_x64_store(IR* ir)
{
    int32_t disp = 0;
    uint32_t base = ir->src2->phys;
    uint32_t src = ir->src1->phys;
    if(ir->kind == IR_STORE_S)
    {
        vp_assertX(!vrf_const(ir->src2), "const src2");
        vp_assertX(vrf_spill(ir->src2), "src2 not spilled");
        base = RBP;
        disp = ir->src2->fi.ofs;
    }

    if(vrf_flo(ir->src1))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        switch(ir->src1->vsize)
        {
        case VRSize4: emit_movss_mr(V, src, MEM(base, NOREG, 1, disp)); break;
        case VRSize8: emit_movsd_mr(V, src, MEM(base, NOREG, 1, disp)); break;
        default: vp_assertX(0, "?");
        }
    }
    else
    {
        VRSize p = ir->src1->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(vrf_const(ir->src1))
        {
            switch(p)
            {
            case VRSize1: emit_mov8_mi(V, ir->src2->phys, ir->src1->i64); break;
            case VRSize2: emit_mov16_mi(V, ir->src2->phys, ir->src1->i64); break;
            case VRSize4: emit_mov32_mi(V, ir->src2->phys, ir->src1->i64); break;
            case VRSize8: emit_mov64_mi(V, ir->src2->phys, ir->src1->i64); break;
            }
        }
        else
        {
            switch(p)
            {
            case VRSize1: emit_mov8_mr(V, MEM(base, NOREG, 1, disp), src); break;
            case VRSize2: emit_mov16_mr(V, MEM(base, NOREG, 1, disp), src); break;
            case VRSize4: emit_mov32_mr(V, MEM(base, NOREG, 1, disp), src); break;
            case VRSize8: emit_mov64_mr(V, MEM(base, NOREG, 1, disp), src); break;
            }
        }
    }
}

static void ir_x64_load(IR* ir)
{
    int32_t disp = 0;
    uint32_t base = ir->src1->phys;
    uint32_t dst = ir->dst->phys;
    if(ir->kind == IR_LOAD_S)
    {
        disp = ir->src1->fi.ofs;
        base = RBP;
    }

    if(vrf_flo(ir->dst))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        switch(ir->src1->vsize)
        {
        case VRSize4: emit_movss_rm(V, dst, MEM(base, NOREG, 1, disp)); break;
        case VRSize8: emit_movsd_rm(V, dst, MEM(base, NOREG, 1, disp)); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(vrf_const(ir->src1))
        {
            switch(p)
            {
            case VRSize1: emit_mov8_mi(V, dst, ir->src1->i64); break;
            case VRSize2: emit_mov16_mi(V, dst, ir->src1->i64); break;
            case VRSize4: emit_mov32_mi(V, dst, ir->src1->i64); break;
            case VRSize8: emit_mov64_mi(V, dst, ir->src1->i64); break;
            }
        }
        else
        {
            switch(p)
            {
            case VRSize1: emit_mov8_rm(V, dst, MEM(base, NOREG, 1, disp)); break;
            case VRSize2: emit_mov16_rm(V, dst, MEM(base, NOREG, 1, disp)); break;
            case VRSize4: emit_mov32_rm(V, dst, MEM(base, NOREG, 1, disp)); break;
            case VRSize8: emit_mov64_rm(V, dst, MEM(base, NOREG, 1, disp)); break;
            }
        }
    }
}

static void ir_x64_ret(IR* ir)
{
    VReg dst = {.phys = vrf_flo(ir->src1) ? XMM0 : RAX, .vsize = VRSize8, .flag = ir->src1->flag};
    emit_mov(&dst, ir->src1);
}

static void ir_x64_pusharg(IR* ir)
{
    if(vrf_flo(ir->src1))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        uint32_t dst = V->target->raset->fmap[ir->arg.idx];
        VRSize p = ir->src1->vsize;
        if(dst != ir->src1->phys)
        {
            switch(p)
            {
            case VRSize4: emit_movss_rr(V, dst, ir->src1->phys); break;
            case VRSize8: emit_movsd_rr(V, dst, ir->src1->phys); break;
            default: vp_assertX(0, "?"); break;
            }
        }
    }
    else
    {
        uint32_t dst = V->target->raset->imap[ir->arg.idx];
        VRSize p = ir->src1->vsize;
        if(vrf_const(ir->src1))
        {
            emit_mov_ri(p, dst, ir->src1->i64);
        }
        else if(dst != ir->src1->phys)
        {
            emit_mov_rr(p, dst, ir->src1->phys);
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
            emit_call_rip_rel32(V, 0);
            patchinfo_callabs(ir->call->label, ofs);
        }
        else
        {
            uint32_t ofs = sbuf_len(&V->code) + 1;
            emit_call_rel32(V, 0);
            patchinfo_callrel(ir->call->fn, ofs);
        }
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        emit_call64_r(V, ir->src1->phys);
    }

    /* Restore caller registers */
    pop_caller_save(ir->call->saves, total);

    if(ir->dst)
    {
        if(vrf_flo(ir->dst))
        {
            if(ir->dst->phys != XMM0)
            {
                switch(ir->dst->vsize)
                {
                case VRSize4: emit_movss_rr(V, XMM0, ir->dst->phys); break;
                case VRSize8: emit_movsd_rr(V, XMM0, ir->dst->phys); break;
                default: vp_assertX(0, "?");
                }
            }
        }
        else
        {
            if(ir->dst->phys != RAX)
            {
                VRSize p = ir->dst->vsize;
                vp_assertVSize(p, VRSize1, VRSize8);
                emit_mov_rr(p, RAX, ir->dst->phys);
            }
        }
    }
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
            switch(ir->dst->vsize)
            {
                case VRSize4: emit_cvtsd2ss_rr(V, ir->dst->phys, ir->src1->phys); break;
                case VRSize8: emit_cvtss2sd_rr(V, ir->dst->phys, ir->src1->phys); break;
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
                    emit_movzx_rr(VRSize4, ps, ir->src1->phys, ir->src1->phys);
                }
                else
                {
                    emit_movsx_rr(VRSize4, ps, ir->src1->phys, ir->src1->phys);
                }
                ps = VRSize4;
            }
            if(!ir->cast.srcunsigned)
            {
                switch(ir->dst->vsize)
                {
                    case VRSize4: emit_cvtsi2ss_xr(ir->dst->vsize, ir->dst->phys, ir->src1->phys); break;
                    case VRSize8: emit_cvtsi2sd_xr(ir->dst->vsize, ir->dst->phys, ir->src1->phys); break;
                    default: vp_assertX(0, "?"); break;
                }
            }
            else if(ps < VRSize8)
            {
                switch(ir->dst->vsize)
                {
                    case VRSize4: emit_cvtsi2ss_xr64(V, ir->dst->phys, ir->src1->phys); break;
                    case VRSize8: emit_cvtsi2sd_xr64(V, ir->dst->phys, ir->src1->phys); break;
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
            case VRSize4: emit_cvttss2si_rx(pd, ir->dst->phys, ir->src1->phys); break;
            case VRSize8: emit_cvttsd2si_rx(pd, ir->dst->phys, ir->src1->phys); break;
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
                emit_mov_rr(p, ir->dst->phys, ir->src1->phys);
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
                    emit_mov_rr(ps, ir->dst->phys, ir->src1->phys);
                }
                else
                {
                    emit_movzx_rr(pd, ps, ir->dst->phys, ir->src1->phys);
                }
            }
            else
            {
                emit_movsx_rr(pd, ps, ir->dst->phys, ir->src1->phys);
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
        if((cond & COND_MASK) <= COND_NEQ)
        {
            switch(src1->vsize)
            {
            case VRSize4: emit_ucomiss_rr(V, src1->phys, src2->phys); break;
            case VRSize8: emit_ucomisd_rr(V, src1->phys, src2->phys); break;
            default: vp_assertX(0, "?"); break;
            }
        }
        else
        {
            switch(src1->vsize)
            {
            case VRSize4: emit_comiss_rr(V, src1->phys, src2->phys); break;
            case VRSize8: emit_comisd_rr(V, src1->phys, src2->phys); break;
            default: vp_assertX(0, "?"); break;
            }
        }
    }
    else
    {
        vp_assertX(!vrf_const(src1), "const src");
        VRSize p = src1->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(vrf_const(src2) && src2->i64 == 0)
        {
            emit_test_r(p, src1->phys, src1->phys);
        }
        else
        {
            if(vrf_const(src2))
            {
                emit_cmp_ri(p, src1->phys, src2->i64);
            }
            else
            {
                emit_cmp_rr(p, src1->phys, src2->i64);
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
    uint32_t dst = ir->dst->phys;
    CondKind cond = ir->cond;
    if(vrf_flo(ir->dst))
    {
        vp_assertX(0, "not implemented");
    }

    cmp_vregs(src1, src2, cond);

    X64CC cc = cond2cc(cond);
    emit_setcc(V, cc, dst);
    emit_movsx_r32r8(V, dst, dst);
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
            emit_jcc_rel32(V, CC_P, 0);
            emit_jcc_rel32(V, CC_E, 0);
            return;
        case COND_NEQ:
            cmp_vregs(src1, src2, cond);
            emit_jcc_rel32(V, CC_P, 0);
            emit_jcc_rel32(V, CC_NE, 0);
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
        emit_jmp_rel32(V, 0);
        patchinfo_jmprel(ir->jmp.bb, patch);
        return;
    }

    cmp_vregs(src1, src2, cond);

    uint32_t patch = sbuf_len(&V->code) + 2;
    X64CC cc = cond2cc(cond);
    emit_jcc_rel32(V, cc, 0);
    patchinfo_jmprel(ir->jmp.bb, patch);
}

static void ir_x64_add(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src");
    if(vrf_flo(ir->dst))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        vp_assertX(!vrf_const(ir->src2), "const src2");
        switch(ir->dst->vsize)
        {
        case VRSize4: emit_addss_rr(V, ir->dst->phys, ir->src2->phys); break;
        case VRSize8: emit_addsd_rr(V, ir->dst->phys, ir->src2->phys); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        uint32_t dst = ir->dst->phys;
        if(vrf_const(ir->src2))
        {
            switch(ir->src2->i64)
            {
            case 0: break;
            case 1: emit_inc_r(p, dst); break;
            case -1: emit_dec_r(p, dst); break;
            default: emit_add_ri(p, dst, ir->src2->i64); break;
            }
        }
        else
        {
            emit_add_rr(p, dst, ir->src2->phys);
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
        switch(ir->dst->vsize)
        {
        case VRSize4: emit_subss_rr(V, ir->dst->phys, ir->src2->phys); break;
        case VRSize8: emit_subsd_rr(V, ir->dst->phys, ir->src2->phys); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        uint32_t dst = ir->dst->phys;
        if(vrf_const(ir->src2))
        {
            switch(ir->src2->i64)
            {
            case 0: break;
            case 1: emit_dec_r(p, dst); break;
            case -1: emit_inc_r(p, dst); break;
            default: emit_sub_ri(p, dst, ir->src2->i64); break;
            }
        }
        else
        {
            emit_sub_rr(p, dst, ir->src2->phys);
        }
    }
}

static void ir_x64_mul(IR* ir)
{
    vp_assertX(!vrf_const(ir->src1) &&
            !vrf_const(ir->src2), "const src1 and src2");
    if(vrf_flo(ir->dst))
    {
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        switch(ir->dst->vsize)
        {
        case VRSize4: emit_mulss_rr(V, ir->dst->phys, ir->src2->phys); break;
        case VRSize8: emit_mulsd_rr(V, ir->dst->phys, ir->src2->phys); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        /* Break RAX, RDX */
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        vp_assertX(ir->src2->phys != RAX, "src2 == RAX");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(ir->src1->phys != RAX)
        {
            emit_mov_rr(p, RAX, ir->src1->phys);
        }
        emit_mul_r(p, ir->src2->phys);
        if(ir->dst->phys != RAX)
        {
            emit_mov_rr(p, ir->dst->phys, RAX);
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
        switch(ir->dst->vsize)
        {
        case VRSize4: emit_divss_rr(V, ir->dst->phys, ir->src2->phys); break;
        case VRSize8: emit_divsd_rr(V, ir->dst->phys, ir->src2->phys); break;
        default: vp_assertX(0, "?"); break;
        }
    }
    else
    {
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst == src1");
        vp_assertX(ir->src2->phys != RAX, "src2 == RAX");
        /* Break RAX, RDX */
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        if(ir->src1->phys != RAX)
        {
            emit_mov_rr(p, RAX, ir->src1->phys);
        }
        if(!irf_unsigned(ir))
        {
            switch(p)
            {
                case VRSize2: emit_cwde(V); break;
                case VRSize4: emit_cdq(V); break;
                case VRSize8: emit_cqo(V); break;
                default: vp_assertX(0, "?");
            }
            emit_idiv_r(p, ir->src2->phys);
        }
        else
        {
            switch(p)
            {
                case VRSize2: emit_xor16_rr(V, RDX, RDX); break;
                case VRSize4: emit_xor32_rr(V, RDX, RDX); break;
                case VRSize8: emit_xor32_rr(V, RDX, RDX); break;
                default: vp_assertX(0, "?");
            }
            emit_div_r(p, ir->src2->phys);
        }
        if(ir->dst->phys != RAX)
        {
            emit_mov_rr(p, ir->dst->phys, RAX);
        }
    }
}

static void ir_x64_mod(IR* ir)
{
    vp_assertX(!vrf_const(ir->src1) &&
            !vrf_const(ir->src2), "const src1 and src2");
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(ir->src2->phys != RAX, "dst == RAX");
    /* Break RAX, RDX */
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    if(ir->src1->phys != RAX)
    {
        emit_mov_rr(p, RAX, ir->src1->phys);
    }
    if(!irf_unsigned(ir))
    {
        switch(p)
        {
            case VRSize2: emit_cwde(V); break;
            case VRSize4: emit_cdq(V); break;
            case VRSize8: emit_cqo(V); break;
            default: vp_assertX(0, "?");
        }
        emit_idiv_r(p, ir->src2->phys);
    }
    else
    {
        switch(p)
        {
            case VRSize2: emit_xor16_rr(V, RDX, RDX); break;
            case VRSize4: emit_xor32_rr(V, RDX, RDX); break;
            case VRSize8: emit_xor32_rr(V, RDX, RDX); break;
            default: vp_assertX(0, "?");
        }
        emit_div_r(p, ir->src2->phys);
    }
    if(ir->dst->phys != RDX)
    {
        emit_mov_rr(p, ir->dst->phys, RDX);
    }
}

static void ir_x64_band(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    if(vrf_const(ir->src2))
    {
        emit_and_ri(p, ir->dst->phys, ir->src2->i64);
    }
    else
    {
        emit_and_rr(p, ir->dst->phys, ir->src2->phys);
    }
}

static void ir_x64_bor(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    if(vrf_const(ir->src2))
    {
        emit_or_ri(p, ir->dst->phys, ir->src2->i64);
    }
    else
    {
        emit_or_rr(p, ir->dst->phys, ir->src2->phys);
    }
}

static void ir_x64_bxor(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    if(vrf_const(ir->src2))
    {
        emit_xor_ri(p, ir->dst->phys, ir->src2->i64);
    }
    else
    {
        emit_xor_rr(p, ir->dst->phys, ir->src2->phys);
    }
}

static void ir_x64_lshift(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    if(vrf_const(ir->src2))
    {
        emit_shl_ri(p, ir->dst->phys, ir->src2->i64 & 255);
    }
    else
    {
        vp_assertX(ir->src2->phys != RCX, "src2 == RCX");
        vp_assertX(ir->dst->phys != RCX, "dst == RCX");
        emit_mov8_rr(V, RCX, ir->src2->phys);
        emit_shl_rcl(p, ir->dst->phys);
    }
}

static void ir_x64_rshift(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    if(vrf_const(ir->src2))
    {
        if(ir->flag & IRF_UNSIGNED)
        {
            emit_shr_ri(p, ir->dst->phys, ir->src2->i64 & 255);
        }
        else
        {
            emit_sar_ri(p, ir->dst->phys, ir->src2->i64 & 255);
        }
    }
    else
    {
        vp_assertX(ir->src2->phys != RCX, "src2 == RCX");
        vp_assertX(ir->dst->phys != RCX, "dst == RCX");
        emit_mov8_rr(V, RCX, ir->src2->phys);
        if(ir->flag & IRF_UNSIGNED)
        {
            emit_shr_rcl(p, ir->dst->phys);
        }
        else
        {
            emit_sar_rcl(p, ir->dst->phys);
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
                emit_push64_r(V, RAX);
                uint64_t mask = single ? (1ULL << 31) : (1ULL << 63);
                emit_mov64_ri(V, RAX, mask);
                if(single)
                {
                    emit_movd_xr(V, ir->dst->phys, RAX);
                }
                else
                {
                    emit_movq_xr(V, ir->dst->phys, RAX);
                }
                emit_pop64_r(V, RAX);
                if(single)
                {
                    emit_xorps_rr(V, ir->dst->phys, ir->src1->phys);
                }
                else
                {
                    emit_xorpd_rr(V, ir->dst->phys, ir->src1->phys);
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
        emit_neg_r(p, ir->dst->phys);
    }
}

static void ir_x64_bnot(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->dst), "const dst");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    emit_not_r(p, ir->dst->phys);
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
                ioccupy = (1ULL << RDX) | (1ULL << RAX);
            break;
        case IR_LSHIFT: case IR_RSHIFT:
            if(!vrf_flo(ir->src2))
                ioccupy = 1ULL << RCX;
            break;
        default: break;
    }
    if(ra->flag & RAF_STACK_FRAME)
        ioccupy |= 1ULL << RBP;
    ioccupy |= 1ULL << RSP;
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