/*
** vp_sel_x64.c
** Instruction selection (IR -> x64)
*/

#include "vp_emit_x64.c"

#include "vp_sel.h"
#include "vp_ir.h"
#include "vp_regalloc.h"
#include "vp_state.h"
#include "vp_target.h"
#include "vp_vec.h"

RegSet sel_extra(RegAlloc* ra, IR* ir)
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

static void emit_mov_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_mov8_rr(V, r1, r2); break;
    case VRSize2: emit_mov16_rr(V, r1, r2); break;
    case VRSize4: emit_mov32_rr(V, r1, r2); break;
    case VRSize8: emit_mov64_rr(V, r1, r2); break;
    }
}

static void emit_mov_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_mov8_ri(V, r, i); break;
    case VRSize2: emit_mov16_ri(V, r, i); break;
    case VRSize4: emit_mov32_ri(V, r, i); break;
    case VRSize8: emit_mov64_ri(V, r, i); break;
    }
}

static void emit_add_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_add8_rr(V, r1, r2); break;
    case VRSize2: emit_add16_rr(V, r1, r2); break;
    case VRSize4: emit_add32_rr(V, r1, r2); break;
    case VRSize8: emit_add64_rr(V, r1, r2); break;
    }
}

static void emit_add_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_add8_ri(V, r, i); break;
    case VRSize2: emit_add16_ri(V, r, i); break;
    case VRSize4: emit_add32_ri(V, r, i); break;
    case VRSize8: emit_add64_ri(V, r, i); break;
    }
}

static void emit_sub_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_sub8_rr(V, r1, r2); break;
    case VRSize2: emit_sub16_rr(V, r1, r2); break;
    case VRSize4: emit_sub32_rr(V, r1, r2); break;
    case VRSize8: emit_sub64_rr(V, r1, r2); break;
    }
}

static void emit_sub_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_sub8_ri(V, r, i); break;
    case VRSize2: emit_sub16_ri(V, r, i); break;
    case VRSize4: emit_sub32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_mul_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_mul8_r(V, r); break;
    case VRSize2: emit_mul16_r(V, r); break;
    case VRSize4: emit_mul32_r(V, r); break;
    case VRSize8: emit_mul64_r(V, r); break;
    }
}

static void emit_div_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_div8_r(V, r); break;
    case VRSize2: emit_div16_r(V, r); break;
    case VRSize4: emit_div32_r(V, r); break;
    case VRSize8: emit_div64_r(V, r); break;
    }
}

static void emit_idiv_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_idiv8_r(V, r); break;
    case VRSize2: emit_idiv16_r(V, r); break;
    case VRSize4: emit_idiv32_r(V, r); break;
    case VRSize8: emit_idiv64_r(V, r); break;
    }
}

static void emit_and_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_and8_rr(V, r1, r2); break;
    case VRSize2: emit_and16_rr(V, r1, r2); break;
    case VRSize4: emit_and32_rr(V, r1, r2); break;
    case VRSize8: emit_and64_rr(V, r1, r2); break;
    }
}

static void emit_and_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_and8_ri(V, r, i); break;
    case VRSize2: emit_and16_ri(V, r, i); break;
    case VRSize4: emit_and32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_or_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_or8_rr(V, r1, r2); break;
    case VRSize2: emit_or16_rr(V, r1, r2); break;
    case VRSize4: emit_or32_rr(V, r1, r2); break;
    case VRSize8: emit_or64_rr(V, r1, r2); break;
    }
}

static void emit_or_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_or8_ri(V, r, i); break;
    case VRSize2: emit_or16_ri(V, r, i); break;
    case VRSize4: emit_or32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_xor_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_xor8_rr(V, r1, r2); break;
    case VRSize2: emit_xor16_rr(V, r1, r2); break;
    case VRSize4: emit_xor32_rr(V, r1, r2); break;
    case VRSize8: emit_xor64_rr(V, r1, r2); break;
    }
}

static void emit_xor_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_xor8_ri(V, r, i); break;
    case VRSize2: emit_xor16_ri(V, r, i); break;
    case VRSize4: emit_xor32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_shl_ri(VRSize p, uint32_t r, uint8_t i)
{
    switch(p)
    {
    case VRSize1: emit_shl_r8i8(V, r, i); break;
    case VRSize2: emit_shl_r16i8(V, r, i); break;
    case VRSize4: emit_shl_r32i8(V, r, i); break;
    case VRSize8: emit_shl_r64i8(V, r, i); break;
    }
}

static void emit_shl_rcl(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_shl_r8cl(V, r); break;
    case VRSize2: emit_shl_r16cl(V, r); break;
    case VRSize4: emit_shl_r32cl(V, r); break;
    case VRSize8: emit_shl_r64cl(V, r); break;
    }
}

static void emit_shr_ri(VRSize p, uint32_t r, uint8_t i)
{
    switch(p)
    {
    case VRSize1: emit_shr8_ri(V, r, i); break;
    case VRSize2: emit_shr_r16i8(V, r, i); break;
    case VRSize4: emit_shr_r32i8(V, r, i); break;
    case VRSize8: emit_shr_r64i8(V, r, i); break;
    }
}

static void emit_shr_rcl(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_shr_r8cl(V, r); break;
    case VRSize2: emit_shr_r16cl(V, r); break;
    case VRSize4: emit_shr_r32cl(V, r); break;
    case VRSize8: emit_shr_r64cl(V, r); break;
    }
}

static void emit_inc_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_inc8_r(V, r); break;
    case VRSize2: emit_inc16_r(V, r); break;
    case VRSize4: emit_inc32_r(V, r); break;
    case VRSize8: emit_inc64_r(V, r); break;
    }
}

static void emit_dec_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_dec8_r(V, r); break;
    case VRSize2: emit_dec16_r(V, r); break;
    case VRSize4: emit_dec32_r(V, r); break;
    case VRSize8: emit_dec64_r(V, r); break;
    }
}

static void emit_neg_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_neg8_r(V, r); break;
    case VRSize2: emit_neg16_r(V, r); break;
    case VRSize4: emit_neg32_r(V, r); break;
    case VRSize8: emit_neg64_r(V, r); break;
    }
}

static void emit_not_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_not8_r(V, r); break;
    case VRSize2: emit_not16_r(V, r); break;
    case VRSize4: emit_not32_r(V, r); break;
    case VRSize8: emit_not64_r(V, r); break;
    }
}

static void emit_test_r(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_test8_rr(V, r1, r2); break;
    case VRSize2: emit_test16_rr(V, r1, r2); break;
    case VRSize4: emit_test32_rr(V, r1, r2); break;
    case VRSize8: emit_test64_rr(V, r1, r2); break;
    }
}

static void emit_cmp_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_cmp8_ri(V, r, i); break;
    case VRSize2: emit_cmp16_ri(V, r, i); break;
    case VRSize4: emit_cmp32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_cmp_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_cmp8_rr(V, r1, r2); break;
    case VRSize2: emit_cmp16_rr(V, r1, r2); break;
    case VRSize4: emit_cmp32_rr(V, r1, r2); break;
    case VRSize8: emit_cmp64_rr(V, r1, r2);break;
    }
}

static void emit_mov(VReg* dst, VReg* src)
{
    if(vrf_flo(dst))
    {
        vp_assertX(!vrf_const(src), "const src1");
        if(!vrf_flo(src))
        {
            switch(src->vsize)
            {
            case VRSize4: emit_movq_xr(V, dst->phys, src->phys); break;
            case VRSize8: emit_movd_xr(V, dst->phys, src->phys); break;
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
            if(src != dst)
            {
                emit_mov_rr(pow, dst->phys, src->phys);
            }
        }
    }
}

static void sel_bofs(IR* ir)
{
    int64_t ofs = ir->bofs.fi->ofs + ir->bofs.ofs;
    emit_lea64_rm(V, ir->dst->phys, MEM(RBP, NOREG, 1, ofs));
}

static void sel_iofs(IR* ir)
{
    int64_t ofs = ir->iofs.ofs;
    emit_lea64_rm(V, ir->dst->phys, MEM(RIP, NOREG, 1, ofs));
}

static void sel_sofs(IR* ir)
{
    int64_t ofs = ir->sofs.ofs;
    emit_lea64_rm(V, ir->dst->phys, MEM(RSP, NOREG, 1, ofs));
}

static void sel_mov(IR* ir)
{
    emit_mov(ir->dst, ir->src1);
}

static void sel_store(IR* ir)
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

static void sel_load(IR* ir)
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

static void sel_ret(IR* ir)
{
    emit_mov(ir->src1, ir->src1);
}

static void sel_pusharg(IR* ir)
{
    if(vrf_flo(ir->src1))
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        uint32_t dst = winx64_ra.fmap[ir->arg.idx];
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
        uint32_t dst = winx64_ra.imap[ir->arg.idx];
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

static vec_t(PatchInfo) jmps = NULL;
static vec_t(PatchInfo) calls = NULL;

static void patchinfo_add(BB* target, uint32_t ofs)
{
    PatchInfo pi = {NULL, target, ofs};
    vec_push(jmps, pi);
}

static void sel_call(IR* ir)
{
    /* Save caller registers */

    if(ir->call->label)
    {
        uint32_t ofs = sbuf_len(&V->code) + 1;
        emit_call_rel32(V, 0);
        PatchInfo pi = {ir->call->fn, NULL, ofs};
        vec_push(calls, pi);
    }
    else
    {
        vp_assertX(!vrf_const(ir->src1), "const src1");
        emit_call64_r(V, ir->src1->phys);
    }

    /* Restore caller registers */

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

static void sel_cast(IR* ir)
{
    UNUSED(ir);
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

static void sel_cond(IR* ir)
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

static void sel_jmp(IR* ir)
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
        patchinfo_add(ir->jmp.bb, patch);
        return;
    }

    cmp_vregs(src1, src2, cond);

    uint32_t patch = sbuf_len(&V->code) + 2;
    X64CC cc = cond2cc(cond);
    emit_jcc_rel32(V, cc, 0);
    patchinfo_add(ir->jmp.bb, patch);
}

static void sel_add(IR* ir)
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
            default:
                emit_add_ri(p, dst, ir->src2->i64);
                break;
            }
        }
        else
        {
            emit_add_rr(p, dst, ir->src2->phys);
        }
    }
}

static void sel_sub(IR* ir)
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
            default:
                emit_sub_ri(p, dst, ir->src2->i64);
                break;
            }
        }
        else
        {
            emit_sub_rr(p, dst, ir->src2->phys);
        }
    }
}

static void sel_mul(IR* ir)
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

static void sel_div(IR* ir)
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

static void sel_mod(IR* ir)
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

static void sel_band(IR* ir)
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

static void sel_bor(IR* ir)
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

static void sel_bxor(IR* ir)
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

static void sel_lshift(IR* ir)
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

static void sel_rshift(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->src1), "const src1");
    VRSize p = ir->dst->vsize;
    if(vrf_const(ir->src2))
    {
        emit_shr_ri(p, ir->dst->phys, ir->src2->i64 & 255);
    }
    else
    {
        vp_assertX(ir->src2->phys != RCX, "src2 == RCX");
        vp_assertX(ir->dst->phys != RCX, "dst == RCX");
        emit_mov8_rr(V, RCX, ir->src2->phys);
        emit_shr_rcl(p, ir->dst->phys);
    }
}

static void sel_neg(IR* ir)
{
    vp_assertX(vrf_const(ir->dst), "const dst");
    if(vrf_const(ir->src1))
    {
        vp_assertX(0, "not implemented");
    }
    else
    {
        vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
        VRSize p = ir->dst->vsize;
        vp_assertVSize(p, VRSize1, VRSize8);
        emit_neg_r(p, ir->dst->phys);
    }
}

static void sel_bnot(IR* ir)
{
    vp_assertX(ir->dst->phys == ir->src1->phys, "dst != src1");
    vp_assertX(!vrf_const(ir->dst), "const dst");
    VRSize p = ir->dst->vsize;
    vp_assertVSize(p, VRSize1, VRSize8);
    emit_not_r(p, ir->dst->phys);
}

typedef void (*SelIRFn)(IR* ir);
static const SelIRFn seltab[] = {
    [IR_BOFS] = sel_bofs,
    [IR_IOFS] = sel_iofs,
    [IR_SOFS] = sel_sofs,
    [IR_MOV] = sel_mov,
    [IR_STORE] = sel_store, [IR_LOAD] = sel_load,
    [IR_STORE_S] = sel_store, [IR_LOAD_S] = sel_load,
    [IR_RET] = sel_ret,
    [IR_COND] = sel_cond,
    [IR_JMP] = sel_jmp,
    [IR_PUSHARG] = sel_pusharg,
    [IR_CALL] = sel_call,
    [IR_CAST] = sel_cast,
    [IR_ADD] = sel_add, [IR_SUB] = sel_sub,
    [IR_MUL] = sel_mul, [IR_DIV] = sel_div, [IR_MOD] = sel_mod,
    [IR_BAND] = sel_band, [IR_BOR] = sel_bor, [IR_BXOR] = sel_bxor,
    [IR_LSHIFT] = sel_lshift, [IR_RSHIFT] = sel_rshift,
    [IR_NEG] = sel_neg, [IR_BNOT] = sel_bnot,
};

static void sel_ir(IR* ir)
{
    vp_assertX(ir->kind < (int)ARRSIZE(seltab), "out of bounds ir kind");
    vp_assertX(seltab[ir->kind], "empty entry %d", ir->kind);
    return (*seltab[ir->kind])(ir);
}

static uint32_t tmp_fmov(VReg** vp, vec_t(IR*)* irs, uint32_t pos)
{
    VReg* v = *vp;
    vp_assertX(vrf_const(v) && (v->flag & VRF_FLO), "not const float");

    uint64_t bits = 0;
    if(v->vsize == VRSize4)
    {
        float f = (float)v->n;
        memcpy(&bits, &f, sizeof(float));
    }
    else
    {
        double d = v->n;
        memcpy(&bits, &d, sizeof(double));
    }
    v = vp_vreg_ki(bits, v->vsize);
    VReg* tmp1 = vp_ra_spawn(v->vsize, 0);
    IR* mov1 = vp_ir_mov(tmp1, v, 0);
    vec_insert(*irs, pos, mov1); pos++;

    VReg* tmp2 = vp_ra_spawn(v->vsize, VRF_FLO);
    IR* mov2 = vp_ir_mov(tmp2, tmp1, 0);
    vec_insert(*irs, pos, mov2); pos++;

    *vp = tmp2;

    return pos;
}

static void tmp_mov(VReg** vp, vec_t(IR*)* irs, uint32_t pos, uint8_t flag)
{
    VReg* v = *vp;
    VReg* tmp = vp_ra_spawn(v->vsize, v->flag);
    IR* mov = vp_ir_mov(tmp, v, flag);
    vec_insert(*irs, pos, mov);
    *vp = tmp;
}

/* Convert `A = B op C` to `A = B; A = A op C` */
static void sel_conv3to2(vec_t(BB*) bbs)
{
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            switch(ir->kind)
            {
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
void vp_sel_tweak(Code* c)
{
    vec_t(BB*) bbs = c->bbs;
    sel_conv3to2(bbs);
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
                        j = tmp_fmov(vp, &bb->irs, j);
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
                        tmp_mov(vp, &bb->irs, j++, ir->flag);
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
                    tmp_mov(&ir->src2, &bb->irs, j++, ir->flag);
                }
                break;
            case IR_CALL:
                if(ir->src1 && vrf_const(ir->src1))
                {
                    tmp_mov(&ir->src1, &bb->irs, j++, ir->flag);
                }
            default: break;
            }
        }
    }
}

static uint32_t push_callee_save(RegAlloc* ra, RegSet used)
{
    uint32_t count = 0;
    for(uint32_t i = 0; i < ra->set->iphysmax; i++)
    {
        if((ra->set->itemp & (1ULL << i)) && (used & (1ULL << i)))
        {
            emit_push64_r(V, i);
            count++;
        }
    }
    return count;
}

static void pop_callee_save(RegAlloc* ra, RegSet used)
{
    for(uint32_t i = ra->set->iphysmax; i-- > 0;)
    {
        if((ra->set->itemp & (1ULL << i)) && (used & (1ULL << i)))
        {
            emit_pop64_r(V, i);
        }
    }
}

static void assign_params(RegAlloc* ra, Code* c)
{
    for(uint32_t i = 0; i < c->numparams; i++)
    {
        VarInfo* vi = c->scopes[0]->vars[i];
        VReg* vr = vi->vreg;
        if(i < 4)
        {
            uint32_t preg = ra->set->imap[i];
            VRSize p = vr->vsize;
            if(preg != vr->phys)
            {
                emit_mov_rr(p, vr->phys, preg);
            }
        }
    }
}

static void patch_infos(PatchInfo* patch, bool iscall)
{
    for(uint32_t i = 0; i < vec_len(patch); i++)
    {
        PatchInfo* p = &patch[i];

        int32_t from = p->ofs;
        int32_t ofs;
        if(iscall)
        {
            ofs = p->c->ofs;
        }
        else
        {
            ofs = p->target->ofs;
        }
        
        int32_t rel = ofs - (from + 4);
        
        uint8_t* code = (uint8_t*)(V->code.b + p->ofs);
        *(uint32_t*)code = (uint32_t)rel;
    }
}

static void emit_bbs(BB** bbs)
{
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        bb->ofs = sbuf_len(&V->code);
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            sel_ir(ir);
        }
    }
}

static void emit_body(Code* c)
{
    RegAlloc* ra = c->ra;
    c->ofs = sbuf_len(&V->code);

    /* Prologue */
    bool saverbp = false;
    uint32_t framesize = 0;
    uint32_t numcallee = 0;
    numcallee = push_callee_save(ra, ra->iregbits);
    if(c->framesize > 0 || ra->flag & RAF_STACK_FRAME)
    {
        emit_push64_r(V, RBP);
        emit_mov64_rr(V, RBP, RSP);
        saverbp = true;
    }

    framesize = c->framesize;

    if(framesize > 0)
    {
        emit_sub_r64i32(V, RSP, framesize);
    }
    assign_params(ra, c);

    emit_bbs(c->bbs);

    /* Epilogue */
    if(framesize > 0)
    {
        emit_add64_ri(V, RSP, framesize);
    }
    if(saverbp)
    {
        emit_mov64_rr(V, RSP, RBP);
        emit_pop64_r(V, RBP);
    }
    pop_callee_save(ra, ra->iregbits);
    emit_ret(V);
}

void vp_sel(Code** codes)
{
    for(uint32_t i = 0; i < vec_len(codes); i++)
    {
        emit_body(codes[i]);
    }
    
    patch_infos(jmps, false);
    patch_infos(calls, true);
}