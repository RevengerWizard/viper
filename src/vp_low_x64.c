/*
** vp_low_x64.c
** IR x64 lowering
*/

#include "vp_low.h"
#include "vp_regalloc.h"
#include "vp_target.h"
#include "vp_emit_x64.h"
#include "vp_codegen.h"
#include "vp_ir.h"

typedef struct RegParam
{
    Type* ty;
    VReg* vr;
    uint32_t idx;
} RegParam;

static void params_enum(Code* code, RegParam iargs[], RegParam fargs[], uint32_t imax, uint32_t fmax, uint32_t* icount, uint32_t* fcount)
{
    uint32_t inums = 0;
    uint32_t fnums = 0;

    VReg* retvr = code->retvr;
    if(retvr)
    {
        RegParam* rp = &iargs[inums++];
        rp->ty = vp_type_ptr(tyvoid);
        rp->vr = retvr;
        rp->idx = 0;
    }

    for(uint32_t i = 0; i < code->numparams; i++)
    {
        VarInfo* vi = code->scopes[0]->vars[i];
        Type* ty = vi->type;
        if(param_isstack(ty))
            continue;
        VReg* vr = vi->vreg;
        vp_assertX(vr, "empty vreg");
        RegParam* rp = NULL;
        uint32_t idx = 0;
        if(ty_isflo(ty))
        {
            if(fnums < fmax)
                rp = &fargs[idx = fnums++];
        }
        else
        {
            if(inums < imax)
                rp = &iargs[idx = inums++];
        }
        if(rp)
        {
            rp->ty = ty;
            rp->vr = vr;
            rp->idx = idx;
        }
    }

    *icount = inums;
    *fcount = fnums;
}

static void params_assign(RegAlloc* ra, Code* code)
{
    RegParam iparams[10] = {};
    RegParam fparams[10] = {};
    uint32_t inums = 0;
    uint32_t fnums = 0;
    params_enum(code, iparams, fparams, 4, 4, &inums, &fnums);

    /* Int parameter stores */
    for(uint32_t i = 0; i < inums; i++)
    {
        RegParam* rp = &iparams[i];
        VReg* vr = rp->vr;
        uint32_t size = vp_type_sizeof(rp->ty);
        VRSize p = vp_msb(size);
        uint32_t src = ra->set->imap[rp->idx];
        if(vrf_spill(vr))
        {
            uint32_t ofs = vr->fi.ofs;
            vp_assertX(ofs, "0 offset");
            emit_mov64_mr(V, MEM(RBP, NOREG, 1, ofs), src);
        }
        else if(src != vr->phys)
        {
            emit_mov_rr(p, vr->phys, src);
        }
    }

    /* Float parameter stores */
    for(uint32_t i = 0; i < fnums; i++)
    {
        RegParam* rp = &fparams[i];
        VReg* vr = rp->vr;
        uint32_t src = ra->set->imap[rp->idx];
        if(vrf_spill(vr))
        {
            uint32_t ofs = vr->fi.ofs;
            vp_assertX(ofs, "0 offset");
            switch(vr->vsize)
            {
                case VRSize4: emit_movss_mr(V, MEM(RBP, NOREG, 1, ofs), src); break;
                case VRSize8: emit_movsd_mr(V, MEM(RBP, NOREG, 1, ofs), src); break;
                default: vp_assertX(0, "?");
            }
        }
        else if(src != vr->phys)
        {
            uint32_t dst = vr->phys;
            switch(vr->vsize)
            {
                case VRSize4: emit_movss_rr(V, dst, src); break;
                case VRSize8: emit_movsd_rr(V, dst, src); break;
                default: vp_assertX(0, "?");
            }
        }
    }
}

typedef struct RegSave
{
    uint32_t reg;
    bool flo;
} RegSave;

static vec_t(RegSave) collect_caller_save(RegSet iliving, RegSet fliving)
{
    vec_t(RegSave) saves = NULL;

    for(uint32_t i = 0; i < V->target->abiinfo->icallersize; i++)
    {
        uint32_t ireg = V->target->abiinfo->icaller[i];
        if(iliving & (1ULL << ireg))
        {
            RegSave rs = {.reg = ireg};
            vec_push(saves, rs);
        }
    }

    for(uint32_t i = 0; i < V->target->abiinfo->fcallersize; i++)
    {
        uint32_t freg = V->target->abiinfo->fcaller[i];
        if(fliving & (1ULL << freg))
        {
            RegSave rs = {.reg = freg, .flo = true};
            vec_push(saves, rs);
        }
    }

    return saves;
}

static uint32_t detect_call_size(Code* code)
{
    uint32_t max = 0;
    if(code->calls)
    {
        for(uint32_t i = 0; i < vec_len(code->calls); i++)
        {
            IR* ir = code->calls[i];

            /* Caller save registers */
            vec_t(RegSave) saves = collect_caller_save(ir->call->ipregs, ir->call->fpregs);
            ir->call->saves = saves;

            uint32_t total = ir->call->stacksize + vec_len(saves) * TARGET_PTR_SIZE;
            max = MAX(max, total);
        }
    }
    return max;
}

void push_caller_save(vec_t(RegSave) saves, uint32_t total)
{
    uint32_t ofs = total;
    ofs += vec_len(saves) * TARGET_PTR_SIZE;
    for(uint32_t i = 0; i < vec_len(saves); i++)
    {
        RegSave* rs = &saves[i];
        ofs -= TARGET_PTR_SIZE;
        if(rs->flo)
        {
            emit_movsd_mr(V, MEM(RSP, NOREG, 1, ofs), rs->reg);
        }
        else
        {
            emit_mov64_mr(V, MEM(RSP, NOREG, 1, ofs), rs->reg);
        }
    }
}

void pop_caller_save(vec_t(RegSave) saves, uint32_t ofs)
{
    for(uint32_t i = vec_len(saves); i-- > 0;)
    {
        RegSave* rs = &saves[i];
        if(rs->flo)
        {
            emit_movsd_mr(V, rs->reg, MEM(RSP, NOREG, 1, ofs));
        }
        else
        {
            emit_mov64_rm(V, rs->reg, MEM(RSP, NOREG, 1, ofs));
        }
        ofs += TARGET_PTR_SIZE;
    }
}

static uint32_t push_callee_save(RegAlloc* ra, RegSet iused)
{
    uint32_t inum = 0;
    for(uint32_t i = 0; i < ra->set->iphysmax; i++)
    {
        if((ra->set->itemp & (1ULL << i)) && (iused & (1ULL << i)))
        {
            emit_push64_r(V, i);
            inum++;
        }
    }
    return inum;
}

static void pop_callee_save(RegAlloc* ra, RegSet iused)
{
    for(uint32_t i = ra->set->iphysmax; i-- > 0;)
    {
        if((ra->set->itemp & (1ULL << i)) && (iused & (1ULL << i)))
        {
            emit_pop64_r(V, i);
        }
    }
}

static void emit_body(Code* code)
{
    RegAlloc* ra = code->ra;
    code->ofs = sbuf_len(&V->code);

    bool saverbp = false;
    uint32_t framesize = 0;
    uint32_t numcallee = 0;
    uint32_t frameofs = 8;

    uint32_t stacksize = detect_call_size(code);
    code->stacksize = stacksize;

    /* Prologue */
    {
        /* Callee save */
        numcallee = push_callee_save(ra, ra->iregbits);
        if(code->framesize > 0 || raf_stackframe(ra))
        {
            emit_push64_r(V, RBP);
            emit_mov64_rr(V, RBP, RSP);
            saverbp = true;
            /* RBP pushed, 16 bytes align becomes 0 */
            frameofs = 0;
        }

        framesize = code->framesize + stacksize;
        if(code->calls || stacksize)
        {
            /* Align frame size to 16 */
            size_t calleesize = numcallee * TARGET_PTR_SIZE;
            framesize += -(framesize + calleesize + frameofs) & 15;
            framesize += 32;    /* Shadow space? */
        }

        if(framesize > 0)
        {
            emit_sub_r64i32(V, RSP, framesize);
        }
        params_assign(ra, code);
    }

    /* Lower basic block IR */
    for(uint32_t i = 0; i < vec_len(code->bbs); i++)
    {
        BB* bb = code->bbs[i];
        bb->ofs = sbuf_len(&V->code);
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            vp_ir_x64(ir);
        }
    }

    /* Epilogue */
    {
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
}

void vp_low(vec_t(Code*) codes)
{
    for(uint32_t i = 0; i < vec_len(codes); i++)
    {
        if(codes[i]->bbs)
        {
            emit_body(codes[i]);
        }
    }
}