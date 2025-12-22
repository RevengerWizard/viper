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
#include "vp_target_x64.h"

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
        uint32_t idx = ra->set->imap[rp->idx];
        if(vrf_spill(vr))
        {
            uint32_t ofs = vr->fi.ofs;
            vp_assertX(ofs, "0 offset");
            EMITX64(movMR)(MEM_MAKE(RN_BP, NOREG, 1, ofs, 8), REG_MAKE(idx, RC_GPR, 8, SUB_LO));
        }
        else if(idx != vr->phys)
        {
            X64Reg dst = REG_MAKE(vr->phys, RC_GPR, p, SUB_LO);
            X64Reg src = REG_MAKE(idx, RC_GPR, p, SUB_LO);
            EMITX64(movRR)(dst, src);
        }
    }

    /* Float parameter stores */
    for(uint32_t i = 0; i < fnums; i++)
    {
        RegParam* rp = &fparams[i];
        VReg* vr = rp->vr;
        uint32_t idx = ra->set->fmap[rp->idx];
        X64Reg src = REG_MAKE(idx, RC_XMM, 16, SUB_LO);
        if(vrf_spill(vr))
        {
            uint32_t ofs = vr->fi.ofs;
            vp_assertX(ofs, "0 offset");
            switch(vr->vsize)
            {
            case VRSize4: EMITX64(movssMX)(MEM_MAKE(RN_BP, NOREG, 1, ofs, 8), src); break;
            case VRSize8: EMITX64(movsdMX)(MEM_MAKE(RN_BP, NOREG, 1, ofs, 8), src); break;
            default: vp_assertX(0, "?");
            }
        }
        else if(idx != vr->phys)
        {
            X64Reg dst = REG_MAKE(vr->phys, RC_XMM, 16, SUB_LO);
            switch(vr->vsize)
            {
            case VRSize4: EMITX64(movssXX)(dst, src); break;
            case VRSize8: EMITX64(movsdXX)(dst, src); break;
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
            EMITX64(movsdMX)(MEM_MAKE(RN_SP, NOREG, 1, ofs, 8), REG_MAKE(rs->reg, RC_XMM, 16, SUB_LO));
        }
        else
        {
            EMITX64(movMR)(MEM_MAKE(RN_SP, NOREG, 1, ofs, 8), REG_MAKE(rs->reg, RC_GPR, 8, SUB_LO));
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
            EMITX64(movsdXM)(REG_MAKE(rs->reg, RC_XMM, 16, SUB_LO), MEM_MAKE(RN_SP, NOREG, 1, ofs, 8));
        }
        else
        {
            EMITX64(movRM)(REG_MAKE(rs->reg, RC_GPR, 8, SUB_LO), MEM_MAKE(RN_SP, NOREG, 1, ofs, 8));
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
            X64Reg reg = REG_MAKE(i, RC_GPR, 8, SUB_LO);
            EMITX64(pushR)(reg);
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
            X64Reg reg = REG_MAKE(i, RC_GPR, 8, SUB_LO);
            EMITX64(popR)(reg);
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
            EMITX64(pushR)(RBP);
            EMITX64(movRR)(RBP, RSP);
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
            EMITX64(subRI)(RSP, framesize);
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
            EMITX64(addRI)(RSP, framesize);
        }
        if(saverbp)
        {
            EMITX64(movRR)(RSP, RBP);
            EMITX64(popR)(RBP);
        }
        pop_callee_save(ra, ra->iregbits);
        EMITX64(ret)();
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