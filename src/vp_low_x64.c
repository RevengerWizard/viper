/*
** vp_low_x64.c
** IR x64 lowering
*/

#include "vp_low.h"
#include "vp_abi.h"
#include "vp_regalloc.h"
#include "vp_target.h"
#include "vp_emit_x64.h"
#include "vp_codegen.h"
#include "vp_ir.h"
#include "vp_target_x64.h"

/* Assign saved param registers */
static void lowX64_params(Code* code)
{
    uint32_t start = code->retvr ? 1 : 0;

    /* Handle return value parameter */
    if(code->retvr)
    {
        VReg* vr = code->retvr;
        ParamLoc* pl = &code->plocs[0];
        vp_assertX(pl->cls == PC_IREG, "return ptr must be in int reg");

        if(vrf_spill(vr))
        {
            uint32_t ofs = vr->fi.ofs;
            vp_assertX(ofs, "0 offset");
            EMITX64(movMR)(X64MEM(RN_BP, NOREG, 1, ofs, 8),
                          X64REG(pl->idx, RC_GPR, 8, SUB_LO));
        }
        else if(pl->idx != vr->phys)
        {
            X64Reg dst = X64REG(vr->phys, RC_GPR, 8, SUB_LO);
            X64Reg src = X64REG(pl->idx, RC_GPR, 8, SUB_LO);
            EMITX64(movRR)(dst, src);
        }
    }

    /* Handle function parameters */
    for(uint32_t i = 0; i < code->numparams; i++)
    {
        VarInfo* vi = code->scopes[0]->vars[i];
        ParamLoc* pl = &code->plocs[i + start];

        /* Skip memory parameters (already on stack) */
        if(pl->cls == PC_MEM)
            continue;

        VReg* vr = vi->vreg;
        if(!vr)
            continue;

        Type* ty = vi->type;
        uint32_t size = vp_type_sizeof(ty);
        VRSize p = vp_msb(size);

        switch(pl->cls)
        {
            case PC_IREG:
            {
                if(vrf_spill(vr))
                {
                    uint32_t ofs = vr->fi.ofs;
                    vp_assertX(ofs, "0 offset");
                    EMITX64(movMR)(X64MEM(RN_BP, NOREG, 1, ofs, 8),
                                  X64REG(pl->idx, RC_GPR, 8, SUB_LO));
                }
                else if(pl->idx != vr->phys)
                {
                    X64Reg dst = X64REG(vr->phys, RC_GPR, p, SUB_LO);
                    X64Reg src = X64REG(pl->idx, RC_GPR, p, SUB_LO);
                    EMITX64(movRR)(dst, src);
                }
                break;
            }
            case PC_FREG:
            {
                X64Reg src = X64REG(pl->idx, RC_XMM, 16, SUB_LO);

                if(vrf_spill(vr))
                {
                    uint32_t ofs = vr->fi.ofs;
                    vp_assertX(ofs, "0 offset");
                    switch(vr->vsize)
                    {
                    case VRSize4:
                        EMITX64(movssMX)(X64MEM(RN_BP, NOREG, 1, ofs, 8), src);
                        break;
                    case VRSize8:
                        EMITX64(movsdMX)(X64MEM(RN_BP, NOREG, 1, ofs, 8), src);
                        break;
                    default:
                        vp_assertX(0, "invalid float size");
                    }
                }
                else if(pl->idx != vr->phys)
                {
                    X64Reg dst = X64REG(vr->phys, RC_XMM, 16, SUB_LO);
                    switch(vr->vsize)
                    {
                    case VRSize4:
                        EMITX64(movssXX)(dst, src);
                        break;
                    case VRSize8:
                        EMITX64(movsdXX)(dst, src);
                        break;
                    default:
                        vp_assertX(0, "invalid float size");
                    }
                }
                break;
            }
            case PC_STACK:
                /* Already on stack */
                break;
            default:
                vp_assertX(0, "invalid param class");
        }
    }
}

typedef struct RegSave
{
    uint32_t reg;
    bool flo;
} RegSave;

/* Collect caller saved living registers */
static vec_t(RegSave) lowX64_caller_save(RegSet iliving, RegSet fliving)
{
    vec_t(RegSave) saves = vec_init(RegSave);
    RegSet icaller = V->T->abi->icaller;
    RegSet fcaller = V->T->abi->fcaller;

    /* Intersect living with caller-saved */
    RegSet isave = iliving & icaller;
    RegSet fsave = fliving & fcaller;

    /* Collect living int regs */
    while(isave)
    {
        uint32_t ireg = __builtin_ctzll(isave);
        RegSave rs = {.reg = ireg};
        vec_push(saves, rs);
        isave &= isave - 1;  /* Clear lowest bit */
    }

    /* Collect living float regs */
    while(fsave)
    {
        uint32_t freg = __builtin_ctzll(fsave);
        RegSave rs = {.reg = freg, .flo = true};
        vec_push(saves, rs);
        fsave &= fsave - 1; /* Clear lowest bit */
    }

    return saves;
}

/* Detect maximum call stack requirement */
static uint32_t lowX64_call_size(Code* code)
{
    uint32_t max = 0;
    for(uint32_t i = 0; i < vec_len(code->calls); i++)
    {
        IR* ir = code->calls[i];

        /* Caller save registers */
        vec_t(RegSave) saves = lowX64_caller_save(ir->call->ipregs, ir->call->fpregs);
        ir->call->saves = saves;

        uint32_t total = ir->call->stacksize + vec_len(saves) * TARGET_PTR_SIZE;
        max = MAX(max, total);
    }
    return max;
}

/* Push caller-save registers */
void vp_lowX64_caller_push(vec_t(RegSave) saves, uint32_t total)
{
    uint32_t ofs = total;
    ofs += vec_len(saves) * TARGET_PTR_SIZE;
    for(uint32_t i = 0; i < vec_len(saves); i++)
    {
        RegSave* rs = &saves[i];
        ofs -= TARGET_PTR_SIZE;
        X64Mem mem = X64MEM(RN_SP, NOREG, 1, ofs, 8);
        if(rs->flo)
        {
            EMITX64(movsdMX)(mem, X64REG(rs->reg, RC_XMM, 16, SUB_LO));
        }
        else
        {
            EMITX64(movMR)(mem, X64REG(rs->reg, RC_GPR, 8, SUB_LO));
        }
    }
}

/* Pop caller-save registers */
void vp_lowX64_caller_pop(vec_t(RegSave) saves, uint32_t ofs)
{
    for(uint32_t i = vec_len(saves); i-- > 0;)
    {
        RegSave* rs = &saves[i];
        X64Mem mem = X64MEM(RN_SP, NOREG, 1, ofs, 8);
        if(rs->flo)
        {
            EMITX64(movsdXM)(X64REG(rs->reg, RC_XMM, 16, SUB_LO), mem);
        }
        else
        {
            EMITX64(movRM)(X64REG(rs->reg, RC_GPR, 8, SUB_LO), mem);
        }
        ofs += TARGET_PTR_SIZE;
    }
}

/* Push callee-save registers */
static void lowX64_callee_push(RegSet mask, uint32_t max)
{
    for(uint32_t i = 0; i < max; i++)
    {
        if(mask & (1ULL << i))
        {
            EMITX64(pushR)(X64REG(i, RC_GPR, 8, SUB_LO));
        }
    }
}

/* Pop callee-save registers */
static void lowX64_callee_pop(RegSet mask, uint32_t max)
{
    for(uint32_t i = max; i-- > 0;)
    {
        if(mask & (1ULL << i))
        {
            EMITX64(popR)(X64REG(i, RC_GPR, 8, SUB_LO));
        }
    }
}

/* Lower code body to x64 */
static void lowX64_body(Code* code)
{
    RegAlloc* ra = code->ra;
    code->ofs = sbuf_len(&V->code);

    bool saverbp = false;
    uint32_t framesize = 0;
    uint32_t numcallee = 0;
    uint32_t frameofs = 8;
    uint32_t shadow = (code->abi->flags & ABI_SHADOW) ? 32 : 0;

    uint32_t stacksize = lowX64_call_size(code);
    code->stacksize = stacksize;

    /* Compute callee-save mask */
    RegSet calleemask = ra->itemp & ra->iregbits;
    numcallee = __builtin_popcountll(calleemask);

    /* Prologue */
    {
        /* Callee save */
        lowX64_callee_push(calleemask, ra->iphysmax);
        if(code->framesize > 0 || raf_stackframe(ra))
        {
            EMITX64(pushR)(RBP);
            EMITX64(movRR)(RBP, RSP);
            saverbp = true;
            /* RBP pushed, 16 bytes align becomes 0 */
            frameofs = 0;
        }

        framesize = code->framesize + stacksize;
        if(vec_len(code->calls) > 0 || stacksize)
        {
            /* Align frame size to 16 */
            size_t calleesize = numcallee * TARGET_PTR_SIZE;
            framesize += -(framesize + calleesize + frameofs) & 15;
            framesize += shadow;    /* Shadow space */
        }

        if(framesize > 0)
        {
            EMITX64(subRI)(RSP, framesize);
        }
        lowX64_params(code);
    }

    /* Lower basic block IRs */
    for(uint32_t i = 0; i < vec_len(code->bbs); i++)
    {
        BB* bb = code->bbs[i];
        bb->ofs = sbuf_len(&V->code);
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            vp_irX64(ir);
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
        lowX64_callee_pop(calleemask, ra->iphysmax);
        EMITX64(ret)();
    }
}

void vp_lowX64(vec_t(Code*) codes)
{
    for(uint32_t i = 0; i < vec_len(codes); i++)
    {
        if(codes[i]->bbs)
        {
            lowX64_body(codes[i]);
        }
    }
}