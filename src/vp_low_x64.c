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
#include "vp_buf.h"
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
        VReg* vr = vi->vreg;
        if(!vr)
            continue;

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
                    X64Reg dst = X64REG(vr->phys, RC_GPR, 8, SUB_LO);
                    X64Reg src = X64REG(pl->idx, RC_GPR, 8, SUB_LO);
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
            case PC_MEM:
                /* Already on stack */
                break;
            default:
                vp_assertX(0, "invalid param class");
        }
    }
}

typedef struct RegSave
{
    X64Reg reg;
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
        RegSave rs = {.reg = X64REG(ireg, RC_GPR, 8, SUB_LO)};
        vec_push(saves, rs);
        isave &= isave - 1;  /* Clear lowest bit */
    }

    /* Collect living float regs */
    while(fsave)
    {
        uint32_t freg = __builtin_ctzll(fsave);
        RegSave rs = {.reg = X64REG(freg, RC_XMM, 16, SUB_LO)};
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

/* Get bottom of the stack for paramaters */
uint32_t vp_lowX64_stack_params(Code* code)
{
    const ABIInfo* abi = code->abi;
    uint32_t paramofs = TARGET_PTR_SIZE * 2;  /* Return address */

    if(abi->flags & ABI_SHADOW)
        paramofs += 32; /* Windows x64 shadow space */

    RegAlloc* ra = code->ra;
    vp_assertX(ra, "missing regalloc");
    RegSet calleemask = ra->itemp & ra->iregbits;
    uint32_t numcallee = __builtin_popcountll(calleemask);
    paramofs += numcallee * TARGET_PTR_SIZE;    /* RBP is also callee saved */

    return paramofs;
}

/* Push caller-save registers */
void vp_lowX64_caller_push(Code* code, vec_t(RegSave) saves, uint32_t total)
{
    /* Determine ABI to use */
    const ABIInfo* abi = code ? code->abi : V->T->abi;
    uint32_t shadow = (abi->flags & ABI_SHADOW) ? 32 : 0;

    uint32_t ofs = total + shadow;
    ofs += vec_len(saves) * TARGET_PTR_SIZE;
    for(uint32_t i = 0; i < vec_len(saves); i++)
    {
        RegSave* rs = &saves[i];
        ofs -= TARGET_PTR_SIZE;
        X64Mem mem = X64MEM(RN_SP, NOREG, 1, ofs, 8);
        if(REG_CLASS(rs->reg) == RC_XMM)
        {
            EMITX64(movsdMX)(mem, rs->reg);
        }
        else
        {
            EMITX64(movMR)(mem, rs->reg);
        }
    }
}

/* Pop caller-save registers */
void vp_lowX64_caller_pop(Code* code, vec_t(RegSave) saves, uint32_t ofs)
{
    /* Determine ABI to use */
    const ABIInfo* abi = code ? code->abi : V->T->abi;
    uint32_t shadow = (abi->flags & ABI_SHADOW) ? 32 : 0;

    ofs += shadow;
    for(uint32_t i = vec_len(saves); i-- > 0;)
    {
        RegSave* rs = &saves[i];
        X64Mem mem = X64MEM(RN_SP, NOREG, 1, ofs, 8);
        if(REG_CLASS(rs->reg) == RC_XMM)
        {
            EMITX64(movsdXM)(rs->reg, mem);
        }
        else
        {
            EMITX64(movRM)(rs->reg, mem);
        }
        ofs += TARGET_PTR_SIZE;
    }
}

/* Push callee-save GPRs (ascending order) */
static void lowX64_icallee_push(RegSet imask, uint32_t imax)
{
    for(uint32_t i = 0; i < imax; i++)
    {
        if(imask & (1ULL << i))
        {
            EMITX64(pushR)(X64REG(i, RC_GPR, 8, SUB_LO));
        }
    }
}

/* Store callee-save XMMs (ascending order) */
static uint32_t lowX64_fcallee_push(RegSet fmask, uint32_t fmax)
{
    uint32_t ofs = 0;
    for(uint32_t i = 0; i < fmax; i++)
    {
        if(fmask & (1ULL << i))
        {
            X64Reg xmmreg = X64REG(i, RC_XMM, 16, SUB_LO);
            X64Mem mem = X64MEM(RN_SP, NOREG, 1, ofs, 8);
            EMITX64(movdqaMX)(mem, xmmreg);
            ofs += 16;
        }
    }
    return ofs;
}

/* Pop callee-save GPRs (descending order) */
static void lowX64_icallee_pop(RegSet imask, uint32_t imax)
{
    for(uint32_t i = imax; i-- > 0;)
    {
        if(imask & (1ULL << i))
        {
            EMITX64(popR)(X64REG(i, RC_GPR, 8, SUB_LO));
        }
    }
}

/* Load callee-save XMMs (descending order) */
static void lowX64_fcallee_pop(RegSet fmask, uint32_t fmax)
{
    uint32_t ofs = 0;
    for(uint32_t i = 0; i < fmax; i++)
    {
        if(fmask & (1ULL << i))
        {
            X64Reg xmmreg = X64REG(i, RC_XMM, 16, SUB_LO);
            X64Mem mem = X64MEM(RN_SP, NOREG, 1, ofs, 8);
            EMITX64(movdqaXM)(xmmreg, mem);
            ofs += 16;
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
    uint32_t frameofs = 8;
    uint32_t shadow = (code->abi->flags & ABI_SHADOW) ? 32 : 0;

    uint32_t stacksize = lowX64_call_size(code);
    code->stacksize = stacksize;

    /* Compute callee-save masks */
    RegSet imask = ra->itemp & ra->iregbits;
    RegSet fmask = ra->ftemp & ra->fregbits;
    uint32_t icallee = __builtin_popcountll(imask);
    uint32_t fcallee = __builtin_popcountll(fmask);

    /* Prologue */
    {
        /* Push callee-save GPRs and store callee-save XMMs */
        lowX64_icallee_push(imask, ra->iphysmax);
        if(code->framesize > 0 || raf_stackframe(ra))
        {
            EMITX64(pushR)(RBP);
            EMITX64(movRR)(RBP, RSP);
            saverbp = true;
            /* RBP pushed, 16 bytes align becomes 0 */
            frameofs = 0;
        }

        framesize = code->framesize + stacksize;
        if(vec_len(code->calls) > 0 || stacksize || (fcallee > 0))
        {
            /* Align frame size to 16 */
            size_t calleesize = (icallee * TARGET_PTR_SIZE);
            framesize += (fcallee * 16);
            framesize += shadow;    /* Shadow space */
            framesize += -(framesize + calleesize + frameofs) & 15;
        }

        if(framesize > 0)
        {
            EMITX64(subRI)(RSP, framesize);
        }
        lowX64_fcallee_push(fmask, ra->fphysmax);
        lowX64_params(code);
    }

    V->fncode = code;
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
        lowX64_fcallee_pop(fmask, ra->fphysmax);
        if(framesize > 0)
        {
            EMITX64(addRI)(RSP, framesize);
        }
        if(saverbp)
        {
            EMITX64(movRR)(RSP, RBP);
            EMITX64(popR)(RBP);
        }
        lowX64_icallee_pop(imask, ra->iphysmax);
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