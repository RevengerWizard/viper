/*
** vp_ir.c
** Intermediate Representation
*/

#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_codegen.h"
#include "vp_vec.h"
#include "vp_regalloc.h"

/* Viper IR instructions */
const char* const vp_ir_name[] = {
#define IRSTR(name, sp) #name,
    IRDEF(IRSTR)
#undef IRSTR
};

static IR* ir_new(IrKind kind)
{
    IR* ir = vp_arena_alloc(&V->irarena, sizeof(*ir));
    ir->kind = kind;
    ir->flag = 0;
    ir->dst = ir->src1 = ir->src2 = NULL;
    if(V->bb)
    {
        vec_push(V->bb->irs, ir);
    }
    return ir;
}

IR* vp_ir_bofs(FrameInfo* fi)
{
    IR* ir = ir_new(IR_BOFS);
    ir->dst = vp_ra_spawn(VRSize8, 0);
    ir->bofs.fi = fi;
    ir->bofs.ofs = 0;
    return ir;
}

IR* vp_ir_iofs(Str* label)
{
    IR* ir = ir_new(IR_IOFS);
    ir->iofs.ofs = 0;
    ir->iofs.label = label;
    ir->dst = vp_ra_spawn(VRSize8, 0);
    return ir;
}

IR* vp_ir_sofs(uint32_t ofs)
{
    IR* ir = ir_new(IR_SOFS);
    ir->dst = vp_ra_spawn(VRSize8, 0);
    ir->sofs.ofs = ofs;
    return ir;
}

IR* vp_ir_mov(VReg* dst, VReg* src, uint8_t flag)
{
    IR* ir = ir_new(IR_MOV);
    ir->flag = flag;
    ir->dst = dst;
    ir->src1 = src;
    return ir;
}

IR* vp_ir_store(VReg* dst, VReg* src, uint8_t flag)
{
    IR* ir = ir_new(IR_STORE);
    ir->flag = flag;
    ir->src1 = src;
    ir->src2 = dst;
    return ir;
}

IR* vp_ir_load(VReg* src, VRSize vsize, uint8_t vflag, uint8_t irflag)
{
    IR* ir = ir_new(IR_LOAD);
    ir->flag = irflag;
    ir->src1 = src;
    ir->dst = vp_ra_spawn(vsize, vflag);
    return ir;
}

IR* vp_ir_store_s(VReg* dst, VReg* src)
{
    IR* ir = ir_new(IR_STORE_S);
    ir->src1 = src;
    ir->src2 = dst;
    return ir;
}

IR* vp_ir_load_s(VReg* dst, VReg* src, uint8_t flag)
{
    IR* ir = ir_new(IR_LOAD_S);
    ir->flag = flag;
    ir->src1 = src;
    ir->dst = dst;
    return ir;
}

IR* vp_ir_ret(VReg* src, uint8_t flag)
{
    IR* ir = ir_new(IR_RET);
    ir->flag = flag;
    ir->src1 = src;
    return ir;
}

IR* vp_ir_cond(VReg* src1, VReg* src2, CondKind cond)
{
    IR* ir = ir_new(IR_COND);
    ir->src1 = src1;
    ir->src2 = src2;
    ir->dst = vp_ra_spawn(VRSize1, 0);
    ir->cond = cond;
    return ir;
}

IR* vp_ir_jmp(BB* bb)
{
    IR* ir = ir_new(IR_JMP);
    ir->jmp.bb = bb;
    ir->jmp.cond = COND_ANY;
    return ir;
}

/* Conditional jump */
void vp_ir_cjmp(VReg* src1, VReg* src2, CondKind cond, BB* bb)
{
    if((cond & COND_MASK) == COND_NONE)
        return;
    IR* ir = ir_new(IR_JMP);
    ir->src1 = src1;
    ir->src2 = src2;
    ir->jmp.bb = bb;
    ir->jmp.cond = cond;
}

IR* vp_ir_pusharg(VReg* src, uint32_t idx)
{
    IR* ir = ir_new(IR_PUSHARG);
    ir->src1 = src;
    ir->arg.idx = idx;
    return ir;
}

IR* vp_ir_call(IRCallInfo* ci, VReg* dst, VReg* freg)
{
    IR* ir = ir_new(IR_CALL);
    ir->call = ci;
    ir->dst = dst;
    ir->src1 = freg;
    return ir;
}

IR* vp_ir_cast(VReg* src, VRSize dstsize, uint8_t vflag)
{
    IR* ir = ir_new(IR_CAST);
    ir->src1 = src;
    ir->dst = vp_ra_spawn(dstsize, vflag);
    return ir;
}

static VReg* ir_binop_fold(IrKind kind, VReg* src1, VReg* src2, VRSize vsize, uint8_t flag)
{
    if((src2->flag & (VRF_FLO | VRF_CONST)) == VRF_CONST && src2->i64 == 0 &&
        (kind == IR_DIV || kind == IR_MOD))
    {
        return NULL;
    }

    if((src1->flag & (VRF_FLO | VRF_CONST)) == VRF_CONST)
    {
        int64_t lval = src1->i64;
        if((src2->flag & (VRF_FLO | VRF_CONST)) == VRF_CONST)
        {
            int64_t rval = src2->i64;
            int64_t val = 0;
            switch(kind)
            {
            case IR_ADD: val = lval + rval; break;
            case IR_SUB: val = lval - rval; break;
            case IR_MUL: val = lval * rval; break;
            case IR_DIV:
                if(flag & IRF_UNSIGNED)
                    val = (uint64_t)lval / (uint64_t)rval;
                else
                    val = lval / rval;
                break;
            case IR_MOD:
                if(flag & IRF_UNSIGNED)
                    val = (uint64_t)lval % (uint64_t)rval;
                else
                    val = lval % rval;
                break;
            case IR_BAND: val = lval & rval; break;
            case IR_BOR: val = lval | rval; break;
            case IR_BXOR: val = lval ^ rval; break;
            case IR_LSHIFT: val = lval << rval; break;
            case IR_RSHIFT:
                if(flag & IRF_UNSIGNED)
                    val = (uint64_t)lval >> rval;
                else
                    val = lval >> rval;
                break;
            default: vp_assertX(0, "?"); break;
            }
            return vp_vreg_ki(val, vsize);
        }
        else
        {
            switch(kind)
            {
            case IR_ADD:
                if(lval == 0)
                    return src2;  /* No effect */
                break;
            case IR_SUB:
                if(lval == 0)
                    return vp_ir_unary(IR_NEG, src2, src2->vsize, flag);
                break;
            case IR_MUL:
                switch(lval)
                {
                case 1: return src2;  /* No effect */
                case 0: return src1;  /* 0 */
                case -1:
                    if(!(flag & IRF_UNSIGNED))
                        return vp_ir_unary(IR_NEG, src2, src2->vsize, flag);  /* -src2 */
                    break;
                default: break;
                }
                break;
            case IR_DIV:
            case IR_MOD:
                if(lval == 0)
                    return src1;
                break;
            case IR_BAND:
                if(lval == 0)
                    return src1;  /* 0 */
                break;
            case IR_BOR:
            case IR_BXOR:
                if(lval == 0)
                    return src2;  /* No effect */
                break;
            case IR_LSHIFT:
            case IR_RSHIFT:
                if(lval == 0)
                    return src1;  /* 0 */
                break;
            default:
                break;
            }
        }
    }
    else
    {
        if((src2->flag & (VRF_FLO | VRF_CONST)) == VRF_CONST)
        {
            int64_t rval = src2->i64;
            switch(kind)
            {
            case IR_ADD:
            case IR_SUB:
                if(rval == 0)
                    return src1;  /* No effect */
                break;
            case IR_DIV:
                if(rval == 0)
                    return src1;  /* Detect zero division */
                /* Fall */
            case IR_MUL:
                switch(rval)
                {
                case 1: return src1;  /* No effect */
                case 0: return src2;  /* 0 */
                case -1:
                    if(!(flag & IRF_UNSIGNED))
                        return vp_ir_unary(IR_NEG, src1, src1->vsize, flag);  /* -src1 */
                    break;
                default: break;
                }
                break;
            case IR_BAND:
                if(rval == 0)
                    return src2;  /* 0 */
                break;
            case IR_BOR:
            case IR_BXOR:
                if(rval == 0)
                    return src1;  /* No effect */
                break;
            case IR_LSHIFT:
            case IR_RSHIFT:
                if(src2->i64 == 0)
                    return src1;  /* no effect */
                break;
            default:
                break;
            }
        }
    }
    return NULL;
}

VReg* vp_ir_binop(IrKind kind, VReg* src1, VReg* src2, VRSize vsize, uint8_t flag)
{
    VReg* dst = ir_binop_fold(kind, src1, src2, vsize, flag);
    if(dst) return dst;
    
    dst = vp_ra_spawn(vsize, src1->flag & VRF_MASK);
    IR* ir = ir_new(kind);
    ir->flag = flag;
    ir->dst = dst;
    ir->src1 = src1;
    ir->src2 = src2;
    return dst;
}

VReg* vp_ir_unary(IrKind kind, VReg* src, VRSize vsize, uint8_t flag)
{
    VReg* dst = vp_ra_spawn(vsize, src->flag & VRF_MASK);
    IR* ir = ir_new(kind);
    ir->flag = flag;
    ir->src1 = src;
    ir->dst = dst;
    return dst;
}

/* Create a new frame info offset */
FrameInfo* vp_frameinfo_new()
{
    FrameInfo* fi = vp_mem_alloc(sizeof(*fi));
    fi->ofs = 0;
    return fi;
}

/* Create a new call info */
IRCallInfo* vp_ircallinfo_new(VReg** args, uint32_t argnum, Str* label)
{
    IRCallInfo* ci = vp_mem_calloc(1, sizeof(*ci));
    ci->argnum = argnum;
    ci->args = args;
    ci->label = label;
    return ci;
}

CondKind vp_cond_swap(CondKind cond)
{
    vp_assertX((cond & ~COND_MASK) == 0, "bad condition");
    if(cond >= COND_LT)
        cond = (COND_GT + COND_LT) - cond;
    return cond;
}

/* Invert a condition */
CondKind vp_cond_invert(CondKind cond)
{
    uint8_t c = cond & COND_MASK;
    vp_assertX(COND_EQ <= c && c <= COND_GT, "not a condition");
    uint8_t ic = c <= COND_NEQ ? (COND_NEQ + COND_EQ) - c
            : (vp_assertX((COND_LT & 3) == 0, "COND_LT must be aligned to 4 (LSBs 00)"), c ^ 2);
    return ic | (cond & ~COND_MASK);
}

/* Create a new basic block */
BB* vp_bb_new()
{
    BB* bb = vp_arena_alloc(&V->irarena, sizeof(*bb));
    bb->label = vp_label_new();
    bb->next = NULL;
    bb->irs = NULL;
    bb->ofs = 0;
    bb->frombbs = NULL;
    bb->inregs = NULL;
    bb->outregs = NULL;
    bb->assignregs = NULL;
    return bb;
}

/* Set current basic block */
void vp_bb_setcurr(BB* bb)
{
    vp_assertX(bb, "missing basic block");
    vp_assertX(V->fncode, "missing function");
    if(V->bb)
    {
        V->bb->next = bb;
    }
    V->bb = bb;
    vec_push(V->fncode->bbs, bb);
}

/* Identify basic blocks reachability */
void vp_bb_detect(vec_t(BB*) bbs)
{
    uint32_t len = vec_len(bbs);
    if(!len) return;

    /* Clear all frombbs */
    for(uint32_t i = 0; i < len; i++)
    {
        BB* bb = bbs[i];
        vec_clear(bb->frombbs);
    }

    Tab checked;
    vp_tab_init(&checked);
    vec_t(BB*) unchecked = NULL;
    vec_push(unchecked, bbs[0]);

    do
    {
        BB* bb = vec_pop(unchecked);
        if(vp_tab_get(&checked, bb->label))
            continue;
        vp_tab_set(&checked, bb->label, bb);

        vec_t(IR*) irs = bb->irs;
        uint32_t lenirs = vec_len(irs);
        if(lenirs > 0)
        {
            IR* ir = irs[lenirs - 1];   /* JMP must be the last IR */
            if(ir->kind == IR_JMP)
            {
                BB* dst = ir->jmp.bb;
                vec_push(dst->frombbs, bb);
                vec_push(unchecked, dst);
                if(ir->jmp.cond == COND_ANY)
                    continue;   /* Next BB is not reachable */
            }
        }
        BB* next = bb->next;
        if(next)
        {
            vec_push(next->frombbs, bb);
            vec_push(unchecked, next);
        }
    }
    while(vec_len(unchecked) > 0);
}

/* Insert vreg in a BB's input/output/assign */
static bool bb_insert_vreg(vec_t(VReg*)* vregs, VReg* vr)
{
    uint32_t lo = UINT32_MAX;
    uint32_t hi = vec_len(*vregs);
    while(hi - lo > 1)
    {
        uint32_t m = lo + (hi - lo) / 2;
        VReg* mid = (*vregs)[m];

        if(mid->virt < vr->virt) lo = m;
        else if(mid->virt > vr->virt) hi = m;
        else return false;
    }
    vp_assertX(hi <= vec_len(*vregs), "hi out of bounds");
    vp_assertX(hi == vec_len(*vregs) || ((*vregs)[hi])->virt != vr->virt, "duplicate vreg");
    vec_insert(*vregs, hi, vr);
    return true;
}

/* Propagate a vreg backwards through basic blocks (liveness analysis) */
static void bb_propagate(VReg* vr, vec_t(BB*)* froms)
{
    BB* bb;
    while(vec_len(*froms) > 0)
    {
        bb = vec_pop(*froms);
        bb_insert_vreg(&bb->outregs, vr);
        if(vec_contains(bb->assignregs, vr) ||
            !bb_insert_vreg(&bb->inregs, vr))
            continue;
        vec_concat(*froms, bb->frombbs);
    }
}

/* Analyze register flow between basic blocks */
void vp_bb_analyze(vec_t(BB*) bbs)
{
    /* Enumerate in and assign registers for each BB */
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        vec_clear(bb->inregs);
        vec_clear(bb->assignregs);
        vec_clear(bb->outregs);

        vec_t(IR*) irs = bb->irs;
        for(uint32_t j = 0; j < vec_len(irs); j++)
        {
            IR* ir = irs[j];
            VReg* vregs[] = { ir->src1, ir->src2 };
            for(uint32_t k = 0; k < 2; k++)
            {
                VReg* vr = vregs[k];
                if(vr == NULL || vrf_const(vr))
                    continue;
                if(!vec_contains(bb->assignregs, vr))
                {
                    bb_insert_vreg(&bb->inregs, vr);
                }
            }
            if(ir->dst)
            {
                bb_insert_vreg(&bb->assignregs, ir->dst);
            }
        }
    }

    /* Propagate inregs to outregs to frombbs recursively */
    vec_t(BB*) dstbbs = NULL;
    for(uint32_t i = vec_len(bbs); i-- > 0;)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->inregs); j++)
        {
            vp_assertX(vec_len(dstbbs) == 0, "bad dstbbs?");
            vec_concat(dstbbs, bb->frombbs);
            VReg* vr = bb->inregs[j];
            bb_propagate(vr, &dstbbs);
        }
    }
}

static uint32_t labelno;

Str* vp_label_new()
{
    labelno++;
    char buf[2 + sizeof(uint32_t) * 3 + 1];
    snprintf(buf, sizeof(buf), ".L%04d", labelno);
    return vp_str_newlen(buf);
}