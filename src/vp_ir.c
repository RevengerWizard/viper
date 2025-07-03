/*
** vp_ir.c
** Intermediate Representation
*/

#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_ast.h"
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

IR* vp_ir_load(VReg* src, VRSize vsize, uint8_t flag)
{
    IR* ir = ir_new(IR_LOAD);
    ir->flag = flag;
    ir->src1 = src;
    ir->dst = vp_ra_spawn(vsize, src->flag);
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

/* Zero memory */
IR* vp_ir_memzero(VReg* dst, uint32_t size)
{
    IR* ir = ir_new(IR_MEMZERO);
    ir->dst = dst;
    ir->mem.size = size;
    return ir;
}

IR* vp_ir_memcpy(VReg* dst, VReg* src, uint32_t size)
{
    IR* ir = ir_new(IR_MEMCPY);
    ir->dst = dst;
    ir->src1 = src;
    ir->mem.size = size;
    return ir;
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
    return bb;
}

/* Set current basic block */
void vp_bb_setcurr(BB* bb)
{
    vp_assertX(bb, "missing basic block");
    if(V->bb)
    {
        V->bb->next = bb;
    }
    V->bb = bb;
    vec_push(V->currfn->fn.bbs, bb);
}

static uint32_t labelno;

Str* vp_label_new()
{
    labelno++;
    char buf[2 + sizeof(uint32_t) * 3 + 1];
    snprintf(buf, sizeof(buf), ".L%04d", labelno);
    return vp_str_newlen(buf);
}