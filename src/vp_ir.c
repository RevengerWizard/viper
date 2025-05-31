/*
** vp_ir.c
** Intermediate Representation
*/

#include "vp_ir.h"
#include "vp_mem.h"
#include "vp_state.h"
#include "vp_vec.h"
#include "vp_regalloc.h"

static IR* ir_new(IrKind kind)
{
    IR* ir = vp_arena_alloc(&V->irarena, sizeof(*ir));
    ir->kind = kind;
    ir->dst = ir->src1 = ir->src2 = NULL;
    vec_push(V->irs, ir);
    return ir;
}

IR* vp_ir_bofs()
{
    IR* ir = ir_new(IR_BOFS);
    ir->dst = vp_regalloc_spawn(VRegSize8, 0);
    return ir;
}

IR* vp_ir_iofs(Str* label)
{
    IR* ir = ir_new(IR_IOFS);
    ir->label = label;
    ir->dst = vp_regalloc_spawn(VRegSize8, 0);
    return ir;
}

IR* vp_ir_mov(VReg* dst, VReg* src)
{
    IR* ir = ir_new(IR_MOV);
    ir->dst = dst;
    ir->src1 = src;
    return ir;
}

IR* vp_ir_store(VReg* dst, VReg* src)
{
    IR* ir = ir_new(IR_STORE);
    ir->src1 = src;
    ir->src2 = dst;
    return ir;
}

IR* vp_ir_load(VReg* src, VRegSize vsize)
{
    IR* ir = ir_new(IR_LOAD);
    ir->src1 = src;
    ir->dst = vp_regalloc_spawn(vsize, src->flag);
    return ir;
}

IR* vp_ir_ret(VReg* src)
{
    IR* ir = ir_new(IR_RET);
    ir->src1 = src;
    return ir;
}

IR* vp_ir_cond(VReg* src1, VReg* src2, CondKind cond)
{
    IR* ir = ir_new(IR_COND);
    ir->src1 = src1;
    ir->src2 = src2;
    ir->dst = vp_regalloc_spawn(VRegSize1, 0);
    ir->cond = cond;
    return ir;
}

VReg* vp_ir_binop(IrKind kind, VReg* src1, VReg* src2, VRegSize vsize)
{
    VReg* dst = vp_regalloc_spawn(vsize, src1->flag);
    IR* ir = ir_new(kind);
    ir->dst = dst;
    ir->src1 = src1;
    ir->src2 = src2;
    return dst;
}

VReg* vp_ir_unary(IrKind kind, VReg* src, VRegSize vsize)
{
    VReg* dst = vp_regalloc_spawn(vsize, src->flag);
    IR* ir = ir_new(kind);
    ir->src1 = src;
    ir->dst = dst;
    return dst;
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