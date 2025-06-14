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
    ir->dst = vp_ra_spawn(VRegSize8, 0);
    ir->bofs.fi = fi;
    ir->bofs.ofs = 0;
    return ir;
}

IR* vp_ir_iofs(Str* label)
{
    IR* ir = ir_new(IR_IOFS);
    ir->label = label;
    ir->dst = vp_ra_spawn(VRegSize8, 0);
    return ir;
}

IR* vp_ir_sofs(uint32_t ofs)
{
    IR* ir = ir_new(IR_SOFS);
    ir->dst = vp_ra_spawn(VRegSize8, 0);
    ir->sofs.ofs = ofs;
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

IR* vp_ir_load_s(VReg* dst, VReg* src, VRegSize vsize)
{
    IR* ir = ir_new(IR_LOAD_S);
    ir->src1 = src;
    ir->dst = dst;
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
    ir->dst = vp_ra_spawn(VRegSize1, 0);
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

IR* vp_ir_cast(VReg* src, VRegSize dstsize)
{
    IR* ir = ir_new(IR_CAST);
    ir->src1 = src;
    ir->dst = vp_ra_spawn(dstsize, 0);
    return ir;
}

VReg* vp_ir_binop(IrKind kind, VReg* src1, VReg* src2, VRegSize vsize)
{
    VReg* dst = vp_ra_spawn(vsize, 0);
    IR* ir = ir_new(kind);
    ir->dst = dst;
    ir->src1 = src1;
    ir->src2 = src2;
    return dst;
}

VReg* vp_ir_unary(IrKind kind, VReg* src, VRegSize vsize)
{
    VReg* dst = vp_ra_spawn(vsize, 0);
    IR* ir = ir_new(kind);
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