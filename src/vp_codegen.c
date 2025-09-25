/*
** vp_code.c
** Code generation (AST -> IR)
*/

#include "vp_codegen.h"
#include "vp_mem.h"
#include "vp_opt.h"
#include "vp_regalloc.h"
#include "vp_ast.h"
#include "vp_ir.h"
#include "vp_sel.h"
#include "vp_str.h"
#include "vp_tab.h"
#include "vp_target.h"
#include "vp_type.h"
#include "vp_var.h"
#include "vp_vec.h"

#include "vp_dump.h"

/* Determine if type parameter needs to be allocated on the stack */
static bool param_isstack(Type* ty)
{
    if(ty_isaggr(ty))
    {
        uint32_t size = vp_type_sizeof(ty);
        return size > 8;
    }
    return false;
}

/* Generate storage location for variable */
static VReg* gen_varinfo(VarInfo* vi)
{
    VReg* vr;
    vp_assertX(vi, "empty variable info");
    if(!ty_isscalar(vi->type))
    {
        vi->fi = vp_frameinfo_new();
        vi->vreg = vr = NULL;
        Slot sl = {.type = vi->type, .fi = vi->fi};
        vec_push(V->fncode->slots, sl);
        if(!(V->ra->flag & RAF_STACK_FRAME))
        {
            V->ra->flag = RAF_STACK_FRAME;
        }
    }
    else
    {
        vi->vreg = vr = vp_vreg_new(vi->type);
        vi->fi = NULL;
    }
    return vr;
}

static VRSize vreg_elem(uint32_t size, uint32_t align)
{
    uint32_t s = MIN(align, size);
    if(!IS_POW2(s) || s > 8)
    {
        for(s = 8; s > 1; s >>= 1)
        {
            if(s <= size && size % s == 0)
                break;
        }
    }
    vp_assertX(s > 0, "s == 0");
    return vp_msb(s);
}

/* Zero a memory vreg */
static void gen_memzero(Type* ty, VReg* dst)
{
    uint32_t size = vp_type_sizeof(ty);
    uint32_t align = vp_type_alignof(ty);
    vp_assertX(size, "size == 0");
    VRSize velem = vreg_elem(size, align);
    uint32_t count = size >> velem;
    vp_assertX(count, "count == 0");
    VReg* vzero = vp_vreg_ki(0, velem);
    if(count == 1)
    {
        vp_ir_store(dst, vzero, 0);
    }
    else
    {
        VReg* dstp = vp_ra_spawn(VRSize8, 0);
        vp_ir_mov(dstp, dst, IRF_UNSIGNED);

        VRSize vsize = VRSize8;
        VReg* vcount = vp_ra_spawn(vsize, 0);
        vp_ir_mov(vcount, vp_vreg_ki(count, vsize), IRF_UNSIGNED);
        VReg* vadd = vp_vreg_ki(1 << velem, vsize);

        BB* bbloop = vp_bb_new();
        vp_bb_setcurr(bbloop);
        /* *dstp = 0 */
        vp_ir_store(dstp, vzero, 0);
        /* dstp += velem */
        vp_ir_mov(dstp, vp_ir_binop(IR_ADD, dstp, vadd, dstp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount -= 1 */
        vp_ir_mov(vcount, vp_ir_binop(IR_SUB, vcount, vp_vreg_ki(1, vsize), vcount->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount != 0 */
        vp_ir_cjmp(vcount, vp_vreg_ki(0, vcount->vsize), COND_NEQ, bbloop);
        vp_bb_setcurr(vp_bb_new());
    }
}

static void gen_memcpy(Type* ty, VReg* dst, VReg* src)
{
    uint32_t size = vp_type_sizeof(ty);
    uint32_t align = vp_type_alignof(ty);
    vp_assertX(size, "size == 0");
    VRSize velem = vreg_elem(size, align);
    uint32_t count = size >> velem;
    vp_assertX(count, "count == 0");
    if(count == 1)
    {
        VReg* tmp = vp_ir_load(src, velem, vp_vflag(ty), 0)->dst;
        vp_ir_store(dst, tmp, 0);
    }
    else
    {
        VReg* srcp = vp_ra_spawn(VRSize8, 0);
        vp_ir_mov(srcp, src, IRF_UNSIGNED);
        VReg* dstp = vp_ra_spawn(VRSize8, 0);
        vp_ir_mov(dstp, dst, IRF_UNSIGNED);

        VRSize vsize = VRSize8;
        VReg* vcount = vp_ra_spawn(vsize, 0);
        vp_ir_mov(vcount, vp_vreg_ki(count, vsize), IRF_UNSIGNED);
        VReg* vadd = vp_vreg_ki(1 << velem, vsize);

        BB* bbloop = vp_bb_new();
        vp_bb_setcurr(bbloop);
        VReg* tmp = vp_ir_load(srcp, velem, vp_vflag(ty), 0)->dst;
        /* srcp += velem */
        vp_ir_mov(srcp, vp_ir_binop(IR_ADD, srcp, vadd, srcp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* *dstp = *srcp */
        vp_ir_store(dstp, tmp, 0);
        /* dstp += velem */
        vp_ir_mov(dstp, vp_ir_binop(IR_ADD, dstp, vadd, dstp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount -= 1 */
        vp_ir_mov(vcount, vp_ir_binop(IR_SUB, vcount, vp_vreg_ki(1, vsize), vcount->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount != 0 */
        vp_ir_cjmp(vcount, vp_vreg_ki(0, vcount->vsize), COND_NEQ, bbloop);
        vp_bb_setcurr(vp_bb_new());
    }
}

/* Generate store operation, based on type */
static void gen_store(VReg* dst, VReg* src, Type* ty)
{
    if(ty_isscalar(ty))
    {
        vp_ir_store(dst, src, ir_flag(ty));
    }
    else if(ty_isaggr(ty))
    {
        gen_memcpy(ty, dst, src);
    }
    else
    {
        vp_assertX(0, "unsupported type for assignment store");
    }
}

/* Forward declarations */
static void gen_stmt(Stmt* st);
static VReg* gen_expr(Expr* e);
static VReg* gen_complit(Expr* e);
static VReg* gen_lval(Expr* e);

/* Generate nil constant (0) */
static VReg* gen_nil(Expr* e)
{
    return vp_vreg_ki(0, vp_vsize(e->ty));
}

/* Generate true constant (1) */
static VReg* gen_true(Expr* e)
{
    return vp_vreg_ki(e->b, vp_vsize(e->ty));
}

/* Generate false constant (0) */
static VReg* gen_false(Expr* e)
{
    return vp_vreg_ki(e->b, vp_vsize(e->ty));
}

/* Generate integer constant */
static VReg* gen_int(Expr* e)
{
    return vp_vreg_ki(e->i, vp_vsize(e->ty));
}

/* Generate unsigned integer constant */
static VReg* gen_uint(Expr* e)
{
    return vp_vreg_ki(e->u, vp_vsize(e->ty));
}

/* Generate double constant */
static VReg* gen_num(Expr* e)
{
    return vp_vreg_kf(e->n, vp_vsize(e->ty));
}

/* Generate float constant */
static VReg* gen_flo(Expr* e)
{
    return vp_vreg_kf(e->f, vp_vsize(e->ty));
}

/* Generate name reference */
static VReg* gen_name(Expr* e)
{
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        Scope* scope;
        VarInfo* vi = vp_scope_find(e->scope, e->name, &scope);
        vp_assertX(vi, "name not found");
        if(!vp_scope_isglob(scope))
        {
            vp_assertX(vi->vreg, "missing vreg");
            return vi->vreg;
        }
        VReg* src = gen_lval(e);
        VReg* dst = vp_ir_load(src, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
        return dst;
    }
    else if(ty->kind == TY_array || ty->kind == TY_struct)
    {
        return gen_lval(e);
    }
    vp_assertX(0, "?");
    return NULL;
}

/* Generate binary operation */
static VReg* gen_binop(Expr* e)
{
    vp_assertX(EX_ADD <= e->kind && e->kind <= EX_RSHIFT, "not a binary operator");
    Type* ty = e->ty;
    uint8_t flag = ir_flag(ty);
    VReg* lhs = gen_expr(e->binop.lhs);
    VReg* rhs = gen_expr(e->binop.rhs);
    return vp_ir_binop(e->kind + (IR_ADD - EX_ADD), lhs, rhs, vp_vsize(ty), flag);
}

/* Generate unary operation */
static VReg* gen_unary(Expr* e)
{
    vp_assertX(EX_NEG <= e->kind && e->kind <= EX_BNOT, "not a unary operator");
    uint8_t flag = ir_flag(e->ty);
    VReg* src = gen_expr(e->unary);
    return vp_ir_unary(e->kind + (IR_NEG - EX_NEG), src, vp_vsize(e->ty), flag);
}

/* Generate unary ! */
static VReg* gen_not(Expr* e)
{
    vp_assertX(e->kind == EX_NOT, "! operator");
    VReg* src = gen_expr(e->unary);
    CondKind cond = COND_EQ;
    uint8_t flag = cond_flag(e->ty);
    VReg* zero = vp_vreg_ki(0, src->vsize);
    return vp_ir_cond(src, zero, cond | flag)->dst;
}

/* Generate reference/address */
static VReg* gen_ref(Expr* e)
{
    switch(e->kind)
    {
        case EX_NAME:
        {
            Scope* scope;
            Str* name = e->name;
            VarInfo* vi = vp_scope_find(e->scope, name, &scope);
            vp_assertX(vi, "name not found");
            if(vp_scope_isglob(scope))
            {
                return vp_ir_iofs(name)->dst;
            }
            if(!vi->fi)
            {
                if(!vrf_spill(vi->vreg))
                {
                    vi->vreg->flag |= VRF_REF;
                    vreg_spill(vi->vreg);
                }
                vi->fi = &vi->vreg->fi;
            }
            return vp_ir_bofs(vi->fi)->dst;
        }
        case EX_DEREF:
            return gen_expr(e->unary);
        case EX_FIELD:
        {
            Type* basety = e->field.expr->ty;
            if(ty_isptr(basety))
            {
                basety = basety->p;
            }
            VReg* base = gen_expr(e->field.expr);
            uint32_t offset = vp_type_offset(basety, e->field.name);
            if(offset == 0)
                return base;
            
            /* base + offset */
            VReg* ofsreg = vp_vreg_ki(offset, VRSize8);
            return vp_ir_binop(IR_ADD, base, ofsreg, VRSize8, IRF_UNSIGNED);
        }
        case EX_IDX:
        {
            VReg* base = gen_expr(e->idx.expr);
            VReg* idx = gen_expr(e->idx.index);
            Type* elemty = e->idx.expr->ty->p;
            uint32_t elemsize = vp_type_sizeof(elemty);

            /* base + (idx * sizeof(base)) */
            if(elemsize > 1)
            {
                VReg* sizevr = vp_vreg_ki(elemsize, VRSize8);
                idx = vp_ir_binop(IR_MUL, idx, sizevr, VRSize8, IRF_UNSIGNED);
            }
            return vp_ir_binop(IR_ADD, base, idx, VRSize8, IRF_UNSIGNED);
        }
        case EX_COMPLIT:
        {
            return gen_complit(e);
        }
        default:
            vp_assertX(0, "?");
            return NULL;
    }
}

/* Generate field access */
static VReg* gen_field(Expr* e)
{
    vp_assertX(e->kind == EX_FIELD, "not a field expression");
    VReg* freg = gen_lval(e);
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        return vp_ir_load(freg, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
    }
    return freg;
}

/* Generate array index */
static VReg* gen_idx(Expr* e)
{
    vp_assertX(e->kind == EX_IDX, "not an index expression");
    VReg* addr = gen_lval(e);
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        return vp_ir_load(addr, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
    }
    return addr;
}

/* Generate function call */
static VReg* gen_call(Expr* e)
{
    vp_assertX(e->kind == EX_CALL, "not a call expression");
    
    typedef struct
    {
        uint32_t regidx;
        uint32_t offset;
        uint32_t size;
        bool stack; /* Is a stack argument */
        bool flo;   /* Is a float argument */
    } ArgInfo;

    vec_t(VReg*) args = NULL;
    vec_t(ArgInfo) arginfos = NULL;
    uint32_t offset = 0;
    uint32_t regargs = 0;
    uint32_t argnum = vec_len(e->call.args);

    /* Argument placements */
    {
        for(uint32_t i = 0; i < argnum; i++)
        {
            Expr* arg = e->call.args[i];
            Type* argty = arg->ty;
            vp_assertX(argty, "missing arg type");

            ArgInfo p = {0};
            p.size = vp_type_sizeof(argty);
            p.flo = ty_isflo(argty);

            if(i < 4)
            {
                p.stack = false;
                p.regidx = i;
                regargs++;
            }
            else
            {
                uint32_t align = vp_type_alignof(argty);
                p.stack = true;
                offset = ALIGN_UP(offset, align);
                p.offset = offset;
                offset += ALIGN_UP(p.size, 8);
            }

            vec_push(arginfos, p);
        }
    }
    
    /* Generate arguments */
    {
        for(uint32_t i = argnum; i-- > 0;)
        {
            VReg* src = gen_expr(e->call.args[i]);
            vec_push(args, src);

            ArgInfo* p = &arginfos[i];
            if(p->stack)
            {
                Type* argty = e->call.args[i]->ty;
                VReg* dst = vp_ir_sofs(p->offset)->dst;
                gen_memcpy(argty, dst, src);
            }
            else
            {
                vp_ir_pusharg(src, p->regidx);
            }
        }
    }

    /* Determine if this is a direct or indirect call */
    bool labelcall = false;
    if(e->call.expr->kind == EX_NAME)
    {
        VarInfo* vi = vp_scope_find(e->call.expr->scope, e->call.expr->name, NULL);
        vp_assertX(vi, "name not found");
        labelcall = vi->type->kind == TY_func;
    }

    Str* label = NULL;
    VReg* freg = NULL;
    VReg* dst = NULL;
    Type* ret = e->ty;
    if(ret->kind != TY_void)
    {
        dst = vp_vreg_new(ret);
    }
    if(labelcall)
        label = e->call.expr->name;
    else
        freg = gen_expr(e->call.expr);

    IRCallInfo* ci = vp_ircallinfo_new(args, argnum, label);
    ci->regargs = regargs;
    ci->stacksize = offset;
    ci->fn = vp_tab_get(&V->funcs, label);
    vp_assertX(ci->fn, "?");

    IR* ir = vp_ir_call(ci, dst, freg);
    vec_push(V->fncode->calls, ir);

    return dst;
}

/* Generate cast expression */
static VReg* gen_cast(Expr* e)
{
    vp_assertX(e->kind == EX_CAST || e->kind == EX_PTRCAST || e->kind == EX_INTCAST, "not a cast expression");

    Type* dstty = e->ty;
    Type* srcty = e->cast.expr->ty;
    VReg* vr = gen_expr(e->cast.expr);
    switch(dstty->kind)
    {
        case TY_void:
        case TY_struct:
        case TY_union:
            return vr;
        default:
            break;
    }

    uint32_t dstsize = vp_type_sizeof(dstty);

    uint32_t srcsize = 1U << vr->vsize;
    if(dstsize == srcsize &&
        ty_isflo(dstty) == (vrf_flo(vr) != 0) &&
        (ty_isflo(dstty) || ty_isunsigned(dstty) == ty_isunsigned(srcty)))
    {
        /* Ignore cast between equal integers/floats */
        return vr;
    }

    IR* ir = vp_ir_cast(vr, ty_isunsigned(srcty), vp_vsize(dstty), vp_vflag(dstty));
    ir->flag = ir_flag(dstty);
    return ir->dst;
}

static VReg* gen_bitcast(Expr* e)
{
    vp_assertX(e->kind == EX_BITCAST, "not a bitcast expression");

    /* Do nothing */
    return gen_expr(e->cast.expr);
}

typedef struct FlatField
{
    uint32_t offset;     /* Byte offset from base */
    Expr* init; /* Initializer expression */
    Type* type;
} FlatField;

/* Flatten a compound literal */
static void flatten_complit(Expr* e, uint32_t base_offset, vec_t(FlatField*) flat_fields)
{
    vp_assertX(e->kind == EX_COMPLIT, "not a compound literal");
    
    Type* compty = e->ty;
    Field* fields = e->comp.fields;
    
    for(uint32_t i = 0; i < vec_len(fields); i++)
    {
        Field* field = &e->comp.fields[i];
        uint32_t field_offset = 0;
        
        /* Calculate field offset */
        switch(field->kind)
        {
            case FIELD_NAME:
            {
                uint32_t idx = vp_type_fieldidx(compty, field->name);
                field_offset = base_offset + compty->st.fields[idx].offset;
                break;
            }
            case FIELD_IDX:
            {
                vp_assertX(field->idx->kind == EX_INT || field->idx->kind == EX_UINT, "not constexpr");
                uint32_t idx_val = (field->idx->kind == EX_INT) ? field->idx->i : field->idx->u;
                Type* elemty = compty->p;
                uint32_t elemsize = vp_type_sizeof(elemty);
                field_offset = base_offset + (idx_val * elemsize);
                break;
            }
            case FIELD_DEFAULT:
            {
                Type* elemty = compty->p;
                uint32_t elemsize = vp_type_sizeof(elemty);
                field_offset = base_offset + (i * elemsize);
                break;
            }
            default:
                vp_assertX(0, "unknown field kind");
                break;
        }
        
        /* Check if the field initializer is another compound literal */
        if(field->init->kind == EX_COMPLIT)
        {
            flatten_complit(field->init, field_offset, flat_fields);
        }
        else
        {
            FlatField ff = {
                .offset = field_offset,
                .init = field->init,
                .type = field->init->ty
            };
            vec_push(*flat_fields, ff);
        }
    }
}

/* Generate compound literal */
static VReg* gen_complit(Expr* e)
{
    vp_assertX(e->kind == EX_COMPLIT, "compound literal");
    vp_assertX(e->ty, "missing compound type");
    
    FrameInfo* fi = vp_frameinfo_new();
    VReg* base = vp_ir_bofs(fi)->dst;

    Slot sl = {.type = e->ty, .fi = fi};
    vec_push(V->fncode->slots, sl);
    
    gen_memzero(e->ty, base);

    FlatField* flat_fields = NULL;
    flatten_complit(e, 0, &flat_fields);

    /* Generate stores for all flattened fields */
    for(uint32_t i = 0; i < vec_len(flat_fields); i++)
    {
        FlatField* ff = &flat_fields[i];
        
        /* Calculate field address */
        VReg* field_addr;
        if(ff->offset == 0)
        {
            field_addr = base;
        }
        else
        {
            VReg* offset_reg = vp_vreg_ki(ff->offset, VRSize8);
            field_addr = vp_ir_binop(IR_ADD, base, offset_reg, VRSize8, IRF_UNSIGNED);
        }
        
        VReg* value = gen_expr(ff->init);
        gen_store(field_addr, value, ff->type);
    }
    
    vec_free(flat_fields);
    
    return base;
}

typedef struct CmpExpr
{
    CondKind cond;
    VReg* lhs, *rhs;
} CmpExpr;

/* Generate comparison expression */
static CmpExpr gen_cmp(Expr* e, Expr* lhs, Expr* rhs)
{
    vp_assertX(EX_EQ <= e->kind && e->kind <= EX_GE, "cmp expression");
    CondKind cond = e->kind + (COND_EQ - EX_EQ);
    uint8_t flag = cond_flag(e->ty);
    VReg* vlhs = gen_expr(lhs);
    VReg* vrhs = gen_expr(rhs);
    vp_assertX(ty_isscalar(e->ty), "complex type");
    return (CmpExpr){.cond = cond | flag, .lhs = vlhs, .rhs = vrhs};
}

/* Generate condition expression */
static VReg* gen_cond(Expr* e)
{
    CmpExpr cmp = gen_cmp(e, e->binop.lhs, e->binop.rhs);
    return vp_ir_cond(cmp.lhs, cmp.rhs, cmp.cond)->dst;
}

/* Generate conditional jump */
static void gen_cond_jmp(Expr* e, BB* tbb, BB* fbb)
{
    ExprKind ck = e->kind;
    switch(ck)
    {
        case EX_EQ: case EX_NOTEQ:
        case EX_LT: case EX_LE:
        case EX_GT: case EX_GE:
        {
            CmpExpr cmp = gen_cmp(e, e->binop.lhs, e->binop.rhs);
            vp_ir_cjmp(cmp.lhs, cmp.rhs, cmp.cond, tbb);
            BB* bb1 = vp_bb_new();
            vp_bb_setcurr(bb1);
            vp_ir_jmp(fbb);
            break;
        }
        case EX_AND:
        {
            BB* bb1 = vp_bb_new();
            gen_cond_jmp(e->binop.lhs, bb1, fbb);
            vp_bb_setcurr(bb1);
            gen_cond_jmp(e->binop.rhs, tbb, fbb);
            break;
        }
        case EX_OR:
        {
            BB* bb1 = vp_bb_new();
            gen_cond_jmp(e->binop.lhs, tbb, bb1);
            vp_bb_setcurr(bb1);
            gen_cond_jmp(e->binop.rhs, tbb, fbb);
            break;
        }
        default:
        {
            VReg* src1 = gen_expr(e);
            VReg* src2 = vp_vreg_ki(0, vp_vsize(e->ty));
            vp_ir_cjmp(src1, src2, COND_NEQ, tbb);
            BB* bb1 = vp_bb_new();
            vp_bb_setcurr(bb1);
            vp_ir_jmp(fbb);
            break;
        }
    }
}

/* Generate pointer dereference */
static VReg* gen_deref(Expr* e)
{
    VReg* vr = gen_expr(e->unary);
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        vr = vp_ir_load(vr, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
    }
    return vr;
}

/* Generate reference & */
static VReg* gen_addr(Expr* e)
{
    return gen_ref(e->unary);
}

/* Generate lvalue */
static VReg* gen_lval(Expr* e)
{
    return gen_ref(e);
}

/* Generate logical and/or expression */
static VReg* gen_logical(Expr* e)
{
    BB* tbb = vp_bb_new();  /* True branch */
    BB* fbb = vp_bb_new();  /* False branch */
    BB* nbb = vp_bb_new();  /* Continuation block */
    gen_cond_jmp(e, tbb, fbb);
    vp_bb_setcurr(tbb);
    VReg* dst = vp_vreg_new(tybool);
    vp_ir_mov(dst, vp_vreg_ki(true, VRSize1), 0);
    vp_ir_jmp(nbb);
    vp_bb_setcurr(fbb);
    vp_ir_mov(dst, vp_vreg_ki(false, VRSize1), 0);
    vp_bb_setcurr(nbb);
    return dst;
}

typedef VReg* (*GenExprFn)(Expr*);
static const GenExprFn genexprtab[] = {
    [EX_NIL] = gen_nil,
    [EX_TRUE] = gen_true, [EX_FALSE] = gen_false,
    [EX_INT] = gen_int, [EX_UINT] = gen_uint,
    [EX_NUM] = gen_num, [EX_FLO] = gen_flo,
    [EX_NAME] = gen_name,
    [EX_ADD] = gen_binop, [EX_SUB] = gen_binop,
    [EX_MUL] = gen_binop, [EX_DIV] = gen_binop, [EX_MOD] = gen_binop,
    [EX_BAND] = gen_binop, [EX_BOR] = gen_binop, [EX_BXOR] = gen_binop,
    [EX_LSHIFT] = gen_binop, [EX_RSHIFT] = gen_binop,
    [EX_NEG] = gen_unary, [EX_BNOT] = gen_unary, [EX_NOT] = gen_not,
    [EX_EQ] = gen_cond, [EX_NOTEQ] = gen_cond,
    [EX_LE] = gen_cond, [EX_LT] = gen_cond,
    [EX_GE] = gen_cond, [EX_GT] = gen_cond,
    [EX_REF] = gen_addr, [EX_DEREF] = gen_deref,
    [EX_AND] = gen_logical, [EX_OR] = gen_logical,
    [EX_COMPLIT] = gen_complit,
    [EX_FIELD] = gen_field, [EX_IDX] = gen_idx,
    [EX_CALL] = gen_call,
    [EX_CAST] = gen_cast,
    [EX_INTCAST] = gen_cast,
    [EX_FLOATCAST] = gen_cast,
    [EX_PTRCAST] = gen_cast,
    [EX_BITCAST] = gen_bitcast,
};

/* Generate expression */
static VReg* gen_expr(Expr* e)
{
    vp_assertX(e->ty, "missing type");
    vp_assertX(e->kind < (int)ARRSIZE(genexprtab), "out of bounds expression kind");
    vp_assertX(genexprtab[e->kind], "empty entry %d", e->kind);
    return (*genexprtab[e->kind])(e);
}

/* Generate expression statement */
static void gen_expr_stmt(Stmt* st)
{
    gen_expr(st->expr);
}

static void gen_comp_assign(Stmt* st)
{
    ExprKind op = st->kind - ST_ADD_ASSIGN + EX_ADD;
    Expr* lhs = st->lhs;
    Expr* rhs = st->rhs;
    if(lhs->kind == EX_NAME && ty_isscalar(lhs->ty))
    {
        Scope* scope;
        VarInfo* vi = vp_scope_find(lhs->scope, lhs->name, &scope);
        if(vp_var_isloc(vi))
        {
            vp_assertX(vi->vreg, "empty vreg");
            VReg* vlhs = vi->vreg;
            VReg* vrhs = gen_expr(rhs);
            VReg* res = vp_ir_binop(op + (IR_ADD - EX_ADD), vlhs, vrhs, vp_vsize(lhs->ty), ir_flag(lhs->ty));
            vp_ir_mov(vlhs, res, ir_flag(lhs->ty));
            return;
        }
    }

    /* (ptr = &lhs, *ptr = *ptr + rhs) */
    VReg* pvlhs = gen_lval(lhs);
    VReg* lvlhs = vp_ir_load(pvlhs, vp_vsize(lhs->ty), vp_vflag(lhs->ty), ir_flag(lhs->ty))->dst;
    VReg* vrhs = gen_expr(rhs);
    VReg* res = vp_ir_binop(op + (IR_ADD - EX_ADD), lvlhs, vrhs, vp_vsize(lhs->ty), ir_flag(lhs->ty));
    gen_store(pvlhs, res, lhs->ty);
}

/* Generate assignment */
static void gen_assign(Stmt* st)
{
    Expr* lhs = st->lhs;
    Expr* rhs = st->rhs;
    VReg* src = gen_expr(rhs);
    if(lhs->kind == EX_NAME && ty_isscalar(lhs->ty))
    {
        VarInfo* vi = vp_scope_find(lhs->scope, lhs->name, NULL);
        if(vp_var_isloc(vi))
        {
            vp_assertX(vi->vreg, "empty vreg");
            vp_ir_mov(vi->vreg, src, ir_flag(rhs->ty));
            return;
        }
    }

    VReg* dst = gen_lval(lhs);
    gen_store(dst, src, lhs->ty);
}

/* Generate variable declaration */
static void gen_var(Stmt* st)
{
    Decl* d = st->decl;
    if(d->kind == DECL_NOTE) return;
    vp_assertX(d->kind == DECL_VAR, "?");
    
    VarInfo* vi = d->var.vi;
    vp_assertX(vi, "empty variable info");
    gen_varinfo(vi);
    
    /* Generate initialization if provided */
    if(d->var.expr)
    {
        if(ty_isscalar(vi->type))
        {
            vp_assertX(vi->vreg, "empty vreg");
            uint8_t flag = ir_flag(vi->type);
            VReg* src = gen_expr(d->var.expr);
            vp_ir_mov(vi->vreg, src, flag);
        }
        else
        {
            vp_assertX(vi->fi, "missing frame info");
            VReg* dst = vp_ir_bofs(vi->fi)->dst;
            VReg* src = gen_expr(d->var.expr);
            gen_memcpy(vi->type, dst, src);
        }
    }
    else if(!ty_isscalar(vi->type))
    {
        vp_assertX(vi->fi, "missing frame info");
        VReg* dst = vp_ir_bofs(vi->fi)->dst;
        gen_memzero(vi->type, dst);
    }
}

/* Generate return statement */
static void gen_ret(Stmt* st)
{
    vp_assertX(st->kind == ST_RETURN, "not return statement");
    BB* bb = vp_bb_new();
    if(st->expr)
    {
        VReg* vreg = gen_expr(st->expr);
        vp_ir_ret(vreg, ir_flag(st->expr->ty));
    }
    vp_ir_jmp(V->fncode->retbb);
    vp_bb_setcurr(bb);
}

static BB* breakbb;
static BB* continuebb;

static BB* bb_push_break(BB** bb)
{
    *bb = breakbb;
    BB* bb1 = vp_bb_new();
    breakbb = bb1;
    return bb1;
}

static BB* bb_push_continue(BB** bb)
{
    *bb = continuebb;
    BB* bb1 = vp_bb_new();
    continuebb = bb1;
    return bb1;
}

static void bb_pop_break(BB* bb)
{
    breakbb = bb;
}

static void bb_pop_continue(BB* bb)
{
    continuebb = bb;
}

/* Generate break statement */
static void gen_break(Stmt* st)
{
    UNUSED(st);
    vp_assertX(breakbb != NULL, "missing loop");
    BB* bb = vp_bb_new();
    vp_ir_jmp(breakbb);
    vp_bb_setcurr(bb);
}

/* Generate continue statement */
static void gen_continue(Stmt* st)
{
    UNUSED(st);
    vp_assertX(continuebb != NULL, "missing loop");
    BB* bb = vp_bb_new();
    vp_ir_jmp(continuebb);
    vp_bb_setcurr(bb);
}

/* Generate block of statements */
static void gen_block(Stmt* st)
{
    vec_t(Stmt*) block = st->block;
    for(uint32_t i = 0; i < vec_len(block); i++)
    {
        gen_stmt(block[i]);
    }
}

/* Generate if/else if/else statement */
static void gen_if_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_IF, "not an if statement");

    BB* tbb = vp_bb_new();
    BB* fbb = vp_bb_new();

    gen_cond_jmp(st->ifst.cond, tbb, fbb);
    vp_bb_setcurr(tbb);
    gen_stmt(st->ifst.tblock);

    if(st->ifst.fblock == NULL)
    {
        vp_bb_setcurr(fbb);
    }
    else
    {
        BB* nbb = vp_bb_new();
        vp_ir_jmp(nbb);
        vp_bb_setcurr(fbb);
        gen_stmt(st->ifst.fblock);
        vp_bb_setcurr(nbb);
    }
}

/* Generate while statement */
static void gen_while_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_WHILE, "not a while statement");

    BB* breakbb1, *contbb1;
    BB* loopbb = vp_bb_new();
    BB* condbb = bb_push_continue(&contbb1);
    BB* exitbb = bb_push_break(&breakbb1);

    vp_ir_jmp(condbb);

    vp_bb_setcurr(loopbb);
    gen_stmt(st->whst.body);

    /* Set up the condition block */
    vp_bb_setcurr(condbb);
    gen_cond_jmp(st->whst.cond, loopbb, exitbb);

    vp_bb_setcurr(exitbb);
    bb_pop_break(breakbb1);
    bb_pop_continue(contbb1);
}

static void gen_asm(Stmt* st)
{
    vp_assertX(st->kind == ST_ASM, "not asm statement");
    for(uint32_t i = 0; i < vec_len(st->asm_.insts); i++)
    {
        Inst* inst = st->asm_.insts[i];
        vp_ir_asm(inst);
    }
}

typedef void (*GenStmtFn)(Stmt*);
static const GenStmtFn gensttab[] = {
    [ST_DECL] = gen_var,
    [ST_BLOCK] = gen_block,
    [ST_ASSIGN] = gen_assign,
    [ST_ADD_ASSIGN] = gen_comp_assign, [ST_SUB_ASSIGN] = gen_comp_assign, 
    [ST_MUL_ASSIGN] = gen_comp_assign, [ST_DIV_ASSIGN] = gen_comp_assign, 
    [ST_MOD_ASSIGN] = gen_comp_assign,
    [ST_BAND_ASSIGN] = gen_comp_assign, [ST_BOR_ASSIGN] = gen_comp_assign,  
    [ST_BXOR_ASSIGN] = gen_comp_assign,
    [ST_LSHIFT_ASSIGN] = gen_comp_assign, [ST_RSHIFT_ASSIGN] = gen_comp_assign,
    [ST_EXPR] = gen_expr_stmt,
    [ST_IF] = gen_if_stmt,
    [ST_WHILE] = gen_while_stmt,
    [ST_RETURN] = gen_ret,
    [ST_BREAK] = gen_break,
    [ST_CONTINUE] = gen_continue,
    [ST_ASM] = gen_asm,
};

/* Generate a statement */
static void gen_stmt(Stmt* st)
{
    vp_assertX(st->kind < (int)ARRSIZE(gensttab), "out of bounds statement kind");
    vp_assertX(gensttab[st->kind], "empty entry %d", st->kind);
    (*gensttab[st->kind])(st);
}

static void gen_stack(Code* cd)
{
    uint32_t framesize = 0;

    /* Allocate stack frame*/
    for(uint32_t i = 0; i < vec_len(cd->slots); i++)
    {
        Slot* slot = &cd->slots[i];
        FrameInfo* fi = slot->fi;
        Type* type = slot->type;

        uint32_t size = vp_type_sizeof(type);
        if(size < 1) size = 1;
        uint32_t align = vp_type_alignof(type);

        framesize = ALIGN_UP(framesize + size, align);
        fi->ofs = -(int32_t)framesize;
    }

    /* Allocate spilled variables onto stack frame */
    RegAlloc* ra = cd->ra;
    for(uint32_t i = 0; i < vec_len(ra->vregs); i++)
    {
        LiveInterval* li = ra->sorted[i];
        if(li->state != LI_SPILL)
            continue;
        VReg* vr = ra->vregs[li->virt];
        vp_assertX(vrf_spill(vr), "not spilled vreg?");
        if(vr->flag & VRF_STACK_PARAM)
            continue;

        uint32_t size, align;
        size = align = 1 << vr->vsize;

        framesize = ALIGN_UP(framesize + size, align);
        vr->fi.ofs = -(int32_t)framesize;
    }

    cd->framesize = framesize;
}

/* Generate function parameters */
static void gen_params(Decl* d)
{
    Type* ret = d->fn.rett;
    bool stack = param_isstack(ret);
    uint32_t start = stack ? 1 : 0;

    if(stack)
    {
        VReg* vr = vp_vreg_new(vp_type_ptr(ret));
        vr->flag |= VRF_PARAM;
        vr->param = REG_NO;
    }

    for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
    {
        VarInfo* vi = d->fn.scopes[0]->vars[i];
        VReg* vr = gen_varinfo(vi);
        if(vr)
        {
            vr->flag |= VRF_PARAM;
            uint32_t slot = i + start;
            if(slot < 4)
            {
                vr->param = slot;
            }
            else
            {
                vr->flag |= VRF_STACK_PARAM;
            }
        }
    }
}

/* Generate function body, if present */
static Code* gen_fn(Decl* d)
{
    vp_assertX(d->kind == DECL_FN, "not function declaration");

    RegAlloc* ra = V->ra = vp_ra_new(&winx64_ra);
    Code* code = vp_mem_calloc(1, sizeof(*code));
    code->name = d->name;
    code->numparams = vec_len(d->fn.params);
    code->scopes = d->fn.scopes;
    code->ra = ra;
    vp_tab_set(&V->funcs, code->name, code);
    V->fncode = code;

    vp_bb_setcurr(vp_bb_new());

    if(d->fn.params)
    {
        gen_params(d);
    }

    V->fncode->retbb = vp_bb_new();

    gen_block(d->fn.body);

    vp_bb_setcurr(V->fncode->retbb);
    V->bb = NULL;

    vp_bb_detect(code->bbs);
    vp_opt(code);

    vp_sel_tweak(code);
    vp_bb_analyze(code->bbs);
    vp_ra_alloc(ra, code->bbs);
    gen_stack(code);

    vp_dump_bb(code);

    V->fncode = NULL;
    V->ra = NULL;

    return code;
}

/* Generate code IR for all declarations */
vec_t(Code*) vp_codegen(vec_t(Decl*) decls)
{
    vec_t(Code*) codes = NULL;
    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        Code* cd;
        Decl* d = decls[i];
        if(!d) continue;
        if(d->kind == DECL_FN && d->fn.body)
        {
            cd = gen_fn(d);
            vec_push(codes, cd);
        }
    }
    return codes;
}