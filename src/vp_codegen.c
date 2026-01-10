/*
** vp_code.c
** Code generation (AST -> IR)
*/

#include "vp_codegen.h"
#include "vp_low.h"
#include "vp_mem.h"
#include "vp_opt.h"
#include "vp_regalloc.h"
#include "vp_ast.h"
#include "vp_ir.h"
#include "vp_str.h"
#include "vp_tab.h"
#include "vp_target.h"
#include "vp_type.h"
#include "vp_var.h"
#include "vp_vec.h"

#include "vp_dump.h"

/* Generate storage location for variable */
static VReg* gen_varinfo(VarInfo* vi)
{
    VReg* vr;
    vp_assertX(vi, "empty variable info");
    if(!ty_isscalar(vi->type))
    {
        vi->fi = vp_frameinfo_new();
        vi->vreg = vr = NULL;
    }
    else
    {
        vi->vreg = vr = vp_vreg_new(vi->type);
        vi->fi = NULL;
    }
    return vr;
}

/* Zero a memory vreg */
static void gen_memzero(Type* ty, VReg* dst)
{
    uint32_t size = vp_type_sizeof(ty);
    uint32_t align = vp_type_alignof(ty);
    vp_assertX(size, "size == 0");
    VRSize velem = vp_vreg_elem(size, align);
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
    VRSize velem = vp_vreg_elem(size, align);
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

/* -- Gen expressions ----------------------------------------------- */

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

/* Generate string literal */
static VReg* gen_str(Expr* e)
{
    vp_assertX(e->kind == EX_STR, "string literal");
    vp_assertX(e->ty, "missing string type");

    Str* str = e->str;

    if(ty_isarr(e->ty))
    {
        vp_assertX(e->ty->p == tyuint8, "bad str array type");

        FrameInfo* fi = vp_frameinfo_new();
        VReg* base = vp_ir_bofs(fi)->dst;

        Slot sl = {.type = e->ty, .fi = fi};
        vec_push(V->fncode->slots, sl);

        gen_memzero(e->ty, base);

        /* Generate stores for each char in the string */
        for(uint32_t i = 0; i < str->len; i++)
        {
            /* Calculate index address */
            VReg* idx;
            if(i == 0)
            {
                idx = base;
            }
            else
            {
                VReg* offset_reg = vp_vreg_ki(i, VRSize8);
                idx = vp_ir_binop(IR_ADD, base, offset_reg, VRSize8, IRF_UNSIGNED);
            }

            VReg* value = vp_vreg_ki(str_data(str)[i], VRSize1);
            gen_store(idx, value, tyuint8);
        }

        return base;
    }
    else if(ty_isptr(e->ty))
    {
        vp_assertX(e->ty->p && e->ty->p->kind == TY_uint8, "bad str ptr type");
        vp_assertX(vec_len(V->strs) == vec_len(V->strofs), "non-matching lens");

        uint32_t ofs = 0;
        if(vec_len(V->strofs))
        {
            Str* last = V->strs[vec_len(V->strs) - 1];
            ofs = V->strofs[vec_len(V->strofs) - 1] + (last->len + 1);
        }
        vec_push(V->strs, str);
        vec_push(V->strofs, ofs);

        return vp_ir_iofs(str, false, true)->dst;
    }
    vp_assertX(0, "bad str type");
    return NULL;
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
        if(ty_isfunc(ty))
        {
            return src;
        }
        VReg* dst = vp_ir_load(src, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
        return dst;
    }
    else if(ty_isarr(ty) || ty_isaggr(ty))
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
                return vp_ir_iofs(name, vi->storage & VS_FN, false)->dst;
            }
            if(!vi->fi)
            {
                if(!vrf_spill(vi->vreg))
                {
                    vi->vreg->flag |= VRF_REF;
                    vreg_spill(vi->vreg);
                    if(!raf_stackframe(V->ra))
                    {
                        V->ra->flag |= RAF_STACK_FRAME;
                    }
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
    vp_assertX(e->ty, "call type");

    Type* ret = e->ty;
    FrameInfo* fi = NULL;
    if(param_isstack(ret))
    {
        fi = vp_frameinfo_new();
        Slot sl = {.type = ret, .fi = fi};
        vec_push(V->fncode->slots, sl);
    }

    typedef struct
    {
        uint32_t regidx;
        uint32_t offset;
        uint32_t size;
        bool stack; /* Is a stack argument */
        bool flo;   /* Is a float argument */
    } ArgInfo;

    vec_t(ArgInfo) arginfos = vec_init(ArgInfo);
    uint32_t argstart = (fi != NULL) ? 1 : 0;
    uint32_t offset = 0;
    uint32_t regargs = 0;
    uint32_t argnum = vec_len(e->call.args);

    /* Argument placements */
    {
        for(uint32_t i = argstart; i < argnum; i++)
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
                offset += ALIGN_UP(p.size, TARGET_PTR_SIZE);
            }

            vec_push(arginfos, p);
        }
    }

    uint32_t argtotal = argnum + argstart;
    VReg** args = argtotal == 0 ? NULL : vp_mem_calloc(argtotal, sizeof(*args));

    /* Generate arguments */
    {
        for(uint32_t i = argnum; i-- > 0;)
        {
            VReg* src = gen_expr(e->call.args[i]);

            ArgInfo* p = &arginfos[i];
            if(p->stack)
            {
                Type* argty = e->call.args[i]->ty;
                VReg* dst = vp_ir_sofs(p->offset + 32)->dst;    /* +32 for shadow space? */
                if(param_isstack(argty))
                {
                    gen_memcpy(argty, dst, src);
                }
                else
                {
                    vp_ir_store(dst, src, ir_flag(argty));
                }
            }
            else
            {
                vp_ir_pusharg(src, p->regidx);
            }

            args[i + argstart] = src;
        }
    }
    /* Handle stack return */
    if(fi != NULL)
    {
        VReg* dst = vp_ir_bofs(fi)->dst;
        vp_ir_pusharg(dst, 0);
        args[0] = dst;
        regargs++;
    }

    /* Determine if this is a direct or indirect call */
    bool labelcall = false;
    if(e->call.expr->kind == EX_NAME)
    {
        VarInfo* vi = vp_scope_find(e->call.expr->scope, e->call.expr->name, NULL);
        vp_assertX(vi, "'%s' not found", str_data(e->call.expr->name));
        labelcall = ty_isfunc(vi->type) && (vi->storage & VS_FN);
    }

    Str* label = NULL;
    VReg* freg = NULL;
    VReg* dst = NULL;
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
    ci->argtotal = argtotal;
    if(label)
    {
        ci->fn = vp_tab_get(&V->funcs, label);
        if(!ci->fn)
        {
            vp_tab_set(&V->ifuncs, label, NULL);
            ci->export = true;
        }
    }

    IR* ir = vp_ir_call(ci, dst, freg);
    vec_push(V->fncode->calls, ir);

    return dst;
}

/* Generate cast expression */
static VReg* gen_cast(Expr* e)
{
    vp_assertX(e->kind == EX_CAST || e->kind == EX_INTCAST || e->kind == EX_FLOATCAST || e->kind == EX_PTRCAST, "not a cast expression");

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
    if(vrf_const(vr))
    {
        vp_assertX(!vrf_flo(vr), "const flo");
        int64_t i = vr->i64;
        if(dstsize < (1U << vr->vsize) && dstsize < sizeof(int64_t))
        {
            /* Assume two's complement */
            size_t bit = dstsize * 8;
            uint64_t mask = (-1ULL) << bit;
            if(!ty_isunsigned(dstty) && (i & ((int64_t)1 << (bit - 1))))    /* signed and negative */
                i |= mask;
            else
                i &= ~mask;
        }

        VRSize vsize = vp_vsize(dstty);
        return vp_vreg_ki(i, vsize);
    }

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

    vec_t(FlatField) flat_fields = vec_init(FlatField);
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

    if(flat_fields)
    {
        vec_free(flat_fields);
    }

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
    [EX_CHAR] = gen_int,
    [EX_INT] = gen_int, [EX_UINT] = gen_uint,
    [EX_NUM] = gen_num, [EX_FLO] = gen_flo,
    [EX_STR] = gen_str,
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

/* -- Gen statements ------------------------------------------------ */

/* Generate expression statement */
static void gen_expr_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_EXPR, "not an expression statement");
    gen_expr(st->expr);
}

/* Generate compound assignment */
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
    vp_assertX(d->kind == DECL_VAR || d->kind == DECL_CONST, "var/const");

    VarInfo* vi = d->var.vi;
    vp_assertX(vi, "empty variable info");
    if(!gen_varinfo(vi))
    {
        Slot sl = {.type = vi->type, .fi = vi->fi};
        vec_push(V->fncode->slots, sl);
        if(!raf_stackframe(V->ra))
        {
            V->ra->flag |= RAF_STACK_FRAME;
        }
    }

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
    vp_assertX(st->expr->ty, "missing return type");
    Type* ty = st->expr->ty;
    BB* bb = vp_bb_new();
    if(st->expr)
    {
        VReg* vreg = gen_expr(st->expr);

        if(ty_isscalar(ty))
        {
            vp_ir_ret(vreg, ir_flag(ty));
        }
        else if(ty != tyvoid)
        {
            VReg* retvr = V->fncode->retvr;
            if(retvr)
            {
                gen_memcpy(ty, retvr, vreg);
                vp_ir_ret(vreg, IRF_UNSIGNED);  /* Pointer is unsigned */
            }
            else
            {
                vp_assertX(0, "?");
            }
        }
    }
    vp_ir_jmp(V->fncode->retbb);
    vp_bb_setcurr(bb);
}

static void gen_stmt(Stmt* st);

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

/* Generate for statement */
static void gen_for_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_FOR, "not a for statement");

    BB* breakbb1, *contbb1;
    BB* loopbb = vp_bb_new();
    BB* contbb = bb_push_continue(&contbb1);
    BB* condbb = vp_bb_new();
    BB* exitbb = bb_push_break(&breakbb1);

    gen_stmt(st->forst.init);

    vp_ir_jmp(condbb);

    vp_bb_setcurr(loopbb);
    gen_stmt(st->forst.body);

    vp_bb_setcurr(contbb);
    gen_stmt(st->forst.next);

    /* Set up the condition block */
    vp_bb_setcurr(condbb);
    gen_cond_jmp(st->forst.cond, loopbb, exitbb);

    vp_bb_setcurr(exitbb);
    bb_pop_continue(contbb1);
    bb_pop_break(breakbb1);
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
    bb_pop_continue(contbb1);
    bb_pop_break(breakbb1);
}

/* Generate asm block statement */
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
    [ST_FOR] = gen_for_stmt,
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

/* Detect living registers for each instruction */
static void ra_living(RegAlloc* ra, vec_t(BB*) bbs)
{
    LiveInterval** ilivings = vp_mem_calloc(ra->set->iphysmax, sizeof(*ilivings));
    LiveInterval** flivings = vp_mem_calloc(ra->set->fphysmax, sizeof(*flivings));

    RegSet ipregs = 0;
    RegSet fpregs = 0;

#define VREGFOR(ra, li) ((VReg*)ra->vregs[li->virt])

    /* Activate function parameters first */
    for(uint32_t i = 0; i < vec_len(ra->vregs); i++)
    {
        LiveInterval* li = ra->sorted[i];
        vp_assertX(li, "empty live interval");
        if(li->start != REG_NO)
            break;
        if(li->state != LI_NORMAL || VREGFOR(ra, li) == NULL)
            continue;
        if(vrf_flo(VREGFOR(ra, li)))
        {
            fpregs |= 1ULL << li->phys;
            flivings[li->phys] = li;
        }
        else
        {
            ipregs |= 1ULL << li->phys;
            ilivings[li->phys] = li;
        }
    }

    uint32_t nip = 0, head = 0;
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->irs); j++, nip++)
        {
            /* Eliminate deactivated registers (intervals ending at nip) */
            /* Integer livings */
            for(uint32_t k = 0; k < ra->set->iphysmax; k++)
            {
                LiveInterval* li = ilivings[k];
                if(li && nip == li->end)
                {
                    ipregs &= ~(1ULL << k);
                    ilivings[k] = NULL;
                }
            }

            /* Float livings */
            for(uint32_t k = 0; k < ra->set->fphysmax; k++)
            {
                LiveInterval* li = flivings[k];
                if(li && nip == li->end)
                {
                    fpregs &= ~(1ULL << k);
                    flivings[k] = NULL;
                }
            }

            /* Store living vregs into IR_CALL */
            IR* ir = bb->irs[j];
            if(ir->kind == IR_CALL)
            {
                ir->call->ipregs = ipregs;
                ir->call->fpregs = fpregs;
            }

            /* Add activated registers */
            for(; head < vec_len(ra->vregs); head++)
            {
                LiveInterval* li = ra->sorted[head];
                if(li->start != REG_NO && li->start > nip)
                    break;
                if(li->state != LI_NORMAL)
                    continue;
                if(li->start != REG_NO && nip == li->start)
                {
                    vp_assertX(VREGFOR(ra, li) != NULL, "?");
                    if(vrf_flo(VREGFOR(ra, li)))
                    {
                        fpregs |= 1ULL << li->phys;
                        flivings[li->phys] = li;
                    }
                    else
                    {
                        ipregs |= 1ULL << li->phys;
                        ilivings[li->phys] = li;
                    }
                }
            }
        }
    }

#undef VREGFOR
}

/* Set stack offsets for spills/slots */
static void gen_stack(Code* code)
{
    vp_assertX(code->framesize == 0, "?");
    uint32_t framesize = 0;

    /* Allocate stack frame */
    for(uint32_t i = 0; i < vec_len(code->slots); i++)
    {
        Slot* slot = &code->slots[i];
        FrameInfo* fi = slot->fi;
        Type* type = slot->type;

        uint32_t size = vp_type_sizeof(type);
        uint32_t align = vp_type_alignof(type);
        if(size < 1) size = 1;
        framesize = ALIGN_UP(framesize + size, align);
        fi->ofs = -(int32_t)framesize;
    }

    /* Allocate spilled variables onto stack frame */
    RegAlloc* ra = code->ra;
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

    code->framesize = framesize;
}

/* Generate function parameters */
static void gen_params(Decl* d, Code* code)
{
    Type* ret = d->fn.rett;
    bool stack = param_isstack(ret);
    uint32_t paramofs = TARGET_PTR_SIZE * 2;
    uint32_t start = stack ? 1 : 0;

    if(stack)
    {
        VReg* vr = vp_vreg_new(vp_type_ptr(ret));
        vr->flag |= VRF_STACK_PARAM;
        vr->param = REG_NO;
        code->retvr = vr;
        /* Stack parameter offset */
        paramofs = ALIGN_UP(paramofs, TARGET_PTR_SIZE);
        paramofs += TARGET_PTR_SIZE;
    }

    /* Count register or stack params */
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
                vreg_spill(vr);
                if(!raf_stackframe(V->ra))
                {
                    V->ra->flag = RAF_STACK_FRAME;
                }
                /* Stack parameter offset */
                vr->fi.ofs = paramofs = ALIGN_UP(paramofs, TARGET_PTR_SIZE);
                paramofs += TARGET_PTR_SIZE;
            }
        }
        else
        {
            /* Stack parameter offset */
            uint32_t size = vp_type_sizeof(vi->type);
            uint32_t align = vp_type_alignof(vi->type);
            vi->fi->ofs = paramofs = ALIGN_UP(paramofs, align);
            paramofs += ALIGN_UP(size, TARGET_PTR_SIZE);
        }
    }
}

/* Generate function body, if present */
static Code* gen_fn(Decl* d)
{
    vp_assertX(d->kind == DECL_FN, "not function declaration");

    Code* code = vp_mem_calloc(1, sizeof(*code));
    code->name = d->name;
    code->bbs = NULL;
    code->calls = vec_init(IR*);
    code->slots = vec_init(Slot);
    code->numparams = vec_len(d->fn.params);
    code->scopes = d->fn.scopes;
    vp_tab_set(&V->funcs, code->name, code);

    if(!d->fn.body)
    {
        return code;
    }

    RegAlloc* ra = V->ra = vp_ra_new(V->target->raset);
    code->ra = ra;
    code->bbs = vec_init(BB*);
    V->fncode = code;

    vp_bb_setcurr(vp_bb_new());

    if(d->fn.params)
    {
        gen_params(d, code);
    }

    V->fncode->retbb = vp_bb_new();

    gen_block(d->fn.body);

    vp_bb_setcurr(V->fncode->retbb);
    V->bb = NULL;

    vp_bb_detect(code->bbs);
    vp_opt(code);

    vp_ir_x64_tweak(code);
    vp_bb_analyze(code->bbs);
    vp_ra_alloc(ra, code->bbs);
    ra_living(ra, code->bbs);
    gen_stack(code);

    vp_dump_bbs(code);

    V->fncode = NULL;
    V->ra = NULL;

    return code;
}

/* Generate code IR for all declarations */
vec_t(Code*) vp_codegen(vec_t(Decl*) decls)
{
    vec_t(Code*) codes = vec_init(Code*);
    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        Decl* d = decls[i];
        if(d && d->kind == DECL_FN)
        {
            Code* code = gen_fn(d);
            vec_push(codes, code);
        }
    }
    return codes;
}