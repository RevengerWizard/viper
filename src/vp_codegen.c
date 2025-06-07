/*
** vp_code.c
** Code generation (AST -> IR)
*/

#include "vp_codegen.h"
#include "vp_regalloc.h"
#include "vp_ast.h"
#include "vp_ir.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_var.h"
#include "vp_vec.h"

#include "vp_dump.h"

/* Generate storage location for variable */
static void gen_varinfo(VarInfo* vi)
{
    vp_assertX(vi, "empty variable info");
    
    if(!type_isscalar(vi->type))
    {
        /* For non-scalar types (arrays, structs), allocate frame space */
        vi->fi = vp_frameinfo_new();
        vi->vreg = NULL;
    }
    else
    {
        /* For scalar types, allocate a virtual register */
        vi->vreg = vp_vreg_new(vi->type);
        vi->fi = NULL;
    }
}

/* Generate store operation, based on type */
static void gen_store(VReg* dst, VReg* src, Type* ty)
{
    if(type_isscalar(ty))
    {
        vp_ir_store(dst, src);
    }
    else if(type_isaggr(ty))
    {
        uint32_t tsize = vp_type_sizeof(ty);
        vp_ir_memcpy(dst, src, tsize);
    }
    else
    {
        vp_assertX(0, "unsupported type for assignment store");
    }
}

/* Forward declarations */
static void gen_stmt(Stmt* st);
static VReg* gen_expr(Expr* e);
static VReg* gen_lval(Expr* e);

/* Generate nil constant (0) */
static VReg* gen_nil(Expr* e)
{
    return vp_vreg_ku(0, vp_vsize(e->ty));
}

/* Generate true constant (1) */
static VReg* gen_true(Expr* e)
{
    return vp_vreg_ku(e->b, vp_vsize(e->ty));
}

/* Generate false constant (0) */
static VReg* gen_false(Expr* e)
{
    return vp_vreg_ku(e->b, vp_vsize(e->ty));
}

/* Generate integer constant */
static VReg* gen_int(Expr* e)
{
    return vp_vreg_ki(e->i, vp_vsize(e->ty));
}

/* Generate unsigned integer constant */
static VReg* gen_uint(Expr* e)
{
    return vp_vreg_ku(e->u, vp_vsize(e->ty));
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
    if(type_isscalar(e->ty))
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
        VReg* dst = vp_ir_load(src, vp_vsize(e->ty))->dst;
        return dst;
    }
    else if(e->ty->kind == TY_array || e->ty->kind == TY_struct)
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
    VReg* lhs = gen_expr(e->binop.lhs);
    VReg* rhs = gen_expr(e->binop.rhs);
    return vp_ir_binop(e->kind + (IR_ADD - EX_ADD), lhs, rhs, vp_vsize(e->ty));
}

/* Generate unary operation */
static VReg* gen_unary(Expr* e)
{
    vp_assertX(EX_NEG <= e->kind && e->kind <= EX_BNOT, "not a unary operator");
    VReg* src = gen_expr(e->unary);
    return vp_ir_unary(e->kind + (IR_NEG - EX_NEG), src, vp_vsize(e->ty));
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
            return vp_ir_bofs(vi->fi)->dst;
        }
        case EX_DEREF:
            return gen_expr(e->unary);
        case EX_FIELD:
        {
            /* Generate base address */
            VReg* base = gen_expr(e->field.expr);
            
            /* Get field offset */
            Type* basety = e->field.expr->ty;
            uint32_t offset = vp_type_offset(basety, e->field.name);
            
            /* Return base + offset */
            VReg* ofsreg = vp_vreg_ku(offset, VRegSize8);
            return vp_ir_binop(IR_ADD, base, ofsreg, VRegSize8);
        }
        case EX_IDX:
        {
            VReg* base = gen_expr(e->idx.expr);
            VReg* idx = gen_expr(e->idx.index);

            /* Get element type and size */
            vp_assertX(e->idx.expr->ty && e->idx.expr->ty->kind == TY_array, "not valid type");
            Type* elemty = e->idx.expr->ty->p;
            uint32_t elemsize = vp_type_sizeof(elemty);

            /* Scale the index by element size if needed */
            VReg* scaleidx = idx;
            if(elemsize > 1)
            {
                VReg* sizevr = vp_vreg_ku(elemsize, VRegSize8);
                scaleidx = vp_ir_binop(IR_MUL, idx, sizevr, VRegSize8);
            }
            return vp_ir_binop(IR_ADD, base, scaleidx, VRegSize8);
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
    if(type_isscalar(e->ty))
    {
        return vp_ir_load(freg, vp_vsize(e->ty))->dst;
    }
    return freg;
}

/* Generate array index */
static VReg* gen_idx(Expr* e)
{
    vp_assertX(e->kind == EX_IDX, "not an index expression");
    VReg* addr = gen_lval(e);
    if(type_isscalar(e->ty))
    {
        return vp_ir_load(addr, vp_vsize(e->ty))->dst;
    }
    return addr;
}

/* Generate function call */
static VReg* gen_call(Expr* e)
{
    vp_assertX(e->kind == EX_CALL, "not a call expression");
    
    bool labelcall = false;
    if(e->call.expr->kind == EX_NAME)
    {
        VarInfo* vi = vp_scope_find(e->call.expr->scope, e->call.expr->name, NULL);
        vp_assertX(vi, "name not found");
        labelcall = vi->type->kind == TY_func;
    }
    
    VReg** args = NULL;
    uint32_t argnum = vec_len(e->call.args);
    for(uint32_t i = 0; i < argnum; i++)
    {
        VReg* vr = gen_expr(e->call.args[i]);
        vec_push(args, vr);
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
    vp_ir_call(ci, dst, freg);
    return dst;
}

/* Generate cast expression */
static VReg* gen_cast(Expr* e)
{
    vp_assertX(e->kind == EX_CAST, "not a cast expression");

    Type* dstty = e->ty;
    VReg* src = gen_expr(e->cast.expr);
    switch(dstty->kind)
    {
        case TY_void:
        case TY_struct:
        case TY_union:
            return src;
        default:
            break;
    }

    IR* ir = vp_ir_cast(src, vp_vsize(dstty));
    return ir->dst;
}

/* Generate compound literal */
static VReg* gen_complit(Expr* e)
{
    vp_assertX(e->ty, "missing compound type");
    FrameInfo* fi = vp_frameinfo_new();
    VReg* vreg = vp_ir_bofs(fi)->dst;

    uint32_t struct_size = vp_type_sizeof(e->ty);
    vp_ir_memzero(vreg, struct_size);

    Field* fields = e->comp.fields;
    for(uint32_t i = 0; i < vec_len(fields); i++)
    {
        Field* field = &e->comp.fields[i];
        VReg* freg = gen_expr(field->init);
        VReg* faddr = NULL;
        switch(field->kind)
        {
            case FIELD_NAME:
            {
                uint32_t idx = vp_type_fieldidx(e->ty, field->name);
                uint32_t ofs = e->ty->st.fields[idx].offset;
                VReg* ofsreg = vp_vreg_ku(ofs, VRegSize4);
                faddr = vp_ir_binop(IR_ADD, vreg, ofsreg, VRegSize8);
                break;
            }
            case FIELD_IDX:
            {
                VReg* idx = gen_expr(field->idx);
                
                /* Get element type and size */
                Type* elemty = e->ty->p;
                uint32_t elemsize = vp_type_sizeof(elemty);

                /* Scale the index by element size if needed */
                VReg* scaleidx = idx;
                if(elemsize > 1)
                {
                    VReg* sizevr = vp_vreg_ku(elemsize, VRegSize8);
                    scaleidx = vp_ir_binop(IR_MUL, idx, sizevr, VRegSize8);
                }

                faddr = vp_ir_binop(IR_ADD, vreg, scaleidx, VRegSize8);
                break;
            }
            case FIELD_DEFAULT:
            {
                uint32_t offset = i * vp_type_sizeof(e->ty->p);
                VReg* ofsreg = vp_vreg_ku(offset, VRegSize8);
                faddr = vp_ir_binop(IR_ADD, vreg, ofsreg, VRegSize8);
                break;
            }
            default:
                vp_assertX(0, "?");
                break;
        }

        gen_store(faddr, freg, field->init->ty);
    }
    return vreg;
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

    uint8_t flag = 0;
    if(type_isunsigned(e->ty))
        flag = COND_UNSIGNED;
    if(type_isflo(e->ty))
        flag |= COND_NUM;

    VReg* vlhs = gen_expr(lhs);
    VReg* vrhs = gen_expr(rhs);
    vp_assertX(type_isscalar(e->ty), "complex type");
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
            VReg* src2 = vp_vreg_ku(0, vp_vsize(e->ty));
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
    if(type_isscalar(e->ty))
    {
        vr = vp_ir_load(vr, vp_vsize(e->ty))->dst;
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
    vp_ir_mov(dst, vp_vreg_ku(true, VRegSize1));
    vp_ir_jmp(nbb);
    vp_bb_setcurr(fbb);
    vp_ir_mov(dst, vp_vreg_ku(false, VRegSize1));
    vp_bb_setcurr(nbb);
    return dst;
}

typedef VReg* (*GenExprFn)(Expr*);
static const GenExprFn table[] = {
    [EX_NIL] = gen_nil,
    [EX_TRUE] = gen_true, [EX_FALSE] = gen_false,
    [EX_INT] = gen_int, [EX_UINT] = gen_uint,
    [EX_NUM] = gen_num, [EX_FLO] = gen_flo,
    [EX_NAME] = gen_name,
    [EX_ADD] = gen_binop, [EX_SUB] = gen_binop,
    [EX_MUL] = gen_binop, [EX_DIV] = gen_binop, [EX_MOD] = gen_binop,
    [EX_BAND] = gen_binop, [EX_BOR] = gen_binop, [EX_BXOR] = gen_binop,
    [EX_LSHIFT] = gen_binop, [EX_RSHIFT] = gen_binop,
    [EX_NEG] = gen_unary, [EX_BNOT] = gen_unary, [EX_NOT] = gen_unary,
    [EX_EQ] = gen_cond, [EX_NOTEQ] = gen_cond,
    [EX_LE] = gen_cond, [EX_LT] = gen_cond,
    [EX_GE] = gen_cond, [EX_GT] = gen_cond,
    [EX_REF] = gen_addr, [EX_DEREF] = gen_deref,
    [EX_AND] = gen_logical, [EX_OR] = gen_logical,
    [EX_COMPOUND] = gen_complit,
    [EX_FIELD] = gen_field, [EX_IDX] = gen_idx,
    [EX_CALL] = gen_call,
    [EX_CAST] = gen_cast,
};

/* Generate expression */
static VReg* gen_expr(Expr* e)
{
    vp_assertX(e->ty, "missing type");
    vp_assertX(e->kind < (int)ARRSIZE(table), "out of bounds expression kind");
    vp_assertX(table[e->kind], "empty entry %d", e->kind);
    return (*table[e->kind])(e);
}

/* Generate expression statement */
static void gen_expr_stmt(Expr* e)
{
    gen_expr(e);
}

/* Generate assignment */
static void gen_assign(Expr* lhs, Expr* rhs)
{
    VReg* src = gen_expr(rhs);
    if(lhs->kind == EX_NAME)
    {
        VarInfo* vi = vp_scope_find(lhs->scope, lhs->name, NULL);
        if(!vp_scope_isglob(lhs->scope))
        {
            if(vp_var_isloc(vi))
            {
                vp_assertX(vi->vreg, "empty vreg");
                vp_ir_mov(vi->vreg, src);
                return;
            }
        }
    }

    VReg* dst = gen_lval(lhs);
    gen_store(dst, src, lhs->ty);
}

/* Generate variable declaration */
static void gen_var(Decl* d)
{
    vp_assertX(d->kind == DECL_VAR, "?");
    
    VarInfo* vi = d->var.vi;
    vp_assertX(vi, "null variable info");
    
    gen_varinfo(vi);
    
    /* Generate initialization if provided */
    if(d->var.expr)
    {
        VReg* init_val = gen_expr(d->var.expr);
        
        if(type_isscalar(vi->type))
        {
            /* For scalars, vi->vreg is the destination register */
            vp_assertX(vi->vreg, "missing vreg for scalar variable");
            vp_ir_mov(vi->vreg, init_val);
        }
        else
        {
            /* For aggregates, vi->fi provides the frame location */
            vp_assertX(vi->fi, "missing frame info for aggregate variable");
            VReg* dst_addr = vp_ir_bofs(vi->fi)->dst;
            
            /* Zero out the memory first */
            uint32_t type_size = vp_type_sizeof(vi->type);
            vp_ir_memzero(dst_addr, type_size);
            
            /* Then copy the initialization value */
            vp_ir_memcpy(dst_addr, init_val, type_size);
        }
    }
    else if (!type_isscalar(vi->type))
    {
        /* For uninitialized aggregates, just zero them out */
        vp_assertX(vi->fi, "missing frame info for aggregate variable");
        VReg* dst_addr = vp_ir_bofs(vi->fi)->dst;
        uint32_t type_size = vp_type_sizeof(vi->type);
        vp_ir_memzero(dst_addr, type_size);
    }
}

/* Generate return statement */
static void gen_ret(Stmt* st)
{
    vp_assertX(st->kind == ST_RETURN, "not return statement");
    if(st->expr)
    {
        VReg* vreg = gen_expr(st->expr);
        vp_ir_ret(vreg);
    }
    else
    {
        vp_ir_ret(NULL);
    }
}

/* Generate block of statements */
static void gen_block(Stmt** block)
{
    for(uint32_t i = 0; i < vec_len(block); i++)
    {
        gen_stmt(block[i]);
    }
}

static void gen_stmt(Stmt* st)
{
    switch(st->kind)
    {
        case ST_BLOCK: gen_block(st->block); break;
        case ST_DECL: gen_var(st->decl); break;
        case ST_ASSIGN: gen_assign(st->lhs, st->rhs); break;
        case ST_EXPR: gen_expr_stmt(st->expr); break;
        case ST_RETURN: gen_ret(st); break;
        default: vp_assertX(0, "?");
    }
}

static void gen_fn(Decl* d)
{
    if(!d->fn.body)
        return;

    V->ra = vp_regalloc_new();
    V->currfn = d;
    BB* bb = vp_bb_new();
    vp_bb_setcurr(bb);

    if(d->fn.params)
    {
        for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
        {
            VarInfo* vi = d->fn.scopes[0]->vars[i];
            gen_varinfo(vi);
        }
    }

    gen_block(d->fn.body->block);

    vp_dump_bb(d);
}

/* Generate code IR for all declarations */
void vp_codegen(Decl** decls)
{
    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        Decl* d = decls[i];
        if(!d)
            continue;
        if(d->kind == DECL_FN)
            gen_fn(d);
    }
}