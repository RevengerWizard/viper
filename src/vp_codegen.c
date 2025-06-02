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

/* Forward declarations */
static VReg* gen_expr(Expr* e);
static VReg* gen_lval(Expr* e);

static VReg* gen_nil(Expr* e)
{
    return vp_vreg_ku(0, vp_vsize(e->ty));
}

static VReg* gen_true(Expr* e)
{
    return vp_vreg_ki(e->b, vp_vsize(e->ty));
}

static VReg* gen_false(Expr* e)
{
    return vp_vreg_ki(e->b, vp_vsize(e->ty));
}

static VReg* gen_int(Expr* e)
{
    return vp_vreg_ki(e->i, vp_vsize(e->ty));
}

static VReg* gen_uint(Expr* e)
{
    return vp_vreg_ku(e->u, vp_vsize(e->ty));
}

static VReg* gen_num(Expr* e)
{
    return vp_vreg_kf(e->n, vp_vsize(e->ty));
}

static VReg* gen_flo(Expr* e)
{
    return vp_vreg_kf(e->f, vp_vsize(e->ty));
}

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
    vp_assertX(0, "?");
    return NULL;
}

static VReg* gen_binop(Expr* e)
{
    vp_assertX(EX_ADD <= e->kind && e->kind <= EX_RSHIFT, "not a binary operator");
    VReg* lhs = gen_expr(e->binop.lhs);
    VReg* rhs = gen_expr(e->binop.rhs);
    return vp_ir_binop(e->kind + (IR_ADD - EX_ADD), lhs, rhs, vp_vsize(e->ty));
}

static VReg* gen_unary(Expr* e)
{
    vp_assertX(EX_NEG <= e->kind && e->kind <= EX_BNOT, "not a unary operator");
    VReg* src = gen_expr(e->unary);
    return vp_ir_unary(e->kind + (IR_NEG - EX_NEG), src, vp_vsize(e->ty));
}

static VReg* gen_field(Expr* e)
{
    VReg* base = gen_expr(e->field.expr);
    return NULL;
}

typedef struct CmpExpr
{
    CondKind cond;
    VReg* lhs, *rhs;
} CmpExpr;

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

static VReg* gen_cond(Expr* e)
{
    CmpExpr cmp = gen_cmp(e, e->binop.lhs, e->binop.rhs);
    return vp_ir_cond(cmp.lhs, cmp.rhs, cmp.cond)->dst;
}

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
            return vp_ir_bofs()->dst;
        }
        case EX_DEREF:
            return gen_expr(e->unary);
        default:
            vp_assertX(0, "?");
            return NULL;
    }
}

static VReg* gen_deref(Expr* e)
{
    VReg* vr = gen_expr(e->unary);
    if(type_isscalar(e->ty))
    {
        vr = vp_ir_load(vr, vp_vsize(e->ty))->dst;
    }
    return vr;
}

static VReg* gen_addr(Expr* e)
{
    return gen_ref(e->unary);
}

static VReg* gen_lval(Expr* e)
{
    return gen_ref(e);
}

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
    [EX_FIELD] = gen_field,
};

static VReg* gen_expr(Expr* e)
{
    vp_assertX(e->ty, "missing type");
    vp_assertX(e->kind < (int)ARRSIZE(table), "out of bounds expression kind");
    vp_assertX(table[e->kind], "empty entry %d", e->kind);
    return (*table[e->kind])(e);
}

static void gen_expr_stmt(Expr* e)
{
    gen_expr(e);
}

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
    if(type_isscalar(lhs->ty))
    {
        vp_ir_store(dst, src);
    }
}

static void gen_var(Decl* d)
{
    vp_assertX(d->kind == DECL_VAR, "?");
    VarInfo* vi = d->var.vi;
    VReg* src = gen_expr(d->var.expr);
    vp_ir_mov(vi->vreg, src);
}

static void gen_stmt(Stmt* st)
{
    switch(st->kind)
    {
        case ST_DECL: gen_var(st->decl); break;
        case ST_ASSIGN: gen_assign(st->lhs, st->rhs); break;
        case ST_EXPR: gen_expr_stmt(st->expr); break;
        default: vp_assertX(0, "?");
    }
}

static void gen_fn(Decl* d)
{
    V->ra = vp_regalloc_new();
    V->currfn = d;
    BB* bb = vp_bb_new();
    vp_bb_setcurr(bb);

    for(uint32_t i = 0; i < vec_len(d->fn.scopes); i++)
    {
        Scope* scope = d->fn.scopes[i];
        if(scope->vars == NULL)
            continue;
        for(uint32_t j = 0; j < vec_len(scope->vars); j++)
        {
            VarInfo* vi = scope->vars[j];
            VReg* vreg = vp_regalloc_spawn(vp_vsize(vi->type), 0);
            vi->vreg = vreg;
        }
    }

    for(uint32_t i = 0; i < vec_len(d->fn.body->block); i++)
    {
        gen_stmt(d->fn.body->block[i]);
    }

    vp_dump_bb(d);
}

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