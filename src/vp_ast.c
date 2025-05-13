/*
** vp_ast.c
** Abstract Syntax Tree
*/

#include <string.h>

#include "vp_ast.h"
#include "vp_lex.h"
#include "vp_mem.h"
#include "vp_str.h"
#include "vp_type.h"

const char* const vp_ast_binop[] = {
    "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", "<=", ">", ">=", "and", "or"
};

const char* const vp_ast_unary[] = {
    "-", "not ", "~"
};

static void* ast_alloc(uint32_t size)
{
    vp_assertX(size != 0, "0 size");
    void* p = vp_arena_alloc(&V->astarena, size);
    memset(p, 0, size);
    return p;
}

/* -- AST expressions ----------------------------------------------- */

static Expr* expr_new(ExprKind kind, SrcLoc loc)
{
    Expr* e = ast_alloc(sizeof(*e));
    e->kind = kind;
    e->loc = loc;
    return e;
}

Expr* vp_expr_binop(SrcLoc loc, ExprKind kind, Expr* lhs, Expr* rhs)
{
    Expr* expr = expr_new(kind, loc);
    expr->binop.lhs = lhs;
    expr->binop.rhs = rhs;
    return expr;
}

Expr* vp_expr_unary(SrcLoc loc, ExprKind kind, Expr* unary)
{
    Expr* expr = expr_new(kind, loc);
    expr->unary = unary;
    return expr;
}

Expr* vp_expr_false(SrcLoc loc)
{
    Expr* expr = expr_new(EX_FALSE, loc);
    expr->b = false;
    return expr;
}

Expr* vp_expr_true(SrcLoc loc)
{
    Expr* expr = expr_new(EX_TRUE, loc);
    expr->b = true;
    return expr;
}

Expr* vp_expr_nil(SrcLoc loc)
{
    return expr_new(EX_NIL, loc);
}

Expr* vp_expr_ilit(SrcLoc loc, int64_t i)
{
    Expr* expr = expr_new(EX_INT, loc);
    expr->i = i;
    return expr;
}

Expr* vp_expr_ulit(SrcLoc loc, uint64_t u)
{
    Expr* expr = expr_new(EX_UINT, loc);
    expr->u = u;
    return expr;
}

Expr* vp_expr_nlit(SrcLoc loc, double n)
{
    Expr* expr = expr_new(EX_NUM, loc);
    expr->n = n;
    return expr;
}

Expr* vp_expr_flit(SrcLoc loc, float f)
{
    Expr* expr = expr_new(EX_FLO, loc);
    expr->f = f;
    return expr;
}

Expr* vp_expr_str(SrcLoc loc, Str* str)
{
    Expr* expr = expr_new(EX_STR, loc);
    expr->name = str;
    return expr;
}

Expr* vp_expr_name(SrcLoc loc, Str* name)
{
    Expr* expr = expr_new(EX_NAME, loc);
    expr->name = name;
    return expr;
}

Expr* vp_expr_comp(SrcLoc loc, Field* fields)
{
    Expr* expr = expr_new(EX_COMPOUND, loc);
    expr->comp.fields = fields;
    return expr;
}

Expr* vp_expr_call(SrcLoc loc, Expr* e, Expr** args)
{
    Expr* expr = expr_new(EX_CALL, loc);
    expr->call.expr = e;
    expr->call.args = args;
    return expr;
}

Expr* vp_expr_idx(SrcLoc loc, Expr* e, Expr* idx)
{
    Expr* expr = expr_new(EX_IDX, loc);
    expr->idx.expr = e;
    expr->idx.index = idx;
    return expr;
}

Expr* vp_expr_field(SrcLoc loc, Expr* e, Str* name)
{
    Expr* expr = expr_new(EX_FIELD, loc);
    expr->field.expr = e;
    expr->field.name = name;
    return expr;
}

Expr* vp_expr_cast(SrcLoc loc, TypeSpec* spec, Expr* e)
{
    Expr* expr = expr_new(EX_CAST, loc);
    expr->cast.spec = spec;
    expr->cast.expr = e;
    return expr;
}

/* -- AST statements ------------------------------------------------ */

static Stmt* stmt_new(StmtKind kind, SrcLoc loc)
{
    Stmt* st = ast_alloc(sizeof(*st));
    st->kind = kind;
    st->loc = loc;
    return st;
}

Stmt* vp_stmt_assign(SrcLoc loc, Expr* lhs, Expr* rhs)
{
    Stmt* st = stmt_new(ST_ASSIGN, loc);
    st->lhs = lhs;
    st->rhs = rhs;
    return st;
}

Stmt* vp_stmt_expr(SrcLoc loc, Expr* e)
{
    Stmt* st = stmt_new(ST_EXPR, loc);
    st->expr = e;
    return st;
}

Stmt* vp_stmt_decl(SrcLoc loc, Decl* d)
{
    Stmt* st = stmt_new(ST_DECL, loc);
    st->decl = d;
    return st;
}

Stmt* vp_stmt_block(SrcLoc loc, Stmt** block)
{
    Stmt* st = stmt_new(ST_BLOCK, loc);
    st->block = block;
    return st;
}

Stmt* vp_stmt_return(SrcLoc loc, Expr* e)
{
    Stmt* st = stmt_new(ST_RETURN, loc);
    st->expr = e;
    return st;
}

/* -- AST declarations ---------------------------------------------- */

static Decl* decl_new(DeclKind kind, SrcLoc loc, Str* name)
{
    Decl* d = ast_alloc(sizeof(*d));
    d->kind = kind;
    d->loc = loc;
    d->name = name;
    return d;
}

Decl* vp_decl_var(SrcLoc loc, Str* name, TypeSpec* spec, Expr* e)
{
    Decl* d = decl_new(DECL_VAR, loc, name);
    d->var.spec = spec;
    d->var.expr = e;
    return d;
}

Decl* vp_decl_fn(SrcLoc loc, TypeSpec* ret, Str* name)
{
    Decl* d = decl_new(DECL_FN, loc, name);;
    d->fn.ret = ret;
    return d;
}

Decl* vp_decl_type(SrcLoc loc, Str* name, TypeSpec* spec)
{
    Decl* d = decl_new(DECL_TYPE, loc, name);;
    d->ts.spec = spec;
    return d;
}

Decl* vp_decl_aggr(SrcLoc loc, DeclKind kind, Str* name, Aggregate* agr)
{
    Decl* d = decl_new(kind, loc, name);
    d->agr = agr;
    return d;
}

Decl* vp_decl_enum(Str* name, TypeSpec* spec)
{
    return NULL;
}

Aggregate* vp_aggr_new(SrcLoc loc, AggregateKind kind, AggregateItem* items)
{
    Aggregate* ag = ast_alloc(sizeof(*ag));
    ag->kind = kind;
    ag->items = items;
    return ag;
}

/* -- AST type specs ------------------------------------------------ */

static TypeSpec* new_typespec(TypeSpecKind kind, SrcLoc loc)
{
    TypeSpec* ts = ast_alloc(sizeof(*ts));
    ts->kind = kind;
    ts->loc = loc;
    return ts;
}

TypeSpec* vp_typespec_name(SrcLoc loc, Str* name)
{
    TypeSpec* ts = new_typespec(SPEC_NAME, loc);
    ts->name = name;
    return ts;
}

TypeSpec* vp_typespec_type(SrcLoc loc, Type* ty)
{
    TypeSpec* ts = new_typespec(SPEC_TYPE, loc);
    ts->ty = ty;
    return ts;
}

TypeSpec* vp_typespec_ptr(SrcLoc loc, TypeSpec* base)
{
    TypeSpec* ts = new_typespec(SPEC_PTR, loc);
    ts->ptr = base;
    return ts;
}

TypeSpec* vp_typespec_arr(SrcLoc loc, TypeSpec* base, Expr* e)
{
    TypeSpec* ts = new_typespec(SPEC_ARRAY, loc);
    ts->arr.base = base;
    ts->arr.expr = e;
    return ts;
}

TypeSpec* vp_typespec_fn(SrcLoc loc, TypeSpec* ret, TypeSpec** args)
{
    TypeSpec* ts = new_typespec(SPEC_FUNC, loc);
    ts->fn.ret = ret;
    ts->fn.args = args;
    return ts;
}