/*
** vp_ast.c
** Abstract Syntax Tree
*/


#include "vp_ast.h"
#include "vp_mem.h"
#include "vp_str.h"
#include "vp_type.h"

const char* const vp_ast_binop[] = {
    "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", "<=", ">", ">=", "and", "or"
};

const char* const vp_ast_unary[] = {
    "-", "not ", "~"
};

/* -- AST expressions ----------------------------------------------- */

static Expr* expr_new(ExprKind kind)
{
    Expr* expr = (Expr*)vp_mem_alloc(sizeof(*expr));
    expr->kind = kind;
    return expr;
}

Expr* vp_expr_binop(SrcLoc loc, ExprKind kind, Expr* lhs, Expr* rhs)
{
    Expr* expr = expr_new(kind);
    expr->loc = loc;
    expr->binop.lhs = lhs;
    expr->binop.rhs = rhs;
    return expr;
}

Expr* vp_expr_unary(SrcLoc loc, ExprKind kind, Expr* unary)
{
    Expr* expr = expr_new(kind);
    expr->loc = loc;
    expr->unary = unary;
    return expr;
}

Expr* vp_expr_false(SrcLoc loc)
{
    Expr* expr = expr_new(EX_FALSE);
    expr->loc = loc;
    expr->b = false;
    return expr;
}

Expr* vp_expr_true(SrcLoc loc)
{
    Expr* expr = expr_new(EX_TRUE);
    expr->loc = loc;
    expr->b = true;
    return expr;
}

Expr* vp_expr_nil(SrcLoc loc)
{
    Expr* expr = expr_new(EX_NIL);
    expr->loc = loc;
    return expr;
}

Expr* vp_expr_ilit(SrcLoc loc, int64_t i)
{
    Expr* expr = expr_new(EX_INT);
    expr->loc = loc;
    expr->i = i;
    return expr;
}

Expr* vp_expr_flit(SrcLoc loc, double n)
{
    Expr* expr = expr_new(EX_NUM);
    expr->loc = loc;
    expr->n = n;
    return expr;
}

Expr* vp_expr_str(SrcLoc loc, Str* str)
{
    Expr* expr = expr_new(EX_STR);
    expr->loc = loc;
    expr->name = str;
    return expr;
}

Expr* vp_expr_name(SrcLoc loc, Str* name)
{
    Expr* expr = expr_new(EX_NAME);
    expr->loc = loc;
    expr->name = name;
    return expr;
}

Expr* vp_expr_comp(SrcLoc loc, Field* fields)
{
    Expr* expr = expr_new(EX_COMPOUND);
    expr->loc = loc;
    expr->comp.fields = fields;
    return expr;
}

Expr* vp_expr_call(SrcLoc loc, Expr* e, Expr** args)
{
    Expr* expr = expr_new(EX_CALL);
    expr->loc = loc;
    expr->call.expr = e;
    expr->call.args = args;
    return expr;
}

Expr* vp_expr_idx(SrcLoc loc, Expr* e, Expr* idx)
{
    Expr* expr = expr_new(EX_IDX);
    expr->loc = loc;
    expr->idx.expr = e;
    expr->idx.index = idx;
    return expr;
}

Expr* vp_expr_field(SrcLoc loc, Expr* e, Str* name)
{
    Expr* expr = expr_new(EX_FIELD);
    expr->loc = loc;
    expr->field.expr = e;
    expr->field.name = name;
    return expr;
}

Expr* vp_expr_cast(SrcLoc loc, TypeSpec* spec, Expr* e)
{
    Expr* expr = expr_new(EX_CAST);
    expr->cast.spec = spec;
    expr->cast.expr = e;
    return expr;
}

/* -- AST statements ------------------------------------------------ */

static Stmt* stmt_new(StmtKind kind)
{
    Stmt* st = (Stmt*)vp_mem_calloc(1, sizeof(*st));
    st->kind = kind;
    return st;
}

Stmt* vp_stmt_assign(SrcLoc loc, Expr* lhs, Expr* rhs)
{
    Stmt* st = stmt_new(ST_ASSIGN);
    st->loc = loc;
    st->lhs = lhs;
    st->rhs = rhs;
    return st;
}

Stmt* vp_stmt_expr(SrcLoc loc, Expr* e)
{
    Stmt* st = stmt_new(ST_EXPR);
    st->loc = loc;
    st->expr = e;
    return st;
}

Stmt* vp_stmt_decl(SrcLoc loc, Decl* d)
{
    Stmt* st = stmt_new(ST_DECL);
    st->loc = loc;
    st->decl = d;
    return st;
}

Stmt* vp_stmt_block(SrcLoc loc, Stmt** block)
{
    Stmt* st = stmt_new(ST_BLOCK);
    st->loc = loc;
    st->block = block;
    return st;
}

Stmt* vp_stmt_return(SrcLoc loc, Expr* e)
{
    Stmt* st = stmt_new(ST_RETURN);
    st->loc = loc;
    st->expr = e;
    return st;
}

/* -- AST declarations ---------------------------------------------- */

Decl* vp_decl_var(SrcLoc loc, Str* name, TypeSpec* spec, Expr* e)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_VAR;
    d->loc = loc;
    d->name = name;
    d->var.spec = spec;
    d->var.expr = e;
    return d;
}

Decl* vp_decl_fn(SrcLoc loc, TypeSpec* ret, Str* name)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_FN;
    d->loc = loc;
    d->name = name;
    d->fn.ret = ret;
    return d;
}

Decl* vp_decl_type(SrcLoc loc, Str* name, TypeSpec* spec)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_TYPE;
    d->name = name;
    d->ts.spec = spec;
    return d;
}

Decl* vp_decl_aggr(SrcLoc loc, DeclKind kind, Str* name, Aggregate* agr)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = kind;
    d->loc = loc;
    d->agr = agr;
    d->name = name;
    return d;
}

Decl* vp_decl_enum(Str* name, TypeSpec* spec)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_ENUM;
    d->enm.spec = spec;
    return d;
}

Aggregate* vp_aggr_new(SrcLoc loc, AggregateKind kind, AggregateItem* items)
{
    Aggregate* ag = (Aggregate*)vp_mem_calloc(1, sizeof(*ag));
    ag->kind = kind;
    ag->items = items;
    return ag;
}

/* -- AST type specs ------------------------------------------------ */

TypeSpec* vp_typespec_name(SrcLoc loc, Str* name)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_NAME;
    ts->loc = loc;
    ts->name = name;
    return ts;
}

TypeSpec* vp_typespec_type(SrcLoc loc, Type* ty)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_TYPE;
    ts->loc = loc;
    ts->ty = ty;
    return ts;
}

TypeSpec* vp_typespec_ptr(SrcLoc loc, TypeSpec* base)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_PTR;
    ts->loc = loc;
    ts->ptr = base;
    return ts;
}

TypeSpec* vp_typespec_array(SrcLoc loc, TypeSpec* base, Expr* e)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_ARRAY;
    ts->arr.base = base;
    ts->arr.expr = e;
    return ts;
}

TypeSpec* vp_typespec_fn(SrcLoc loc, TypeSpec* ret, TypeSpec** args)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_FUNC;
    ts->fn.ret = ret;
    ts->fn.args = args;
    return ts;
}