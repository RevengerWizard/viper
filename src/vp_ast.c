/*
** vp_ast.c
** Abstract Syntax Tree
*/

#include <stdlib.h>
#include <string.h>

#include "vp_ast.h"

static Expr* expr_new(ExprKind kind)
{
    Expr* expr = (Expr*)malloc(sizeof(*expr));
    expr->kind = kind;
    return expr;
}

Expr* vp_expr_binop(ExprKind kind, Type* ty, Expr* lhs, Expr* rhs)
{
    Expr* expr = expr_new(kind);
    expr->ty = ty;
    expr->binop.lhs = lhs;
    expr->binop.rhs = rhs;
    return expr;
}

Expr* vp_expr_unary(ExprKind kind, Type* ty, Expr* unary)
{
    Expr* expr = expr_new(kind);
    expr->ty = ty;
    expr->unary = unary;
    return expr;
}

Expr* vp_expr_ilit(Type* ty, int64_t i)
{
    Expr* expr = expr_new(EX_INT);
    expr->ty = ty;
    expr->i = i;
    return expr;
}

Expr* vp_expr_flit(Type* ty, double n)
{
    Expr* expr = expr_new(EX_NUM);
    expr->ty = ty;
    expr->n = n;
    return expr;
}

Expr* vp_expr_name(Str* name, Type* ty, Scope* scope)
{
    Expr* expr = expr_new(EX_NAME);
    expr->ty = ty;
    expr->name.name = name;
    expr->name.scope = scope;
    return expr;
}

static Stmt* stmt_new(StmtKind kind)
{
    Stmt* st = (Stmt*)calloc(1, sizeof(*st));
    st->kind = kind;
    return st;
}

Stmt* vp_stmt_expr(Expr* e)
{
    Stmt* st = stmt_new(ST_EXPR);
    st->expr = e;
    return st;
}

Stmt* vp_stmt_block(Scope* scope)
{
    Stmt* st = stmt_new(ST_BLOCK);
    st->block.scope = scope;
    return st;
}

Stmt* vp_stmt_return(Expr* e)
{
    Stmt* st = stmt_new(ST_RETURN);
    st->expr = e;
    return st;
}

Stmt* vp_stmt_var(VarInfo* vi)
{
    Stmt* st = stmt_new(ST_VAR);
    st->vi = vi;
    return st;
}

VarInit* vp_varinit_new(InitKind kind)
{
    VarInit* init = (VarInit*)calloc(1, sizeof(*init));
    init->kind = kind;
    return init;
}

Fn* vp_fn_new(Type* type, Str* name)
{
    Fn* fn = (Fn*)calloc(1, sizeof(*fn));
    fn->type = type;
    fn->name = name;
    memset(&fn->params, 0, sizeof(fn->params));
    return fn;
}