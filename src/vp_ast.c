/*
** vp_ast.c
** Abstract Syntax Tree
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vp_ast.h"
#include "vp_def.h"
#include "vp_mem.h"
#include "vp_str.h"
#include "vp_vec.h"

/* -- AST expressions ----------------------------------------------- */

static Expr* expr_new(ExprKind kind)
{
    Expr* expr = (Expr*)vp_mem_alloc(sizeof(*expr));
    expr->kind = kind;
    return expr;
}

Expr* vp_expr_binop(SrcPos sp, ExprKind kind, Expr* lhs, Expr* rhs)
{
    Expr* expr = expr_new(kind);
    expr->sp = sp;
    expr->binop.lhs = lhs;
    expr->binop.rhs = rhs;
    return expr;
}

Expr* vp_expr_unary(SrcPos sp, ExprKind kind, Expr* unary)
{
    Expr* expr = expr_new(kind);
    expr->sp = sp;
    expr->unary = unary;
    return expr;
}

Expr* vp_expr_false(SrcPos sp)
{
    Expr* expr = expr_new(EX_FALSE);
    expr->sp = sp;
    expr->b = false;
    return expr;
}

Expr* vp_expr_true(SrcPos sp)
{
    Expr* expr = expr_new(EX_TRUE);
    expr->sp = sp;
    expr->b = true;
    return expr;
}

Expr* vp_expr_nil(SrcPos sp)
{
    Expr* expr = expr_new(EX_NIL);
    expr->sp = sp;
    return expr;
}

Expr* vp_expr_ilit(SrcPos sp, int64_t i)
{
    Expr* expr = expr_new(EX_INT);
    expr->sp = sp;
    expr->i = i;
    return expr;
}

Expr* vp_expr_flit(SrcPos sp, double n)
{
    Expr* expr = expr_new(EX_NUM);
    expr->sp = sp;
    expr->n = n;
    return expr;
}

Expr* vp_expr_str(SrcPos sp, Str* str)
{
    Expr* expr = expr_new(EX_STR);
    expr->sp = sp;
    expr->name = str;
    return expr;
}

Expr* vp_expr_name(SrcPos sp, Str* name)
{
    Expr* expr = expr_new(EX_NAME);
    expr->sp = sp;
    expr->name = name;
    return expr;
}

Expr* vp_expr_comp(SrcPos sp, Field* fields)
{
    Expr* expr = expr_new(EX_COMPOUND);
    expr->sp = sp;
    expr->comp.fields = fields;
    return expr;
}

Expr* vp_expr_call(SrcPos sp, Expr* e, Expr** args)
{
    Expr* expr = expr_new(EX_CALL);
    expr->sp = sp;
    expr->call.expr = e;
    expr->call.args = args;
    return expr;
}

Expr* vp_expr_idx(SrcPos sp, Expr* e, Expr* idx)
{
    Expr* expr = expr_new(EX_IDX);
    expr->sp = sp;
    expr->idx.expr = e;
    expr->idx.index = idx;
    return expr;
}

Expr* vp_expr_field(SrcPos sp, Expr* e, Str* name)
{
    Expr* expr = expr_new(EX_FIELD);
    expr->sp = sp;
    expr->field.expr = e;
    expr->field.name = name;
    return expr;
}

Expr* vp_expr_cast(SrcPos sp, TypeSpec* spec, Expr* e)
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

Stmt* vp_stmt_assign(SrcPos sp, Expr* lhs, Expr* rhs)
{
    Stmt* st = stmt_new(ST_ASSIGN);
    st->sp = sp;
    st->lhs = lhs;
    st->rhs = rhs;
    return st;
}

Stmt* vp_stmt_expr(SrcPos sp, Expr* e)
{
    Stmt* st = stmt_new(ST_EXPR);
    st->sp = sp;
    st->expr = e;
    return st;
}

Stmt* vp_stmt_decl(SrcPos sp, Decl* d)
{
    Stmt* st = stmt_new(ST_DECL);
    st->sp = sp;
    st->decl = d;
    return st;
}

Stmt* vp_stmt_block(SrcPos sp, Stmt** block)
{
    Stmt* st = stmt_new(ST_BLOCK);
    st->sp = sp;
    st->block = block;
    return st;
}

Stmt* vp_stmt_return(SrcPos sp, Expr* e)
{
    Stmt* st = stmt_new(ST_RETURN);
    st->sp = sp;
    st->expr = e;
    return st;
}

/* -- AST declarations ---------------------------------------------- */

Decl* vp_decl_var(SrcPos sp, Str* name, TypeSpec* spec, Expr* e)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_VAR;
    d->sp = sp;
    d->name = name;
    d->var.spec = spec;
    d->var.expr = e;
    return d;
}

Decl* vp_decl_fn(SrcPos sp, TypeSpec* ret, Str* name)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_FN;
    d->sp = sp;
    d->name = name;
    d->fn.ret = ret;
    return d;
}

Decl* vp_decl_type(SrcPos sp, Str* name, TypeSpec* spec)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = DECL_TYPE;
    d->name = name;
    d->ts.spec = spec;
    return d;
}

Decl* vp_decl_aggr(SrcPos sp, DeclKind kind, Str* name, Aggregate* agr)
{
    Decl* d = (Decl*)vp_mem_calloc(1, sizeof(*d));
    d->kind = kind;
    d->sp = sp;
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

Aggregate* vp_aggr_new(SrcPos sp, AggregateKind kind, AggregateItem* items)
{
    Aggregate* ag = (Aggregate*)vp_mem_calloc(1, sizeof(*ag));
    ag->kind = kind;
    ag->items = items;
    return ag;
}

/* -- AST type specs ------------------------------------------------ */

TypeSpec* vp_typespec_name(SrcPos sp, Str* name)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_NAME;
    ts->sp = sp;
    ts->name = name;
    return ts;
}

TypeSpec* vp_typespec_type(SrcPos sp, Type* ty)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_TYPE;
    ts->sp = sp;
    ts->ty = ty;
    return ts;
}

TypeSpec* vp_typespec_ptr(SrcPos sp, TypeSpec* base)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_PTR;
    ts->sp = sp;
    ts->ptr = base;
    return ts;
}

TypeSpec* vp_typespec_array(SrcPos sp, TypeSpec* base, Expr* e)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_ARRAY;
    ts->arr.base = base;
    ts->arr.expr = e;
    return ts;
}

TypeSpec* vp_typespec_fn(SrcPos sp, TypeSpec* ret, TypeSpec** args)
{
    TypeSpec* ts = (TypeSpec*)vp_mem_calloc(1, sizeof(*ts));
    ts->kind = SPEC_FUNC;
    ts->fn.ret = ret;
    ts->fn.args = args;
    return ts;
}

/* -- AST printing -------------------------------------------------- */

static const char* const ast_binnames[] = {
    "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", "<=", ">", ">=", "and", "or"
};

static const char* const ast_unarynames[] = {
    "-", "not", "~"
};

static int indent = 0;

static void ast_print_expr(Expr* e);

static void ast_print_indent()
{
    printf("%.*s", 4*indent, "                                                                      ");
}

static void ast_print_typespec(TypeSpec* spec)
{
    switch(spec->kind)
    {
        case SPEC_NAME:
            printf("%s", str_data(spec->name));
            break;
        case SPEC_TYPE:
            printf("%s", vp_type_names[spec->ty->kind]);
            break;
        case SPEC_PTR:
            ast_print_typespec(spec->ptr);
            printf("*");
            break;
        case SPEC_ARRAY:
            ast_print_typespec(spec->arr.base);
            printf("[");
            if(spec->arr.expr != NULL)
                ast_print_expr(spec->arr.expr);
            printf("]");
            break;
        case SPEC_FUNC:
            printf("fn(");
            for(uint32_t i = 0; i < vec_len(spec->fn.args); i++)
            {
                TypeSpec* ts = spec->fn.args[i];
                ast_print_typespec(ts);
                if(i != vec_len(spec->fn.args) - 1)
                {
                    printf(", ");
                }
            }
            printf(") : ");
            ast_print_typespec(spec->fn.ret);
            break;
        default:
            vp_assertX(0, "Unknown typespec");
            break;
    }
}

static void ast_print_aggr(Aggregate* agr)
{
    for(uint32_t i = 0; i < vec_len(agr->items); i++)
    {
        ast_print_indent();
        AggregateItem* item = &agr->items[i];
        for(uint32_t j = 0; j < vec_len(item->names); j++)
        {
            Str* name = item->names[j];
            printf("%s", str_data(name));
            if(j != vec_len(item->names) - 1)
            {
                printf(", ");
            }
        }
        printf(" : ");
        ast_print_typespec(item->type);
        printf("\n");
    }
}

static void ast_print_expr(Expr* e)
{
    switch(e->kind)
    {
        case EX_TRUE:
            printf("true");
            break;
        case EX_FALSE:
            printf("false");
            break;
        case EX_NIL:
            printf("nil");
            break;
        case EX_INT:
        {
            int64_t i = e->i;
            printf("%lli", i);
            break;
        }
        case EX_NUM:
        {
            double n = e->n;
            printf("%g", n);
            break;
        }
        case EX_STR:
        {
            printf("\"%.*s\"", e->name->len, str_data(e->name));
            break;
        }
        case EX_NAME:
        {
            printf("%.*s", e->name->len, str_data(e->name));
            break;
        }
        case EX_COMPOUND:
        {
            printf("{");
            for(Field* c = e->comp.fields; c != vec_end(e->comp.fields); c++)
            {
                if(c->kind == FIELD_IDX)
                {
                    printf("[");
                    ast_print_expr(c->idx);
                    printf("] = ");
                }
                else if(c->kind == FIELD_NAME)
                {
                    printf("%s = ", str_data(c->name));
                }
                ast_print_expr(c->init);
                if(c != vec_end(e->comp.fields) - 1)
                {
                    printf(", ");
                }
            }
            printf("}");
            break;
        }
        case EX_NEG:
        case EX_NOT:
        case EX_BNOT:
        {
            printf("%s", ast_unarynames[e->kind - EX_UNARY]);
            printf("(");
            ast_print_expr(e->unary);
            printf(")");
            break;
        }
        case EX_ADD:
        case EX_SUB:
        case EX_MUL:
        case EX_DIV:
        case EX_MOD:
        case EX_BAND:
        case EX_BOR:
        case EX_BXOR:
        case EX_LSHIFT:
        case EX_RSHIFT:
        case EX_EQ:
        case EX_NOTEQ:
        case EX_LT:
        case EX_LE:
        case EX_GT:
        case EX_GE:
        case EX_AND:
        case EX_OR:
        {
            printf("(");
            ast_print_expr(e->binop.lhs);
            printf(" %s ", ast_binnames[e->kind - EX_BINOP]);
            ast_print_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_CAST:
        {
            printf("cast(");
            ast_print_typespec(e->cast.spec);
            printf(", ");
            ast_print_expr(e->cast.expr);
            printf(")");
            break;
        }
        default:
            vp_assertX(0, "Unknown expression");
            break;
    }
}

void ast_print_stmt(Stmt* stm)
{
    switch(stm->kind)
    {
        case ST_RETURN:
            ast_print_indent();
            printf("return ");
            ast_print_expr(stm->expr);
            printf("\n");
            break;
        case ST_ASSIGN:
            ast_print_indent();
            ast_print_expr(stm->lhs);
            printf(" = ");
            ast_print_expr(stm->rhs);
            printf("\n");
            break;
        case ST_EXPR:
        {
            ast_print_indent();
            ast_print_expr(stm->expr);
            printf("\n");
            break;
        }
        case ST_BLOCK:
        {
            for(uint32_t i = 0; i < vec_len(stm->block); i++)
            {
                Stmt* st = stm->block[i];
                ast_print_stmt(st);
            }
            break;
        }
        case ST_DECL:
        {
            vp_ast_print(stm->decl);
            break;
        }
        default:
            vp_assertX(0, "Unknown statement");
            break;
    }
}

void vp_ast_print(Decl* d)
{
    switch(d->kind)
    {
        case DECL_TYPE:
            printf("type %s = ", str_data(d->name));
            ast_print_typespec(d->ts.spec);
            printf("\n");
            break;
        case DECL_VAR:
        {
            ast_print_indent();
            printf("var %s", str_data(d->name));
            if(d->var.spec)
            {
                printf(" : ");
                ast_print_typespec(d->var.spec);
            }
            printf(" = ");
            ast_print_expr(d->var.expr);
            printf("\n");
            break;
        }
        case DECL_FN:
            printf("fn %s(", str_data(d->name));
            for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
            {
                Param* param = &d->fn.params[i];
                printf("%s : ", str_data(param->name));
                ast_print_typespec(param->spec);
                if(i != vec_len(d->fn.params) - 1)
                {
                    printf(", ");
                }
            }
            printf(")\n{\n");
            indent++;
            ast_print_stmt(d->fn.body);
            indent--;
            printf("}\n");
            break;
        case DECL_STRUCT:
            printf("struct %s\n{\n", str_data(d->name));
            indent++;
            ast_print_aggr(d->agr);
            indent--;
            printf("}\n");
            break;
        default:
            vp_assertX(0, "Unknown decl");
            break;
    }
}