/*
** vp_print.c
** Data structure printing
*/

#include <stdio.h>

#include "vp_print.h"
#include "vp_ast.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_vec.h"
#include "vp_tab.h"
#include "vp_sema.h"

static int indent = 0;

static void print_indent()
{
    printf("%.*s", 4*indent, "                                                                      ");
}

/* -- String interning printing ------------------------------------- */

void vp_print_strintern(void)
{
    printf("\n-- str intern --\n");
    Tab* tab = &V->strtab;
    for(uint32_t i = 0; i < tab->size; i++)
    {
        TabEntry* entry = &tab->entries[i];
        if(entry->key != NULL)
        {
            printf("'%s'; %d\n", str_data(entry->key), entry->key->len);
        }
    }
}

/* -- Type printing ------------------------------------------------- */

void vp_print_type(Type* ty)
{
    switch(ty->kind)
    {
        case TY_bool:
        case TY_uint8:
        case TY_int8:
        case TY_uint16:
        case TY_int16:
        case TY_uint32:
        case TY_int32:
        case TY_uint64:
        case TY_int64:
        case TY_float:
        case TY_double:
        case TY_void:
            printf("%s", type_name(ty));
            break;
        case TY_ptr:
            vp_print_type(ty->p);
            printf("*");
            break;
        case TY_array:
            vp_print_type(ty->p);
            printf("[");
            printf("%d", ty->len);
            printf("]");
            break;
        case TY_func:
            printf("fn(");
            for(uint32_t i = 0; i < vec_len(ty->fn.params); i++)
            {
                Type* pt = ty->fn.params[i];
                vp_print_type(pt);
                if(i != vec_len(ty->fn.params) - 1)
                {
                    printf(", ");
                }
            }
            printf(") : ");
            vp_print_type(ty->fn.ret);
            break;
        case TY_union:
        case TY_struct:
            if(ty->sym)
            {
                /* May need something better to avoid stack overflow */
                printf("%s", str_data(ty->sym->name));
                break;
            }
            if(ty->kind == TY_union)
                printf("union");
            else
                printf("struct");
            if(ty->st.name)
            {
                printf(" %s", str_data(ty->st.name));
            }
            printf("\n{\n");
            for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
            {
                indent++;
                printf("%s : ", str_data(ty->st.fields[i].name));
                vp_print_type(ty->st.fields[i].ty);
                printf("\n");
                indent--;
            }
            printf("}");
            break;
        default:
            vp_assertX(0, "?");
            break;
    }
}

void vp_print_typecache(void)
{
    printf("\n-- cache ptr --\n");
    for(uint32_t i = 0; i < V->cacheptr.size; i++)
    {
        if(V->cacheptr.keys[i])
        {
            vp_print_type((Type*)(uintptr_t)V->cacheptr.vals[i]);
            printf("\n");
        }
    }
    printf("\n-- cache arr --\n");
    for(uint32_t i = 0; i < vec_len(V->cachearr); i++)
    {
        vp_print_type(V->cachearr[i]);
        printf("\n");
    }
    printf("\n-- cache func --\n");
    for(uint32_t i = 0; i < vec_len(V->cachefunc); i++)
    {
        vp_print_type(V->cachefunc[i]);
        printf("\n");
    }
}

/* -- AST printing -------------------------------------------------- */

static void print_ast_expr(Expr* e);

static void print_typespec(TypeSpec* spec)
{
    switch(spec->kind)
    {
        case SPEC_NAME:
            printf("%s", str_data(spec->name));
            break;
        case SPEC_TYPE:
            printf("%s", type_name(spec->ty));
            break;
        case SPEC_PTR:
            print_typespec(spec->ptr);
            printf("*");
            break;
        case SPEC_ARRAY:
            print_typespec(spec->arr.base);
            printf("[");
            if(spec->arr.expr != NULL)
                print_ast_expr(spec->arr.expr);
            printf("]");
            break;
        case SPEC_FUNC:
            printf("fn(");
            for(uint32_t i = 0; i < vec_len(spec->fn.args); i++)
            {
                TypeSpec* ts = spec->fn.args[i];
                print_typespec(ts);
                if(i != vec_len(spec->fn.args) - 1)
                {
                    printf(", ");
                }
            }
            printf(") : ");
            print_typespec(spec->fn.ret);
            break;
        case SPEC_TYPEOF:
            printf("typeof(");
            print_ast_expr(spec->expr);
            printf(")");
            break;
        default:
            vp_assertX(0, "unknown typespec");
            break;
    }
}

static void print_ast_aggr(Aggregate* agr)
{
    for(uint32_t i = 0; i < vec_len(agr->items); i++)
    {
        print_indent();
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
        print_typespec(item->type);
        printf("\n");
    }
}

static void print_ast_expr(Expr* e)
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
        case EX_CHAR:
        {
            int i = e->i;
            printf("%c", i);
            break;
        }
        case EX_UINT:
        {
            uint64_t u = e->u;
            printf("%llu", u);
            break;
        }
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
        case EX_FLO:
        {
            float f = e->f;
            printf("%f", f);
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
            if(e->comp.spec)
            {
                print_typespec(e->comp.spec);
                printf(" ");
            }
            printf("{");
            for(Field* c = e->comp.fields; c != vec_end(e->comp.fields); c++)
            {
                if(c->kind == FIELD_IDX)
                {
                    printf("[");
                    print_ast_expr(c->idx);
                    printf("] = ");
                }
                else if(c->kind == FIELD_NAME)
                {
                    printf("%s = ", str_data(c->name));
                }
                print_ast_expr(c->init);
                if(c != vec_end(e->comp.fields) - 1)
                {
                    printf(", ");
                }
            }
            printf("}");
            break;
        }
        case EX_REF:
        case EX_DEREF:
        case EX_NEG:
        case EX_NOT:
        case EX_BNOT:
        {
            printf("%s", ast_unaryname(e->kind));
            print_ast_expr(e->unary);
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
            print_ast_expr(e->binop.lhs);
            printf(" %s ", ast_binname(e->kind));
            print_ast_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_CAST:
        {
            printf("cast(");
            print_typespec(e->cast.spec);
            printf(", ");
            print_ast_expr(e->cast.expr);
            printf(")");
            break;
        }
        case EX_CALL:
        {
            print_ast_expr(e->call.expr);
            printf("(");
            for(uint32_t i = 0; i < vec_len(e->call.args); i++)
            {
                Expr* arg = e->call.args[i];
                print_ast_expr(arg);
                if(i != vec_len(e->call.args) - 1)
                {
                    printf(", ");
                }
            }
            printf(")");
            break;
        }
        case EX_SIZEOF:
        {
            printf("sizeof(");
            print_typespec(e->spec);
            printf(")");
            break;
        }
        case EX_ALIGNOF:
        {
            printf("alignof(");
            print_typespec(e->spec);
            printf(")");
            break;
        }
        case EX_OFFSETOF:
        {
            printf("offsetof(");
            print_typespec(e->ofst.spec);
            printf(", ");
            printf("%s", str_data(e->ofst.name));
            printf(")");
            break;
        }
        default:
            vp_assertX(0, "unknown expression %d", e->kind);
            break;
    }
}

void print_ast_stmt(Stmt* stm)
{
    switch(stm->kind)
    {
        case ST_RETURN:
            print_indent();
            printf("return ");
            print_ast_expr(stm->expr);
            printf("\n");
            break;
        case ST_ASSIGN:
            print_indent();
            print_ast_expr(stm->lhs);
            printf(" = ");
            print_ast_expr(stm->rhs);
            printf("\n");
            break;
        case ST_EXPR:
        {
            print_indent();
            if(stm->expr->ty)
            {
                printf("(");
                vp_print_type(stm->expr->ty);
                printf(") ");
            }
            print_ast_expr(stm->expr);
            printf("\n");
            break;
        }
        case ST_BLOCK:
        {
            for(uint32_t i = 0; i < vec_len(stm->block); i++)
            {
                Stmt* st = stm->block[i];
                print_ast_stmt(st);
            }
            break;
        }
        case ST_DECL:
        {
            vp_print_ast(stm->decl);
            break;
        }
        default:
            vp_assertX(0, "unknown statement");
            break;
    }
}

void vp_print_ast(Decl* d)
{
    switch(d->kind)
    {
        case DECL_TYPE:
            printf("type %s = ", str_data(d->name));
            print_typespec(d->ts.spec);
            printf("\n");
            break;
        case DECL_VAR:
        {
            print_indent();
            printf("var %s", str_data(d->name));
            if(d->var.spec)
            {
                printf(" : ");
                print_typespec(d->var.spec);
            }
            if(d->var.expr)
            {
                printf(" = ");
                if(d->var.expr->ty)
                {
                    printf("(");
                    vp_print_type(d->var.expr->ty);
                    printf(") ");
                }
                print_ast_expr(d->var.expr);
            }
            printf("\n");
            break;
        }
        case DECL_FN:
            printf("fn %s(", str_data(d->name));
            for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
            {
                Param* param = &d->fn.params[i];
                printf("%s : ", str_data(param->name));
                print_typespec(param->spec);
                if(i != vec_len(d->fn.params) - 1)
                {
                    printf(", ");
                }
            }
            printf(")\n{\n");
            indent++;
            print_ast_stmt(d->fn.body);
            indent--;
            printf("}\n");
            break;
        case DECL_STRUCT:
            printf("struct %s\n{\n", str_data(d->name));
            indent++;
            print_ast_aggr(d->agr);
            indent--;
            printf("}\n");
            break;
        default:
            vp_assertX(0, "unknown decl");
            break;
    }
}