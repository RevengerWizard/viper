/*
** vp_dump.c
** Data structure printing
*/

#include <stdio.h>

#include "vp_dump.h"
#include "vp_ast.h"
#include "vp_ir.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_vec.h"
#include "vp_tab.h"
#include "vp_sema.h"

static int indent = 0;

static void dump_indent()
{
    printf("%.*s", 4*indent, "                                                                      ");
}

/* -- IR dump ------------------------------------------------------- */

static void dump_vreg(VReg* vr)
{
    vp_assertX(vr, "empty vreg");
    if(vr->flag & VRF_CONST)
    {
        if(vr->flag & VRF_NUM)
        {
            if(vr->vsize == VRegSize4)
                printf("%ff", vr->n);
            else
                printf("%.14gf", vr->n);
        }
        else if(vr->flag & VRF_INT)
            printf("#%lli", vr->i64);
        else
            printf("#%llu", vr->u64);
    }
    else
    {
        printf("v%d", vr->virt);
    }
}

static const char* const kcond[] = {
    NULL, "MP", "EQ", "NEQ", "LT", "LE", "GT", "GE",
    NULL, "MP", "EQ", "NEQ", "LT", "LE", "GT", "GE"};

static const char* const kbinop[] = {
    "ADD", "SUB", "MUL", "DIV", "MOD",
    "BAND", "BOR", "BXOR", "LSHIFT", "RSHIFT"
};

static void dump_ir(IR* ir)
{
    switch(ir->kind)
    {
        case IR_ADD:
        case IR_SUB:
        case IR_MUL:
        case IR_DIV:
        case IR_MOD:
        case IR_BAND:
        case IR_BOR:
        case IR_BXOR:
        case IR_LSHIFT:
        case IR_RSHIFT:
            printf("%s ", kbinop[ir->kind - IR_ADD]);
            dump_vreg(ir->dst);
            printf(", ");
            dump_vreg(ir->src1);
            printf(", ");
            dump_vreg(ir->src2);
            printf("\n");
            break;
        case IR_BOFS:
            printf("BOFS ");
            dump_vreg(ir->dst);
            printf("\n");
            break;
        case IR_IOFS:
            printf("IOFS ");
            dump_vreg(ir->dst);
            printf(", &%s", str_data(ir->label));
            printf("\n");
            break;
        case IR_STORE:
            printf("STORE [");
            dump_vreg(ir->src2);
            printf("], ");
            dump_vreg(ir->src1);
            printf("\n");
            break;
        case IR_LOAD:
            printf("LOAD ");
            dump_vreg(ir->dst);
            printf(", [");
            dump_vreg(ir->src1);
            printf("]\n");
            break;
        case IR_MOV:
            printf("MOV ");
            dump_vreg(ir->dst);
            printf(", ");
            dump_vreg(ir->src1);
            printf("\n");
            break;
        case IR_COND:
            printf("%s ", kcond[ir->cond & (COND_MASK | COND_UNSIGNED)]);
            dump_vreg(ir->dst);
            printf(", ");
            dump_vreg(ir->src1);
            printf(", ");
            dump_vreg(ir->src2);
            printf("\n");
            break;
        case IR_JMP:
        {
            printf("J%s ", kcond[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)]);
            if(ir->jmp.cond != COND_ANY && ir->jmp.cond != COND_NONE)
            {
                dump_vreg(ir->src1);
                printf(", ");
                dump_vreg(ir->src2);
                printf(", ");
            }
            printf("%.*s\n", ir->jmp.bb->label->len, str_data(ir->jmp.bb->label));
            break;
        }
        default:
            vp_assertX(0, "unknown ir %d", ir->kind);
            break;
    }
}

void vp_dump_bb(Decl* d)
{
    for(uint32_t i = 0; i < vec_len(d->fn.scopes); i++)
    {
        Scope* scope = d->fn.scopes[i];
        if(scope->vars == NULL)
            continue;
        for(uint32_t j = 0; j < vec_len(scope->vars); j++)
        {
            VarInfo* vi = scope->vars[j];
            VReg* vreg = vi->vreg;
            if(vreg == NULL)
                continue;
            printf("v%d (flag=%x): %.*s : ", vreg->virt, vreg->flag, vi->name->len, str_data(vi->name));
            vp_dump_type(vi->type);
            printf("\n");
        }
    }

    BB** bbs = d->fn.bbs;
    printf("BB: #%d\n", vec_len(bbs));
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        printf("%.*s:\n", bb->label->len, str_data(bb->label));
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            IR* ir = bb->irs[j];
            dump_ir(ir);
        }
    }
}

/* -- String interning dump ----------------------------------------- */

void vp_dump_strintern(void)
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

/* -- Type dump ----------------------------------------------------- */

void vp_dump_type(Type* ty)
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
            vp_dump_type(ty->p);
            printf("*");
            break;
        case TY_array:
            vp_dump_type(ty->p);
            printf("[");
            printf("%d", ty->len);
            printf("]");
            break;
        case TY_func:
            printf("fn(");
            for(uint32_t i = 0; i < vec_len(ty->fn.params); i++)
            {
                Type* pt = ty->fn.params[i];
                vp_dump_type(pt);
                if(i != vec_len(ty->fn.params) - 1)
                {
                    printf(", ");
                }
            }
            printf(") : ");
            vp_dump_type(ty->fn.ret);
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
                vp_dump_type(ty->st.fields[i].ty);
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

void vp_dump_typecache(void)
{
    printf("\n-- cache ptr --\n");
    for(uint32_t i = 0; i < V->cacheptr.size; i++)
    {
        if(V->cacheptr.keys[i])
        {
            vp_dump_type((Type*)(uintptr_t)V->cacheptr.vals[i]);
            printf("\n");
        }
    }
    printf("\n-- cache arr --\n");
    for(uint32_t i = 0; i < vec_len(V->cachearr); i++)
    {
        vp_dump_type(V->cachearr[i]);
        printf("\n");
    }
    printf("\n-- cache func --\n");
    for(uint32_t i = 0; i < vec_len(V->cachefunc); i++)
    {
        vp_dump_type(V->cachefunc[i]);
        printf("\n");
    }
}

/* -- AST dump ------------------------------------------------------ */

static void dump_ast_type(Type* ty)
{
    if(ty)
    {
        printf("(");
        vp_dump_type(ty);
        printf(") ");
    }
}

static void dump_ast_expr(Expr* e);

static void dump_typespec(TypeSpec* spec)
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
            dump_typespec(spec->ptr);
            printf("*");
            break;
        case SPEC_ARRAY:
            dump_typespec(spec->arr.base);
            printf("[");
            if(spec->arr.expr != NULL)
                dump_ast_expr(spec->arr.expr);
            printf("]");
            break;
        case SPEC_FUNC:
            printf("fn(");
            for(uint32_t i = 0; i < vec_len(spec->fn.args); i++)
            {
                TypeSpec* ts = spec->fn.args[i];
                dump_typespec(ts);
                if(i != vec_len(spec->fn.args) - 1)
                {
                    printf(", ");
                }
            }
            printf(") : ");
            dump_typespec(spec->fn.ret);
            break;
        case SPEC_TYPEOF:
            printf("typeof(");
            dump_ast_expr(spec->expr);
            printf(")");
            break;
        default:
            vp_assertX(0, "unknown typespec");
            break;
    }
}

static void dump_ast_aggr(Aggregate* agr)
{
    for(uint32_t i = 0; i < vec_len(agr->items); i++)
    {
        dump_indent();
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
        dump_typespec(item->type);
        printf("\n");
    }
}

static void dump_ast_expr(Expr* e)
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
        case EX_UINTT:
        {
            uint64_t u = e->uintt.u;
            printf("%llu", u);
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
            printf("%.14g", n);
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
                dump_typespec(e->comp.spec);
                printf(" ");
            }
            printf("{");
            for(Field* c = e->comp.fields; c != vec_end(e->comp.fields); c++)
            {
                if(c->kind == FIELD_IDX)
                {
                    printf("[");
                    dump_ast_expr(c->idx);
                    printf("] = ");
                }
                else if(c->kind == FIELD_NAME)
                {
                    printf("%s = ", str_data(c->name));
                }
                dump_ast_expr(c->init);
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
            dump_ast_expr(e->unary);
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
            dump_ast_expr(e->binop.lhs);
            printf(" %s ", ast_binname(e->kind));
            dump_ast_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_CAST:
        {
            printf("cast(");
            dump_typespec(e->cast.spec);
            printf(", ");
            dump_ast_expr(e->cast.expr);
            printf(")");
            break;
        }
        case EX_CALL:
        {
            dump_ast_expr(e->call.expr);
            printf("(");
            for(uint32_t i = 0; i < vec_len(e->call.args); i++)
            {
                Expr* arg = e->call.args[i];
                dump_ast_expr(arg);
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
            dump_typespec(e->spec);
            printf(")");
            break;
        }
        case EX_ALIGNOF:
        {
            printf("alignof(");
            dump_typespec(e->spec);
            printf(")");
            break;
        }
        case EX_OFFSETOF:
        {
            printf("offsetof(");
            dump_typespec(e->ofst.spec);
            printf(", ");
            printf("%s", str_data(e->ofst.name));
            printf(")");
            break;
        }
        case EX_FIELD:
        {
            dump_ast_expr(e->field.expr);
            printf(".%s", str_data(e->field.name));
            break;
        }
        default:
            vp_assertX(0, "unknown expression %d", e->kind);
            break;
    }
}

static void dump_ast_note(Note* note)
{
    uint32_t argsnum = vec_len(note->args);
    if(argsnum)
    {
        for(uint32_t i = 0; i < argsnum; i++)
        {
            dump_ast_expr(note->args[i].e);
            if(i != vec_len(note->args) - 1)
            {
                printf(", ");
            }
        }
    }
}

static void dump_ast_stmt(Stmt* stm)
{
    switch(stm->kind)
    {
        case ST_RETURN:
            dump_indent();
            printf("return ");
            dump_ast_expr(stm->expr);
            printf("\n");
            break;
        case ST_ASSIGN:
            dump_indent();
            dump_ast_type(stm->lhs->ty);
            dump_ast_expr(stm->lhs);
            printf(" = ");
            dump_ast_type(stm->rhs->ty);
            dump_ast_expr(stm->rhs);
            printf("\n");
            break;
        case ST_EXPR:
        {
            dump_indent();
            dump_ast_type(stm->expr->ty);
            dump_ast_expr(stm->expr);
            printf("\n");
            break;
        }
        case ST_BLOCK:
        {
            for(uint32_t i = 0; i < vec_len(stm->block); i++)
            {
                Stmt* st = stm->block[i];
                dump_ast_stmt(st);
            }
            break;
        }
        case ST_DECL:
        {
            vp_dump_ast(stm->decl);
            break;
        }
        default:
            vp_assertX(0, "unknown statement");
            break;
    }
}

void vp_dump_ast(Decl* d)
{
    switch(d->kind)
    {
        case DECL_TYPE:
            printf("type %s = ", str_data(d->name));
            dump_typespec(d->ts.spec);
            printf("\n");
            break;
        case DECL_VAR:
        {
            dump_indent();
            printf("var %s", str_data(d->name));
            if(d->var.spec)
            {
                printf(" : ");
                dump_typespec(d->var.spec);
            }
            if(d->var.expr)
            {
                printf(" = ");
                dump_ast_type(d->var.expr->ty);
                dump_ast_expr(d->var.expr);
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
                dump_typespec(param->spec);
                if(i != vec_len(d->fn.params) - 1)
                {
                    printf(", ");
                }
            }
            printf(")");
            if(d->fn.body)
            {
                printf("\n{\n");
                indent++;
                dump_ast_stmt(d->fn.body);
                indent--;
                printf("}\n");
            }
            break;
        case DECL_STRUCT:
            printf("struct %s\n{\n", str_data(d->name));
            indent++;
            dump_ast_aggr(d->agr);
            indent--;
            printf("}\n");
            break;
        case DECL_NOTE:
            dump_indent();
            printf("#%s", str_data(d->note.name));
            dump_ast_note(&d->note);
            printf("\n");
            break;
        default:
            vp_assertX(0, "unknown decl");
            break;
    }
}