/*
** vp_dump.c
** Data structure print
*/

#include <stdio.h>

#include "vp_dump.h"
#include "vp_ast.h"
#include "vp_ir.h"
#include "vp_regalloc.h"
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

#define STR_DUMP_MAX 40

/* Dump char (escaped) */
static void dump_char(int c)
{
    switch(c)
    {
        case '\n': printf("\\n"); break;
        case '\t': printf("\\t"); break;
        case '\r': printf("\\r"); break;
        case '\v': printf("\\v"); break;
        case '\f': printf("\\f"); break;
        case '\a': printf("\\a"); break;
        case '\b': printf("\\b"); break;
        case '\\': printf("\\\\"); break;
        case '\'': printf("\\'"); break;
        case '\0': printf("\\0"); break;
        default:
            if(c >= 32 && c <= 126)
            {
                printf("%c", c);
            }
            else
            {
                printf("\\x%02X", c & 0xff);
            }
            break;
    }
}

/* Dump string (escaped) */
static void dump_str(Str* s)
{
    printf("\"");
    const char* data = str_data(s);
    uint32_t len = s->len;

    uint32_t limit = len;
    bool trunc = false;

    if(len > STR_DUMP_MAX)
    {
        limit = STR_DUMP_MAX;
        trunc = true;
    }

    for(uint32_t i = 0; i < limit; i++)
    {
        dump_char(data[i]);
    }

    if(trunc)
    {
        printf(" ...");
    }
    printf("\"");
}

/* -- IR dump ------------------------------------------------------- */

static const char* const vrf_flags[] = {
#define VRFNAME(name, bit) [bit] = "VRF_" #name,
    VRFDEF(VRFNAME)
#undef VRFNAME
};

/* Dump a RegSet as a list of registers */
static void dump_regbits(uint64_t regbits, char rt)
{
    if(regbits == 0)
    {
        printf("none");
        return;
    }

    bool first = true;
    for(uint32_t i = 0; i < 64; i++)
    {
        if(regbits & (1ULL << i))
        {
            printf("%s%c%d", first ? "" : ",", rt, i);
            first = false;
        }
    }
}

/* Dump vreg bit flags */
static void dump_vreg_flags(uint8_t flag)
{
    if(flag == 0)
    {
        printf("0");
        return;
    }

    bool first = true;
    for(uint32_t i = 0; i < 8; i++)
    {
        if(flag & (1 << i))
        {
            printf("%s%s", first ? "" : " | ", vrf_flags[i]);
            first = false;
        }
    }
}

static void dump_vreg(VReg* vr)
{
    vp_assertX(vr, "empty vreg");
    vp_assertX(!vrf_spill(vr), "spilled vreg");
    if(vrf_const(vr))
    {
        if(vrf_flo(vr))
        {
            if(vr->vsize == VRSize4)
            {
                printf("%f", vr->n);
            }
            else
            {
                printf("%.14g", vr->n);
            }
        }
        else
        {
            printf("%lli", vr->i64);
        }
    }
    else if(vr->phys != REG_NO)
    {
        char rt = 'r';
        if(vrf_flo(vr))
        {
            rt = 'f';
        }
        printf("%c%d<v%d>", rt, vr->phys, vr->virt);
    }
    else
    {
        printf("v%d", vr->virt);
    }
}

static void dump_vregs(const char* title, vec_t(VReg*) vregs)
{
    printf("%s=[", title);
    for(uint32_t i = 0; i < vec_len(vregs); i++)
    {
        VReg* vr = vregs[i];
        printf("%s%d", i == 0 ? "" : ", ", vr->virt);
    }
    printf("]");
}

static const char* const kcond[] = {
    NULL, "MP", "EQ", "NEQ", "LT", "LE", "GT", "GE",
    NULL, "MP", "EQ", "NEQ", "LT", "LE", "GT", "GE"
};

static const char* const kcond2[] = {
    NULL, NULL, "==", "!=", "<", "<=", ">", ">=",
    NULL, NULL, "==", "!=", "<", "<=", ">", ">="
};

static const char* const kbinop2[] = {
    "+", "-", "*", "/", "%",
    "&", "|", "^", "<<", ">>"
};

static void dump_ir(IR* ir)
{
    switch(ir->kind)
    {
        case IR_JMP:
            printf("J%s", kcond[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)]);
            break;
        default:
            printf("%s", vp_ir_name[ir->kind]);
            break;
    }

    if(irf_unsigned(ir)) printf("U");
    printf("\t");

    switch(ir->kind)
    {
        case IR_BOFS:
        {
            int64_t ofs = ir->bofs.fi->ofs;
            dump_vreg(ir->dst);
            printf(" = &[rbp %c %lli]", ofs >= 0 ? '+' : '-', ofs > 0 ? ofs : -ofs);
            break;
        }
        case IR_IOFS:
        {
            Str* label = ir->iofs.label;
            dump_vreg(ir->dst);
            printf(" = &");
            if(ir->iofs.isstr)
            {
                dump_str(label);
            }
            else
            {
                printf("\"%.*s\"", label->len, str_data(label));
            }
            break;
        }
        case IR_SOFS:
        {
            int64_t ofs = ir->sofs.ofs;
            dump_vreg(ir->dst);
            printf(" = &[rsp %c %lli]", ofs >= 0 ? '+' : '-', ofs > 0 ? ofs : -ofs);
            break;
        }
        case IR_MOV:
            dump_vreg(ir->dst);
            printf(" = ");
            dump_vreg(ir->src1);
            break;
        case IR_STORE:
            printf("[");
            dump_vreg(ir->src2);
            printf("] = ");
            dump_vreg(ir->src1);
            break;
        case IR_STORE_S:
            printf("[v%d] = ", ir->src2->virt);
            dump_vreg(ir->src1);
            break;
        case IR_LOAD:
            dump_vreg(ir->dst);
            printf(" = [");
            dump_vreg(ir->src1);
            printf("]");
            break;
        case IR_LOAD_S:
            dump_vreg(ir->dst);
            printf(" = [v%d]", ir->src1->virt);
            break;
        case IR_RET:
        {
            if(ir->src1)
            {
                dump_vreg(ir->src1);
            }
            break;
        }
        case IR_COND:
            dump_vreg(ir->dst);
            printf(" = ");
            dump_vreg(ir->src1);
            printf(" %s ", kcond2[ir->cond & (COND_MASK | COND_UNSIGNED)]);
            dump_vreg(ir->src2);
            break;
        case IR_JMP:
        {
            if(ir->jmp.cond != COND_ANY && ir->jmp.cond != COND_NONE)
            {
                printf("if ");
                dump_vreg(ir->src1);
                printf(" %s ", kcond2[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)]);
                dump_vreg(ir->src2);
                printf(" goto %.*s", ir->jmp.bb->label->len, str_data(ir->jmp.bb->label));
            }
            else
            {
                printf("goto %.*s",
                    ir->jmp.bb->label->len, str_data(ir->jmp.bb->label));
            }
            break;
        }
        case IR_PUSHARG:
        {
            printf("%d, ", ir->arg.idx);
            dump_vreg(ir->src1);
            break;
        }
        case IR_KEEP:
        {
            if(ir->dst)
            {
                dump_vreg(ir->dst);
                printf(", ");
            }
            if(ir->src1)
            {
                dump_vreg(ir->src1);
                if(ir->src2)
                {
                    printf(", ");
                    dump_vreg(ir->src2);
                }
            }
            break;
        }
        case IR_ASM: break;
        case IR_CALL:
        {
            if(ir->dst)
            {
                dump_vreg(ir->dst);
                printf(" = ");
            }
            if(ir->call->label)
            {
                printf("%.*s", ir->call->label->len, str_data(ir->call->label));
            }
            else
            {
                printf("*");
                dump_vreg(ir->src1);
            }
            uint32_t start = ir->call->argtotal != ir->call->argnum;
            printf("(");
            for(uint32_t i = start; i < ir->call->argtotal; i++)
            {
                dump_vreg(ir->call->args[i]);
                if(i != ir->call->argtotal - 1)
                {
                    printf(", ");
                }
            }
            printf(")");
            printf(" args=%d", ir->call->argnum);
            break;
        }
        case IR_CAST:
        {
            dump_vreg(ir->dst);
            printf(" = ");
            dump_vreg(ir->src1);
            break;
        }
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
            dump_vreg(ir->dst);
            printf(" = ");
            dump_vreg(ir->src1);
            printf(" %s ", kbinop2[ir->kind - IR_ADD]);
            dump_vreg(ir->src2);
            break;
        case IR_NEG:
            dump_vreg(ir->dst);
            printf(" = -");
            dump_vreg(ir->src1);
            break;
        case IR_BNOT:
            dump_vreg(ir->dst);
            printf(" = ~");
            dump_vreg(ir->src1);
            break;
        default:
            vp_assertX(0, "unknown ir %d", ir->kind);
            break;
    }
}

void vp_dump_bbs(Code* code)
{
    printf("fn %s\n", str_data(code->name));
    printf("params and locals:\n");
    vec_t(VarInfo*) stackvars = NULL;
    for(uint32_t i = 0; i < vec_len(code->scopes); i++)
    {
        Scope* scope = code->scopes[i];
        if(scope->vars == NULL)
            continue;
        for(uint32_t j = 0; j < vec_len(scope->vars); j++)
        {
            VarInfo* vi = scope->vars[j];
            VReg* vr = vi->vreg;
            if(vr == NULL)
            {
                vec_push(stackvars, vi);
                continue;
            }
            printf("v%d (flag=", vr->virt);
            dump_vreg_flags(vr->flag);
            printf(") %.*s : ", vi->name->len, str_data(vi->name));
            vp_dump_type(vi->type);
            printf("\n");
        }
    }
    for(uint32_t i = 0; i < vec_len(stackvars); i++)
    {
        VarInfo* vi = stackvars[i];
        printf("stack (offset=%d, size=%d) %.*s : ", vi->fi->ofs, vp_type_sizeof(vi->type), vi->name->len, str_data(vi->name));
        vp_dump_type(vi->type);
        printf("\n");
    }

    RegAlloc* ra = code->ra;
    printf("VREG: #%d\n", vec_len(ra->vregs));
    LiveInterval** sorted = ra->sorted;
    if(sorted)
    {
        for(uint32_t i = 0; i < vec_len(ra->vregs); i++)
        {
            LiveInterval* li = sorted[i];
            VReg* vr = ra->vregs[li->virt];
            if(vr == NULL)
                continue;

            printf("    v%d (flag=", vr->virt);
            dump_vreg_flags(vr->flag);
            printf("):  live %d - %d", li->start, li->end);
            switch(li->state)
            {
                case LI_NORMAL:
                {
                    char rt = vrf_flo(vr) ? 'f' : 'r';
                    printf(" => %c%d", rt, li->phys);
                    if(li->regbits)
                    {
                        printf(", occupied=");
                        dump_regbits(li->regbits, vrf_flo(vr) ? 'f' : 'r');
                    }
                    break;
                }
                case LI_SPILL:
                {
                    printf(" (spilled, offset=%d)", vr->fi.ofs);
                    break;
                }
            }
            printf("\n");
        }
    }

    uint32_t nip = 0;
    vec_t(BB*) bbs = code->bbs;
    printf("BB: #%d\n", vec_len(bbs));
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        printf("%.*s:", bb->label->len, str_data(bb->label));
        if(vec_len(bb->frombbs) > 0)
        {
            printf(" from=[");
            for(uint32_t j = 0; j < vec_len(bb->frombbs); j++)
            {
                BB* fbb = bb->frombbs[j];
                printf("%s%.*s", (j > 0 ? ", " : ""), fbb->label->len, str_data(fbb->label));
            }
            printf(" ]");
        }
        if(vec_len(bb->inregs) > 0)
        {
            dump_vregs(" in", bb->inregs);
        }
        if(vec_len(bb->outregs) > 0)
        {
            dump_vregs(" out", bb->outregs);
        }
        printf("\n");
        for(uint32_t j = 0; j < vec_len(bb->irs); j++, nip++)
        {
            printf("%6d|\t", nip);
            IR* ir = bb->irs[j];
            dump_ir(ir);
            printf("\n");
        }
        if(i != vec_len(bbs) - 1)
        {
            printf("\n");
        }
    }
}

static void dump_inst(void* p, uint32_t size)
{
    /* Windows temp file */
    char tmpname[L_tmpnam];
    tmpnam(tmpname);
    FILE* tmp = fopen(tmpname, "wb");
    if(!tmp)
    {
        printf("  [failed to create temp file]\n");
    }

    fwrite(p, 1, size, tmp);
    fclose(tmp);

    char cmd[512];
    snprintf(cmd, sizeof(cmd),
                "objdump -D -b binary -m i386:x86-64 -M intel \"%s\" 2>nul",
                tmpname);

    FILE* pipe = _popen(cmd, "r");
    if(pipe)
    {
        char line[512];
        int skip = 7;
        while(fgets(line, sizeof(line), pipe))
        {
            if(skip-- > 0) continue;
            printf("  %s", line);
        }
        _pclose(pipe);
    }

    remove(tmpname);
}

void vp_dump_code(vec_t(Code*) codes)
{
    vp_assertX(sbuf_size(&V->code), "empty code");
    uint8_t* base = (uint8_t*)V->code.b;
    for(uint32_t i = 0; i < vec_len(codes); i++)
    {
        Code* code = codes[i];
        vec_t(BB*) bbs = code->bbs;
        if(vec_len(bbs) > 0)
        {
            uint32_t start = code->ofs;
            uint32_t end = bbs[0]->ofs;
            uint32_t size = end - start;

            if(size > 0)
            {
                printf("\n%s: (offset=%d, size=%d)\n",
                        str_data(code->name), start, size);
                dump_inst(base + start, size);
            }
        }
        for(uint32_t j = 0; j < vec_len(bbs); j++)
        {
            BB* bb = bbs[j];
            uint32_t start = bb->ofs;
            uint32_t end = (j + 1 < vec_len(bbs)) ? bbs[j + 1]->ofs : ((i + 1 < vec_len(codes)) ? codes[i + 1]->ofs : sbuf_len(&V->code));
            uint32_t size = end - start;
            if(size == 0) continue;

            printf("\n%.*s: (offset=%d, size=%d)\n",
                           bb->label->len, str_data(bb->label), start, size);

            dump_inst(base + start, size);
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
        case TY_none:
            break;
        case TY_bool:
        case TY_uint8:
        case TY_int8:
        case TY_uint16:
        case TY_int16:
        case TY_uint32:
        case TY_int32:
        case TY_uint64:
        case TY_int64:
        case TY_isize:
        case TY_usize:
        case TY_float32:
        case TY_float64:
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

static void dump_ast_attr(Attr* attr)
{
    printf("[[");
    printf("%.*s", attr->name->len, str_data(attr->name));
    uint32_t argsnum = vec_len(attr->args);
    if(argsnum)
    {
        printf("(");
        for(uint32_t i = 0; i < argsnum; i++)
        {
            dump_ast_expr(attr->args[i].e);
            if(i != argsnum - 1)
            {
                printf(", ");
            }
        }
        printf(")");
    }
    printf("]]\n");
}

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
        case SPEC_CONST:
            printf("const ");
            dump_typespec(spec->ptr);
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

static const char* const exprcast[] = {"cast", "intcast", "floatcast", "ptrcast", "bitcast"};

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
            int c = e->i;
            printf("'");
            dump_char(c);
            printf("'");
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
            dump_str(e->name);
            break;
        }
        case EX_NAME:
        {
            printf("%.*s", e->name->len, str_data(e->name));
            break;
        }
        case EX_COMPLIT:
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
        case EX_INTCAST:
        case EX_FLOATCAST:
        case EX_PTRCAST:
        case EX_BITCAST:
        {
            printf("%s(", exprcast[e->kind - EX_CAST]);
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
        case EX_ACCESS:
        {
            dump_ast_expr(e->access.expr);
            printf("::%s", str_data(e->access.name));
            break;
        }
        case EX_IDX:
        {
            dump_ast_expr(e->idx.expr);
            printf("[");
            dump_ast_expr(e->idx.index);
            printf("]");
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

static void dump_ast_stmt(Stmt* st)
{
    switch(st->kind)
    {
        case ST_IF:
            dump_indent();
            printf("if ");
            dump_ast_expr(st->ifst.cond);
            printf("\n");
            dump_indent();
            printf("{\n");
            indent++;
            dump_ast_stmt(st->ifst.tblock);
            indent--;
            dump_indent();
            printf("}\n");
            /* Else block, if any */
            if(st->ifst.fblock)
            {
                dump_indent();
                printf("else\n");
                dump_indent();
                printf("{\n");
                indent++;
                dump_ast_stmt(st->ifst.fblock);
                indent--;
                dump_indent();
                printf("}\n");
            }
            break;
        case ST_WHILE:
            dump_indent();
            printf("while ");
            dump_ast_expr(st->whst.cond);
            printf("\n");
            dump_indent();
            printf("{\n");
            indent++;
            dump_ast_stmt(st->whst.body);
            indent--;
            dump_indent();
            printf("}\n");
            break;
        case ST_RETURN:
            dump_indent();
            printf("return ");
            dump_ast_expr(st->expr);
            printf("\n");
            break;
        case ST_BREAK:
            dump_indent();
            printf("break ");
            printf("\n");
            break;
        case ST_ASM:
            dump_indent();
            printf("asm ...");
            printf("\n");
            break;
        case ST_CONTINUE:
            dump_indent();
            printf("continue ");
            printf("\n");
            break;
        case ST_ASSIGN:
        case ST_ADD_ASSIGN:
        case ST_SUB_ASSIGN:
        case ST_MUL_ASSIGN:
        case ST_DIV_ASSIGN:
        case ST_MOD_ASSIGN:
        case ST_BAND_ASSIGN:
        case ST_BOR_ASSIGN:
        case ST_BXOR_ASSIGN:
        case ST_LSHIFT_ASSIGN:
        case ST_RSHIFT_ASSIGN:
            dump_indent();
            dump_ast_type(st->lhs->ty);
            dump_ast_expr(st->lhs);
            printf(" %s ", ast_assignname(st->kind));
            dump_ast_type(st->rhs->ty);
            dump_ast_expr(st->rhs);
            printf("\n");
            break;
        case ST_EXPR:
        {
            dump_indent();
            dump_ast_type(st->expr->ty);
            dump_ast_expr(st->expr);
            printf("\n");
            break;
        }
        case ST_BLOCK:
        {
            for(uint32_t i = 0; i < vec_len(st->block); i++)
            {
                Stmt* stmt = st->block[i];
                dump_ast_stmt(stmt);
            }
            break;
        }
        case ST_DECL:
        {
            vp_dump_decl(st->decl);
            break;
        }
        default:
            vp_assertX(0, "unknown statement");
            break;
    }
}

void vp_dump_decl(Decl* d)
{
    if(d->flags & DECL_FLAG_PUB)
    {
        printf("pub ");
    }
    switch(d->kind)
    {
        case DECL_IMPORT:
        {
            Str* path = d->name;
            if(d->imp.items)
            {
                /* from "file" import foo, bar as b */
                printf("from ");
                dump_str(path);
                printf(" import ");
                for(uint32_t i = 0; i < vec_len(d->imp.items); i++)
                {
                    ImportItem* item = &d->imp.items[i];
                    printf("%s", str_data(item->name));
                    if(item->alias)
                    {
                        printf(" as %s", str_data(item->alias));
                    }
                    if(i != vec_len(d->imp.items) - 1)
                    {
                        printf(", ");
                    }
                }
            }
            else if(d->imp.wildcard)
            {
                /* from "file" import * */
                printf("from ");
                dump_str(path);
                printf(" import *");
            }
            else if(d->imp.alias)
            {
                /* import "file" as mod */
                printf("import ");
                dump_str(path);
                printf(" as %s", str_data(d->imp.alias));
            }
            else
            {
                /* import "file" */
                printf("import ");
                dump_str(path);
            }
            printf("\n");
            break;
        }
        case DECL_TYPE:
            printf("type %s = ", str_data(d->name));
            dump_typespec(d->ts.spec);
            printf("\n");
            break;
        case DECL_ALIAS:
            printf("alias %s = ", str_data(d->name));
            dump_typespec(d->ts.spec);
            printf("\n");
            break;
        case DECL_DEF:
        {
            dump_indent();
            printf("def %s", str_data(d->name));
            if(d->def.spec)
            {
                printf(" : ");
                dump_typespec(d->def.spec);
            }
            printf(" = ");
            dump_ast_type(d->def.expr->ty);
            dump_ast_expr(d->def.expr);
            printf("\n");
            break;
        }
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
        {
            uint32_t attrsnum = vec_len(d->fn.attrs);
            if(attrsnum)
            {
                for (uint32_t i = 0; i < vec_len(d->fn.attrs); i++)
                {
                    dump_ast_attr(&d->fn.attrs[i]);
                }
            }
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
            if(d->fn.ret)
            {
                printf(" : ");
                dump_typespec(d->fn.ret);
            }
            if(d->fn.body)
            {
                printf("\n{\n");
                indent++;
                dump_ast_stmt(d->fn.body);
                indent--;
                printf("}");
            }
            printf("\n");
            break;
        }
        case DECL_ENUM:
            printf("enum %s", str_data(d->name));
            if (d->enm.spec)
            {
                printf(" : ");
                dump_typespec(d->enm.spec);
            }
            printf("\n{\n");
            indent++;
            for (uint32_t i = 0; i < vec_len(d->enm.items); i++)
            {
                dump_indent();
                EnumItem* item = &d->enm.items[i];
                printf("%s", str_data(item->name));
                if (item->init)
                {
                    printf(" = ");
                    dump_ast_expr(item->init);
                }
                if (i != vec_len(d->enm.items) - 1)
                {
                    printf(",");
                }
                printf("\n");
            }
            indent--;
            printf("}\n");
            break;
        case DECL_UNION:
        {
            printf("union %s\n{\n", str_data(d->name));
            indent++;
            dump_ast_aggr(d->agr);
            indent--;
            printf("}\n");
            break;
        }
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