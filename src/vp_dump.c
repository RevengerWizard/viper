/*
** vp_dump.c
** Data structure print
*/

#include <stdio.h>

#include "vp_dump.h"
#include "vp_ast.h"
#include "vp_buf.h"
#include "vp_def.h"
#include "vp_ir.h"
#include "vp_regalloc.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_vec.h"
#include "vp_tab.h"

static int indent = 0;

static void dump_fmt(SBuf* sb, const char* fmt, ...)
{
    char buf[1024];
    va_list args;
    va_start(args, fmt);
    int n = vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    if(n > 0 && n < (int)sizeof(buf))
    {
        vp_buf_putmem(sb, buf, n);
    }
}

static void dump_type(SBuf* sb, Type* ty);

static void dump_indent(SBuf* sb)
{
    for(uint32_t i = 0; i < (4*indent); i++)
    {
        vp_buf_putb(sb, ' ');
    }
}

#define STR_DUMP_MAX 40

/* Dump char (escaped) */
static void dump_char(SBuf* sb, int c)
{
    switch(c)
    {
    case '\n': vp_buf_putlit(sb, "\\n"); break;
    case '\t': vp_buf_putlit(sb, "\\t"); break;
    case '\r': vp_buf_putlit(sb, "\\r"); break;
    case '\v': vp_buf_putlit(sb, "\\v"); break;
    case '\f': vp_buf_putlit(sb, "\\f"); break;
    case '\a': vp_buf_putlit(sb, "\\a"); break;
    case '\b': vp_buf_putlit(sb, "\\b"); break;
    case '\\': vp_buf_putlit(sb, "\\\\"); break;
    case '\'': vp_buf_putlit(sb, "\\'"); break;
    case '\0': vp_buf_putlit(sb, "\\0"); break;
    default:
        if(c >= 32 && c <= 126)
        {
            vp_buf_putb(sb, c);
        }
        else
        {
            dump_fmt(sb, "\\x%02X", c & 0xff);
        }
        break;
    }
}

/* Dump string (escaped) */
static void dump_str(SBuf* sb, Str* s)
{
    vp_buf_putb(sb, '"');
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
        dump_char(sb, data[i]);
    }

    if(trunc)
    {
        vp_buf_putlit(sb, " ...");
    }
    vp_buf_putb(sb, '"');
}

/* -- IR dump ------------------------------------------------------- */

static const char* const vrf_flags[] = {
#define VRFNAME(name, bit) [bit] = "VRF_" #name,
    VRFDEF(VRFNAME)
#undef VRFNAME
};

/* Dump a RegSet as a list of registers */
static void dump_regbits(SBuf* sb, uint64_t regbits, char rt)
{
    if(regbits == 0)
    {
        vp_buf_putlit(sb, "none");
        return;
    }

    bool first = true;
    for(uint32_t i = 0; i < 64; i++)
    {
        if(regbits & (1ULL << i))
        {
            dump_fmt(sb, "%s%c%d", first ? "" : ",", rt, i);
            first = false;
        }
    }
}

/* Dump vreg bit flags */
static void dump_vreg_flags(SBuf* sb, uint8_t flag)
{
    if(flag == 0)
    {
        vp_buf_putb(sb, '0');
        return;
    }

    bool first = true;
    for(uint32_t i = 0; i < 8; i++)
    {
        if(flag & (1 << i))
        {
            dump_fmt(sb, "%s%s", first ? "" : " | ", vrf_flags[i]);
            first = false;
        }
    }
}

static void dump_vreg(SBuf* sb, VReg* vr)
{
    vp_assertX(vr, "empty vreg");
    if(vrf_const(vr))
    {
        if(vrf_flo(vr))
        {
            if(vr->vsize == VRSize4)
            {
                dump_fmt(sb, "%f", vr->n);
            }
            else
            {
                dump_fmt(sb, "%.14g", vr->n);
            }
        }
        else
        {
            dump_fmt(sb, "%lli", vr->i64);
        }
    }
    else if(vr->phys != REG_NO)
    {
        vp_assertX(!vrf_spill(vr), "spilled vreg");
        char rt = 'r';
        if(vrf_flo(vr))
        {
            rt = 'f';
        }
        dump_fmt(sb, "%c%d<v%d>", rt, vr->phys, vr->virt);
    }
    else
    {
        dump_fmt(sb, "v%d", vr->virt);
    }
}

static void dump_vregs(SBuf* sb, const char* title, vec_t(VReg*) vregs)
{
    dump_fmt(sb, "%s=[", title);
    for(uint32_t i = 0; i < vec_len(vregs); i++)
    {
        VReg* vr = vregs[i];
        dump_fmt(sb, "%s%d", i == 0 ? "" : ", ", vr->virt);
    }
    vp_buf_putb(sb, ']');
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

static void dump_ir(SBuf* sb, IR* ir)
{
    switch(ir->kind)
    {
        case IR_JMP:
            dump_fmt(sb, "J%s", kcond[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)]);
            break;
        default:
            dump_fmt(sb, "%s", vp_ir_name[ir->kind]);
            break;
    }

    if(irf_unsigned(ir)) vp_buf_putb(sb, 'U');
    vp_buf_putb(sb, '\t');

    switch(ir->kind)
    {
        case IR_BOFS:
        {
            int64_t ofs = ir->bofs.fi->ofs;
            dump_vreg(sb, ir->dst);
            dump_fmt(sb, " = &[rbp %c %lli]", ofs >= 0 ? '+' : '-', ofs > 0 ? ofs : -ofs);
            break;
        }
        case IR_IOFS:
        {
            Str* label = ir->iofs.label;
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = &");
            if(ir->iofs.isstr)
            {
                dump_str(sb, label);
            }
            else
            {
                vp_buf_putstr(sb, label);
            }
            break;
        }
        case IR_SOFS:
        {
            int64_t ofs = ir->sofs.ofs;
            dump_vreg(sb, ir->dst);
            dump_fmt(sb, " = &[rsp %c %lli]", ofs >= 0 ? '+' : '-', ofs > 0 ? ofs : -ofs);
            break;
        }
        case IR_MOV:
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = ");
            dump_vreg(sb, ir->src1);
            break;
        case IR_STORE:
            vp_buf_putb(sb, '[');
            dump_vreg(sb, ir->src2);
            vp_buf_putlit(sb, "] = ");
            dump_vreg(sb, ir->src1);
            break;
        case IR_STORE_S:
            dump_fmt(sb, "[v%d] = ", ir->src2->virt);
            dump_vreg(sb, ir->src1);
            break;
        case IR_LOAD:
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = [");
            dump_vreg(sb, ir->src1);
            vp_buf_putb(sb, ']');
            break;
        case IR_LOAD_S:
            dump_vreg(sb, ir->dst);
            dump_fmt(sb, " = [v%d]", ir->src1->virt);
            break;
        case IR_RET:
        {
            if(ir->src1)
            {
                dump_vreg(sb, ir->src1);
            }
            break;
        }
        case IR_COND:
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = ");
            dump_vreg(sb, ir->src1);
            dump_fmt(sb, " %s ", kcond2[ir->cond & (COND_MASK | COND_UNSIGNED)]);
            dump_vreg(sb, ir->src2);
            break;
        case IR_JMP:
        {
            if(ir->jmp.cond != COND_ANY && ir->jmp.cond != COND_NONE)
            {
                vp_buf_putlit(sb, "if ");
                dump_vreg(sb, ir->src1);
                dump_fmt(sb, " %s ", kcond2[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)]);
                dump_vreg(sb, ir->src2);
                dump_fmt(sb, " goto %.*s", ir->jmp.bb->label->len, str_data(ir->jmp.bb->label));
            }
            else
            {
                dump_fmt(sb, "goto %.*s",
                    ir->jmp.bb->label->len, str_data(ir->jmp.bb->label));
            }
            break;
        }
        case IR_PUSHARG:
        {
            dump_fmt(sb, "r%d, ", ir->arg.idx);
            dump_vreg(sb, ir->src1);
            break;
        }
        case IR_KEEP:
        {
            if(ir->dst)
            {
                dump_vreg(sb, ir->dst);
                vp_buf_putlit(sb, ", ");
            }
            if(ir->src1)
            {
                dump_vreg(sb, ir->src1);
                if(ir->src2)
                {
                    vp_buf_putlit(sb, ", ");
                    dump_vreg(sb, ir->src2);
                }
            }
            break;
        }
        case IR_ASM: break;
        case IR_CALL:
        {
            if(ir->dst)
            {
                dump_vreg(sb, ir->dst);
                vp_buf_putlit(sb, " = ");
            }
            if(ir->call->label)
            {
                vp_buf_putstr(sb, ir->call->label);
            }
            else
            {
                vp_buf_putb(sb, '*');
                dump_vreg(sb, ir->src1);
            }
            uint32_t start = ir->call->argtotal != ir->call->argnum;
            vp_buf_putb(sb, '(');
            for(uint32_t i = start; i < ir->call->argtotal; i++)
            {
                dump_vreg(sb, ir->call->args[i]);
                if(i != ir->call->argtotal - 1)
                {
                    vp_buf_putlit(sb, ", ");
                }
            }
            vp_buf_putb(sb, ')');
            dump_fmt(sb, " args=%d", ir->call->argnum);
            break;
        }
        case IR_CAST:
        {
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = ");
            dump_vreg(sb, ir->src1);
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
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = ");
            dump_vreg(sb, ir->src1);
            dump_fmt(sb, " %s ", kbinop2[ir->kind - IR_ADD]);
            dump_vreg(sb, ir->src2);
            break;
        case IR_NEG:
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = -");
            dump_vreg(sb, ir->src1);
            break;
        case IR_BNOT:
            dump_vreg(sb, ir->dst);
            vp_buf_putlit(sb, " = ~");
            dump_vreg(sb, ir->src1);
            break;
        default:
            vp_assertX(0, "unknown ir %d", ir->kind);
            break;
    }
}

static void dump_bbs(SBuf* sb, Code* code)
{
    dump_fmt(sb, "fn %s\n", str_data(code->name));
    vp_buf_putlit(sb, "params and locals:\n");
    vec_t(VarInfo*) stackvars = vec_init(VarInfo*);
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
            dump_fmt(sb, "v%d (flag=", vr->virt);
            dump_vreg_flags(sb, vr->flag);
            dump_fmt(sb, ") %.*s : ", vi->name->len, str_data(vi->name));
            dump_type(sb, vi->type);
            vp_buf_putb(sb, '\n');
        }
    }
    for(uint32_t i = 0; i < vec_len(stackvars); i++)
    {
        VarInfo* vi = stackvars[i];
        dump_fmt(sb, "stack (offset=%d, size=%d) %.*s : ", vi->fi->ofs, vp_type_sizeof(vi->type), vi->name->len, str_data(vi->name));
        dump_type(sb, vi->type);
        vp_buf_putb(sb, '\n');
    }

    RegAlloc* ra = code->ra;
    dump_fmt(sb, "VREG: #%d\n", vec_len(ra->vregs));
    LiveInterval** sorted = ra->sorted;
    if(sorted)
    {
        for(uint32_t i = 0; i < vec_len(ra->vregs); i++)
        {
            LiveInterval* li = sorted[i];
            VReg* vr = ra->vregs[li->virt];
            if(vr == NULL)
                continue;

            dump_fmt(sb, "    v%d (flag=", vr->virt);
            dump_vreg_flags(sb, vr->flag);
            dump_fmt(sb, "):  live %d - %d", li->start, li->end);
            switch(li->state)
            {
                case LI_NORMAL:
                {
                    char rt = vrf_flo(vr) ? 'f' : 'r';
                    dump_fmt(sb, " => %c%d", rt, li->phys);
                    if(li->regbits)
                    {
                        vp_buf_putlit(sb, ", occupied=");
                        dump_regbits(sb, li->regbits, vrf_flo(vr) ? 'f' : 'r');
                    }
                    break;
                }
                case LI_SPILL:
                {
                    dump_fmt(sb, " (spilled, offset=%d)", vr->fi.ofs);
                    break;
                }
            }
            vp_buf_putb(sb, '\n');
        }
    }

    uint32_t nip = 0;
    vec_t(BB*) bbs = code->bbs;
    dump_fmt(sb, "BB: #%d\n", vec_len(bbs));
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        vp_buf_putstr(sb, bb->label);
        if(vec_len(bb->frombbs) > 0)
        {
            vp_buf_putlit(sb, " from=[");
            for(uint32_t j = 0; j < vec_len(bb->frombbs); j++)
            {
                BB* fbb = bb->frombbs[j];
                dump_fmt(sb, "%s", (j > 0 ? ", " : ""));
                vp_buf_putstr(sb, fbb->label);
            }
            vp_buf_putb(sb, ']');
        }
        if(vec_len(bb->inregs) > 0)
        {
            dump_vregs(sb, " in", bb->inregs);
        }
        if(vec_len(bb->outregs) > 0)
        {
            dump_vregs(sb, " out", bb->outregs);
        }
        vp_buf_putb(sb, '\n');
        for(uint32_t j = 0; j < vec_len(bb->irs); j++, nip++)
        {
            dump_fmt(sb, "%6d|\t", nip);
            IR* ir = bb->irs[j];
            dump_ir(sb, ir);
            vp_buf_putb(sb, '\n');
        }
        if(i != vec_len(bbs) - 1)
        {
            vp_buf_putb(sb, '\n');
        }
    }
}

void vp_dump_bbs(Code* code)
{
    SBuf sb;
    vp_buf_init(&sb);
    dump_bbs(&sb, code);
    fwrite(sb.b, 1, sbuf_len(&sb), stdout);
    fflush(stdout);
}

static void dump_inst(SBuf* sb, void* p, uint32_t size)
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
            dump_fmt(sb, "  %s", line);
        }
        _pclose(pipe);
    }

    remove(tmpname);
}

static void dump_codes(SBuf* sb, vec_t(Code*) codes)
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
                dump_fmt(sb, "\n%s: (offset=%d, size=%d)\n",
                        str_data(code->name), start, size);
                dump_inst(sb, base + start, size);
            }
        }
        for(uint32_t j = 0; j < vec_len(bbs); j++)
        {
            BB* bb = bbs[j];
            uint32_t start = bb->ofs;
            uint32_t end = (j + 1 < vec_len(bbs)) ? bbs[j + 1]->ofs : ((i + 1 < vec_len(codes)) ? codes[i + 1]->ofs : sbuf_len(&V->code));
            uint32_t size = end - start;
            if(size == 0) continue;

            dump_fmt(sb, "\n%.*s: (offset=%d, size=%d)\n",
                           bb->label->len, str_data(bb->label), start, size);

            dump_inst(sb, base + start, size);
        }
    }
}

void vp_dump_code(vec_t(Code*) codes)
{
    SBuf sb;
    vp_buf_init(&sb);
    dump_codes(&sb, codes);
    fwrite(sb.b, 1, sbuf_len(&sb), stdout);
    fflush(stdout);
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

static void dump_type(SBuf* sb, Type* ty)
{
    Str* s = vp_type_tostr(ty);
    vp_buf_putstr(sb, s);
}

static void dump_typecache(SBuf* sb)
{
    vp_buf_putlit(sb, "\n-- cache qual --\n");
    for(uint32_t i = 0; i < V->cachequal.size; i++)
    {
        if(V->cachequal.keys[i])
        {
            dump_type(sb, (Type*)(uintptr_t)V->cachequal.keys[i]);
            vp_buf_putlit(sb, " -> ");
            dump_type(sb, (Type*)(uintptr_t)V->cachequal.vals[i]);
            vp_buf_putb(sb, '\n');
        }
    }
    vp_buf_putlit(sb, "\n-- cache ptr --\n");
    for(uint32_t i = 0; i < V->cacheptr.size; i++)
    {
        if(V->cacheptr.keys[i])
        {
            dump_type(sb, (Type*)(uintptr_t)V->cacheptr.keys[i]);
            vp_buf_putlit(sb, " -> ");
            dump_type(sb, (Type*)(uintptr_t)V->cacheptr.vals[i]);
            vp_buf_putb(sb, '\n');
        }
    }
    vp_buf_putlit(sb, "\n-- cache arr --\n");
    for(uint32_t i = 0; i < vec_len(V->cachearr); i++)
    {
        dump_type(sb, V->cachearr[i]);
        vp_buf_putb(sb, '\n');
    }
    vp_buf_putlit(sb, "\n-- cache func --\n");
    for(uint32_t i = 0; i < vec_len(V->cachefn); i++)
    {
        dump_type(sb, V->cachefn[i]);
        vp_buf_putb(sb, '\n');
    }
    vp_buf_putb(sb, '\n');
}

void vp_dump_typecache(void)
{
    SBuf sb;
    vp_buf_init(&sb);
    dump_typecache(&sb);
    fwrite(sb.b, 1, sbuf_len(&sb), stdout);
    fflush(stdout);
}

/* -- DOT dump ------------------------------------------------------ */

/* Escape special characters for DOT labels */
static void dot_esc(SBuf* sb, const char* str, uint32_t len)
{
    for(uint32_t i = 0; i < len; i++)
    {
        char c = str[i];
        switch(c)
        {
        case '"':  vp_buf_putlit(sb, "\\\""); break;
        case '\\': vp_buf_putlit(sb, "\\\\"); break;
        case '\n': vp_buf_putlit(sb, "\\n"); break;
        case '\t': vp_buf_putlit(sb, "    "); break;
        case '<':  vp_buf_putlit(sb, "\\<"); break;
        case '>':  vp_buf_putlit(sb, "\\>"); break;
        case '{':  vp_buf_putlit(sb, "\\{"); break;
        case '}':  vp_buf_putlit(sb, "\\}"); break;
        case '|':  vp_buf_putlit(sb, "\\|"); break;
        default:   vp_buf_putb(sb, (int)c); break;
        }
    }
}

/* Dump BB label as DOT node ID */
static void dot_node_id(SBuf* sb, Str* label, Str* fname)
{
    /* Node ID: fname_label */
    vp_buf_putstr(sb, fname);
    vp_buf_putb(sb, '_');
    vp_buf_putmem(sb, str_data(label) + 1, label->len - 1);
}

/* Dump IR instruction label */
static void dot_ir_label(SBuf* sb, IR* ir)
{
    SBuf tmp;
    vp_buf_init(&tmp);
    dump_ir(&tmp, ir);
    dot_esc(sb, tmp.b, sbuf_len(&tmp));
}

/* Dump single function code as DOT graph */
static void dot_code(SBuf* sb, Code* code)
{
    vec_t(BB*) bbs = code->bbs;
    if(!bbs || vec_len(bbs) == 0) return;

    vp_buf_putlit(sb, "  subgraph cluster_");
    vp_buf_putstr(sb, code->name);
    vp_buf_putlit(sb, " {\n");
    dump_fmt(sb, "    label=\"%.*s\";\n", code->name->len, str_data(code->name));
    vp_buf_putlit(sb, "    fontsize=16;\n"
                      "    fontname=\"Helvetica,Arial,sans-serif\";\n"
                      "    style=filled;\n"
                      "    color=lightgrey;\n"
                      "    node [style=filled,fillcolor=white];\n\n");

    /* Generate nodes */
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];

        vp_buf_putlit(sb, "    ");
        dot_node_id(sb, bb->label, code->name);
        vp_buf_putlit(sb, " [shape=record,label=\"{");

        dot_esc(sb, str_data(bb->label), bb->label->len);
        vp_buf_putb(sb, '|');

        /* IR instructions */
        bool first = true;
        for(uint32_t j = 0; j < vec_len(bb->irs); j++)
        {
            if(!first) vp_buf_putb(sb, '|');
            first = false;

            IR* ir = bb->irs[j];
            dot_ir_label(sb, ir);
        }

        vp_buf_putlit(sb, "}\"];\n");
    }

    vp_buf_putb(sb, '\n');

    /* Generate edges */
    for(uint32_t i = 0; i < vec_len(bbs); i++)
    {
        BB* bb = bbs[i];
        for(uint32_t j = 0; j < vec_len(bb->frombbs); j++)
        {
            BB* from = bb->frombbs[j];

            /* Edge: from -> bb */
            vp_buf_putlit(sb, "    ");
            dot_node_id(sb, from->label, code->name);
            vp_buf_putlit(sb, " -> ");
            dot_node_id(sb, bb->label, code->name);
            vp_buf_putlit(sb, ";\n");
        }
    }

    vp_buf_putlit(sb, "  }\n\n");
}

/* Generate DOT graph for all functions */
static void dot_graphs(SBuf* sb, vec_t(Code*) codes)
{
    /* DOT header */
    vp_buf_putlit(sb, "digraph IR {\n"
                      "  rankdir=TB;\n"
                      "  node [fontname=\"Courier New,monospace\",fontsize=10];\n"
                      "  edge [fontname=\"Helvetica,Arial,sans-serif\",fontsize=9];\n"
                      "  graph [compound=true];\n\n");

    for(uint32_t i = 0; i < vec_len(codes); i++)
    {
        dot_code(sb, codes[i]);
    }

    vp_buf_putlit(sb, "}\n");
}

/* Dump functions as DOT graph */
void vp_dump_dot(vec_t(Code*) codes, const char* filename)
{
    SBuf* sb = vp_buf_tmp_(V);
    dot_graphs(sb, codes);

    if(filename)
    {
        FILE* f = fopen(filename, "w");
        if(f)
        {
            fwrite(sb->b, 1, sbuf_len(sb), f);
            fclose(f);
        }
    }
    else
    {
        fwrite(sb->b, 1, sbuf_len(sb), stdout);
        fflush(stdout);
    }
}

/* -- AST dump ------------------------------------------------------ */

static void dump_ast_type(SBuf* sb, Type* ty)
{
    if(ty)
    {
        vp_buf_putb(sb, '(');
        dump_type(sb, ty);
        vp_buf_putlit(sb, ") ");
    }
}

static void dump_ast_expr(SBuf* sb, Expr* e);

static void dump_ast_attr(SBuf* sb, Attr* attr)
{
    vp_buf_putlit(sb, "[[");
    vp_buf_putstr(sb, attr->name);
    uint32_t argsnum = vec_len(attr->args);
    if(argsnum)
    {
        vp_buf_putb(sb, '(');
        for(uint32_t i = 0; i < argsnum; i++)
        {
            dump_ast_expr(sb, attr->args[i].e);
            if(i != argsnum - 1)
            {
                vp_buf_putlit(sb, ", ");
            }
        }
        vp_buf_putb(sb, ')');
    }
    vp_buf_putlit(sb, "]]\n");
}

static void dump_typespec(SBuf* sb, TypeSpec* spec)
{
    if(spec->qual & SPEC_CONST)
    {
        vp_buf_putlit(sb, "const ");
    }
    switch(spec->kind)
    {
        case SPEC_NAME:
            vp_buf_putstr(sb, spec->name);
            break;
        case SPEC_TYPE:
            vp_buf_putmem(sb, type_name(spec->ty), strlen(type_name(spec->ty)));
            break;
        case SPEC_PTR:
            dump_typespec(sb, spec->ptr);
            vp_buf_putb(sb, '*');
            break;
        case SPEC_ARRAY:
        {
            TypeSpec* base = spec->arr.base;
            while(base->kind == SPEC_ARRAY)
            {
                base = base->arr.base;
            }

            if(base->kind == SPEC_FN)
            {
                /* Array of functions */
                TypeSpec* fnspec = base;
                vp_buf_putlit(sb, "fn");

                if(fnspec->qual & SPEC_NILABLE)
                {
                    vp_buf_putb(sb, '?');
                }

                /* Render all array subscripts from outermost to innermost */
                TypeSpec* curr = spec;
                while(curr->kind == SPEC_ARRAY)
                {
                    vp_buf_putb(sb, '[');
                    if(curr->arr.expr != NULL)
                        dump_ast_expr(sb, curr->arr.expr);
                    vp_buf_putb(sb, ']');
                    curr = curr->arr.base;
                }

                /* Render function signature */
                vp_buf_putb(sb, '(');
                for(uint32_t i = 0; i < vec_len(fnspec->fn.args); i++)
                {
                    TypeSpec* ts = fnspec->fn.args[i];
                    dump_typespec(sb, ts);
                    if(i != vec_len(fnspec->fn.args) - 1)
                    {
                        vp_buf_putlit(sb, ", ");
                    }
                }
                vp_buf_putlit(sb, ") : ");
                dump_typespec(sb, fnspec->fn.ret);
            }
            else
            {
                /* Normal array */
                dump_typespec(sb, spec->arr.base);
                vp_buf_putb(sb, '[');
                if(spec->arr.expr != NULL)
                    dump_ast_expr(sb, spec->arr.expr);
                vp_buf_putb(sb, ']');
            }
            break;
        }
        case SPEC_FN:
            vp_buf_putlit(sb, "fn(");
            for(uint32_t i = 0; i < vec_len(spec->fn.args); i++)
            {
                TypeSpec* ts = spec->fn.args[i];
                dump_typespec(sb, ts);
                if(i != vec_len(spec->fn.args) - 1)
                {
                    vp_buf_putlit(sb, ", ");
                }
            }
            vp_buf_putlit(sb, ") : ");
            dump_typespec(sb, spec->fn.ret);
            break;
        case SPEC_TYPEOF:
            vp_buf_putlit(sb, "typeof(");
            dump_ast_expr(sb, spec->expr);
            vp_buf_putb(sb, ')');
            break;
        default:
            vp_assertX(0, "unknown typespec");
            break;
    }
    if(spec->qual & SPEC_NILABLE)
    {
        vp_buf_putb(sb, '?');
    }
}

static void dump_ast_aggr(SBuf* sb, Aggregate* agr)
{
    if(!agr->items) return;
    for(uint32_t i = 0; i < vec_len(agr->items); i++)
    {
        dump_indent(sb);
        AggregateItem* item = &agr->items[i];
        switch(item->kind)
        {
            case AGR_ITEM_FIELD:
            {
                for(uint32_t j = 0; j < vec_len(item->names); j++)
                {
                    Str* name = item->names[j];
                    dump_fmt(sb, "%s", str_data(name));
                    if(j != vec_len(item->names) - 1)
                    {
                        vp_buf_putlit(sb, ", ");
                    }
                }
                vp_buf_putlit(sb, " : ");
                dump_typespec(sb, item->type);
                break;
            }
            case AGR_ITEM_SUB:
            {
                Aggregate* sub = item->sub;
                dump_fmt(sb, "%s\n", sub->kind == AGR_UNION ? "union" : "struct");
                dump_indent(sb);
                vp_buf_putlit(sb, "{\n");
                indent++;
                dump_ast_aggr(sb, sub);
                indent--;
                dump_indent(sb);
                vp_buf_putlit(sb, "}\n");
                break;
            }
            default:
                vp_assertX(0, "?");
                break;
        }
        vp_buf_putb(sb, '\n');
    }
}

static const char* const exprcast[] = {"cast", "intcast", "floatcast", "ptrcast", "bitcast"};

static void dump_ast_expr(SBuf* sb, Expr* e)
{
    switch(e->kind)
    {
        case EX_TRUE:
            vp_buf_putlit(sb, "true");
            break;
        case EX_FALSE:
            vp_buf_putlit(sb, "false");
            break;
        case EX_NIL:
            vp_buf_putlit(sb, "nil");
            break;
        case EX_CHAR:
            vp_buf_putb(sb, '\'');
            dump_char(sb, e->i);
            vp_buf_putb(sb, '\'');
            break;
        case EX_UINTT:
            dump_fmt(sb, "%llu", e->uintt.u);
            break;
        case EX_UINT:
            dump_fmt(sb, "%llu", e->u);
            break;
        case EX_INT:
            dump_fmt(sb, "%lli", e->i);
            break;
        case EX_NUM:
            dump_fmt(sb, "%.14g", e->n);
            break;
        case EX_FLO:
            dump_fmt(sb, "%f", e->f);
            break;
        case EX_STR:
            dump_str(sb, e->name);
            break;
        case EX_NAME:
            vp_buf_putstr(sb, e->name);
            break;
        case EX_COMPLIT:
        {
            if(e->comp.spec)
            {
                dump_typespec(sb, e->comp.spec);
                vp_buf_putb(sb, ' ');
            }
            vp_buf_putb(sb, '{');
            for(Field* c = e->comp.fields; c != vec_end(e->comp.fields); c++)
            {
                if(c->kind == FIELD_IDX)
                {
                    vp_buf_putb(sb, '[');
                    dump_ast_expr(sb, c->idx);
                    vp_buf_putlit(sb, "] = ");
                }
                else if(c->kind == FIELD_NAME)
                {
                    dump_fmt(sb, "%s = ", str_data(c->name));
                }
                dump_ast_expr(sb, c->init);
                if(c != vec_end(e->comp.fields) - 1)
                {
                    vp_buf_putlit(sb, ", ");
                }
            }
            vp_buf_putb(sb, '}');
            break;
        }
        case EX_REF:
        case EX_DEREF:
        case EX_NEG:
        case EX_NOT:
        case EX_BNOT:
            dump_fmt(sb, "%s", ast_unaryname(e->kind));
            dump_ast_expr(sb, e->unary);
            break;
        case EX_PREINC:
            vp_buf_putlit(sb, "++");
            dump_ast_expr(sb, e->unary);
            break;
        case EX_PREDEC:
            vp_buf_putlit(sb, "--");
            dump_ast_expr(sb, e->unary);
            break;
        case EX_POSTINC:
            dump_ast_expr(sb, e->unary);
            vp_buf_putlit(sb, "++");
            break;
        case EX_POSTDEC:
            dump_ast_expr(sb, e->unary);
            vp_buf_putlit(sb, "--");
            break;
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
            vp_buf_putb(sb, '(');
            dump_ast_expr(sb, e->binop.lhs);
            dump_fmt(sb, " %s ", ast_binname(e->kind));
            dump_ast_expr(sb, e->binop.rhs);
            vp_buf_putb(sb, ')');
            break;
        case EX_CAST:
        case EX_INTCAST:
        case EX_FLOATCAST:
        case EX_PTRCAST:
        case EX_BITCAST:
        {
            dump_fmt(sb, "%s(", exprcast[e->kind - EX_CAST]);
            dump_typespec(sb, e->cast.spec);
            vp_buf_putlit(sb, ", ");
            dump_ast_expr(sb, e->cast.expr);
            vp_buf_putb(sb, ')');
            break;
        }
        case EX_CALL:
        {
            dump_ast_expr(sb, e->call.expr);
            vp_buf_putb(sb, '(');
            for(uint32_t i = 0; i < vec_len(e->call.args); i++)
            {
                Expr* arg = e->call.args[i];
                dump_ast_expr(sb, arg);
                if(i != vec_len(e->call.args) - 1)
                {
                    vp_buf_putlit(sb, ", ");
                }
            }
            vp_buf_putb(sb, ')');
            break;
        }
        case EX_SIZEOF:
            vp_buf_putlit(sb, "sizeof(");
            dump_typespec(sb, e->spec);
            vp_buf_putb(sb, ')');
            break;
        case EX_ALIGNOF:
            vp_buf_putlit(sb, "alignof(");
            dump_typespec(sb, e->spec);
            vp_buf_putb(sb, ')');
            break;
        case EX_OFFSETOF:
            vp_buf_putlit(sb, "offsetof(");
            dump_typespec(sb, e->ofst.spec);
            vp_buf_putlit(sb, ", ");
            vp_buf_putstr(sb, e->ofst.name);
            vp_buf_putb(sb, ')');
            break;
        case EX_FIELD:
            dump_ast_expr(sb, e->field.expr);
            dump_fmt(sb, ".%s", str_data(e->field.name));
            break;
        case EX_ACCESS:
            dump_ast_expr(sb, e->access.expr);
            dump_fmt(sb, "::%s", str_data(e->access.name));
            break;
        case EX_IDX:
            dump_ast_expr(sb, e->idx.expr);
            vp_buf_putb(sb, '[');
            dump_ast_expr(sb, e->idx.index);
            vp_buf_putb(sb, ']');
            break;
        default:
            vp_assertX(0, "unknown expression %d", e->kind);
            break;
    }
}

static void dump_ast_note(SBuf* sb, Note* note)
{
    uint32_t argsnum = vec_len(note->args);
    if(argsnum)
    {
        for(uint32_t i = 0; i < argsnum; i++)
        {
            dump_ast_expr(sb, note->args[i].e);
            if(i != vec_len(note->args) - 1)
            {
                vp_buf_putlit(sb, ", ");
            }
        }
    }
}

static void dump_ast_stmt(SBuf* sb, Stmt* st);
static void dump_decl(SBuf* sb, Decl* d);

static void dump_ast_block(SBuf* sb, Stmt* st)
{
    vp_assertX(st->kind == ST_BLOCK, "not block");
    dump_indent(sb);
    indent++;
    vp_buf_putlit(sb, "{\n");
    for(uint32_t i = 0; i < vec_len(st->block); i++)
    {
        Stmt* stmt = st->block[i];
        dump_ast_stmt(sb, stmt);
    }
    indent--;
    dump_indent(sb);
    vp_buf_putb(sb, '}');
}

static void dump_ast_stmt(SBuf* sb, Stmt* st)
{
    switch(st->kind)
    {
        case ST_IF:
            dump_indent(sb);
            vp_buf_putlit(sb, "if ");
            dump_ast_expr(sb, st->ifst.cond);
            vp_buf_putb(sb, '\n');
            dump_ast_block(sb, st->ifst.tblock);
            vp_buf_putb(sb, '\n');
            /* Else block, if any */
            if(st->ifst.fblock)
            {
                dump_indent(sb);
                vp_buf_putlit(sb, "else\n");
                dump_ast_stmt(sb, st->ifst.fblock);
                vp_buf_putb(sb, '\n');
            }
            break;
        case ST_FOR:
            dump_indent(sb);
            vp_buf_putlit(sb, "for ");
            dump_ast_stmt(sb, st->forst.init);
            vp_buf_putlit(sb, "; ");
            dump_ast_expr(sb, st->forst.cond);
            vp_buf_putlit(sb, "; ");
            dump_ast_stmt(sb, st->forst.next);
            vp_buf_putb(sb, '\n');
            dump_ast_block(sb, st->forst.body);
            vp_buf_putb(sb, '\n');
            break;
        case ST_WHILE:
            dump_indent(sb);
            vp_buf_putlit(sb, "while ");
            dump_ast_expr(sb, st->whst.cond);
            vp_buf_putb(sb, '\n');
            dump_ast_block(sb, st->whst.body);
            vp_buf_putb(sb, '\n');
            break;
        case ST_RETURN:
            dump_indent(sb);
            vp_buf_putlit(sb, "return ");
            if(st->expr)
            {
                dump_ast_expr(sb, st->expr);
            }
            vp_buf_putb(sb, '\n');
            break;
        case ST_BREAK:
            dump_indent(sb);
            vp_buf_putlit(sb, "break ");
            vp_buf_putb(sb, '\n');
            break;
        case ST_ASM:
            dump_indent(sb);
            vp_buf_putlit(sb, "asm ...");
            vp_buf_putb(sb, '\n');
            break;
        case ST_CONTINUE:
            dump_indent(sb);
            vp_buf_putlit(sb, "continue ");
            vp_buf_putb(sb, '\n');
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
            dump_indent(sb);
            dump_ast_type(sb, st->lhs->ty);
            dump_ast_expr(sb, st->lhs);
            dump_fmt(sb, " %s ", ast_assignname(st->kind));
            dump_ast_type(sb, st->rhs->ty);
            dump_ast_expr(sb, st->rhs);
            vp_buf_putb(sb, '\n');
            break;
        case ST_EXPR:
            dump_indent(sb);
            dump_ast_type(sb, st->expr->ty);
            dump_ast_expr(sb, st->expr);
            vp_buf_putb(sb, '\n');
            break;
        case ST_BLOCK:
            dump_ast_block(sb, st);
            break;
        case ST_DECL:
            dump_decl(sb, st->decl);
            break;
        default:
            vp_assertX(0, "unknown statement");
            break;
    }
}

static void dump_decl(SBuf* sb, Decl* d)
{
    if(d->flags & DECL_FLAG_PUB)
    {
        vp_buf_putlit(sb, "pub ");
    }
    switch(d->kind)
    {
        case DECL_IMPORT:
        {
            Str* path = d->name;
            if(d->imp.items)
            {
                /* from "file" import foo, bar as b */
                vp_buf_putlit(sb, "from ");
                dump_str(sb, path);
                vp_buf_putlit(sb, " import ");
                for(uint32_t i = 0; i < vec_len(d->imp.items); i++)
                {
                    ImportItem* item = &d->imp.items[i];
                    vp_buf_putstr(sb, item->name);
                    if(item->alias)
                    {
                        dump_fmt(sb, " as %s", str_data(item->alias));
                    }
                    if(i != vec_len(d->imp.items) - 1)
                    {
                        vp_buf_putlit(sb, ", ");
                    }
                }
            }
            else if(d->imp.wildcard)
            {
                /* from "file" import * */
                vp_buf_putlit(sb, "from ");
                dump_str(sb, path);
                vp_buf_putlit(sb, " import *");
            }
            else if(d->imp.alias)
            {
                /* import "file" as mod */
                vp_buf_putlit(sb, "import ");
                dump_str(sb, path);
                dump_fmt(sb, " as %s", str_data(d->imp.alias));
            }
            else
            {
                /* import "file" */
                vp_buf_putlit(sb, "import ");
                dump_str(sb, path);
            }
            vp_buf_putb(sb, '\n');
            break;
        }
        case DECL_TYPE:
            dump_fmt(sb, "type %s = ", str_data(d->name));
            dump_typespec(sb, d->ts.spec);
            vp_buf_putb(sb, '\n');
            break;
        case DECL_ALIAS:
            dump_fmt(sb, "alias %s = ", str_data(d->name));
            dump_typespec(sb, d->ts.spec);
            vp_buf_putb(sb, '\n');
            break;
        case DECL_DEF:
        {
            dump_indent(sb);
            dump_fmt(sb, "def %s", str_data(d->name));
            if(d->def.spec)
            {
                vp_buf_putlit(sb, " : ");
                dump_typespec(sb, d->def.spec);
            }
            vp_buf_putlit(sb, " = ");
            dump_ast_type(sb, d->def.expr->ty);
            dump_ast_expr(sb, d->def.expr);
            vp_buf_putb(sb, '\n');
            break;
        }
        case DECL_VAR:
        case DECL_LET:
        {
            dump_indent(sb);
            if(d->kind == DECL_VAR)
            {
                vp_buf_putlit(sb, "var ");
            }
            else
            {
                vp_buf_putlit(sb, "let ");
            }
            vp_buf_putstr(sb, d->name);
            if(d->var.spec)
            {
                vp_buf_putlit(sb, " : ");
                dump_typespec(sb, d->var.spec);
            }
            if(d->var.expr)
            {
                vp_buf_putlit(sb, " = ");
                dump_ast_type(sb, d->var.expr->ty);
                dump_ast_expr(sb, d->var.expr);
            }
            vp_buf_putb(sb, '\n');
            break;
        }
        case DECL_FN:
        {
            if(d->fn.attrs)
            {
                for (uint32_t i = 0; i < vec_len(d->fn.attrs); i++)
                {
                    dump_ast_attr(sb, &d->fn.attrs[i]);
                }
            }
            dump_fmt(sb, "fn %s(", str_data(d->name));
            for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
            {
                Param* param = &d->fn.params[i];
                dump_fmt(sb, "%s : ", str_data(param->name));
                dump_typespec(sb, param->spec);
                if(i != vec_len(d->fn.params) - 1)
                {
                    vp_buf_putlit(sb, ", ");
                }
            }
            vp_buf_putb(sb, ')');
            if(d->fn.ret)
            {
                vp_buf_putlit(sb, " : ");
                dump_typespec(sb, d->fn.ret);
            }
            if(d->fn.body)
            {
                vp_buf_putb(sb, '\n');
                dump_ast_block(sb, d->fn.body);
            }
            vp_buf_putb(sb, '\n');
            break;
        }
        case DECL_ENUM:
            dump_fmt(sb, "enum %s", str_data(d->name));
            if (d->enm.spec)
            {
                vp_buf_putlit(sb, " : ");
                dump_typespec(sb, d->enm.spec);
            }
            vp_buf_putlit(sb, "\n{\n");
            indent++;
            for(uint32_t i = 0; i < vec_len(d->enm.items); i++)
            {
                dump_indent(sb);
                EnumItem* item = &d->enm.items[i];
                vp_buf_putstr(sb, item->name);
                if(item->init)
                {
                    vp_buf_putlit(sb, " = ");
                    dump_ast_expr(sb, item->init);
                }
                if(i != vec_len(d->enm.items) - 1)
                {
                    vp_buf_putlit(sb, ",");
                }
                vp_buf_putb(sb, '\n');
            }
            indent--;
            vp_buf_putlit(sb, "}\n");
            break;
        case DECL_UNION:
            dump_fmt(sb, "union %s\n{\n", str_data(d->name));
            indent++;
            dump_ast_aggr(sb, d->agr);
            indent--;
            dump_fmt(sb, "}\n");
            break;
        case DECL_STRUCT:
            dump_fmt(sb, "struct %s\n{\n", str_data(d->name));
            indent++;
            dump_ast_aggr(sb, d->agr);
            indent--;
            vp_buf_putlit(sb, "}\n");
            break;
        case DECL_NOTE:
            dump_indent(sb);
            dump_fmt(sb, "#%s", str_data(d->note.name));
            dump_ast_note(sb, &d->note);
            vp_buf_putb(sb, '\n');
            break;
        default:
            vp_assertX(0, "unknown decl");
            break;
    }
}

void vp_dump_decl(Decl* d)
{
    SBuf sb;
    vp_buf_init(&sb);
    dump_decl(&sb, d);
    fwrite(sb.b, 1, sbuf_len(&sb), stdout);
    fflush(stdout);
}