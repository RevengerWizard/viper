/*
** vp_code.c
** Code generation (AST -> IR)
*/

#include <stdbool.h>
#include <string.h>

#include "vp_codegen.h"
#include "vp_abi.h"
#include "vp_link.h"
#include "vp_low.h"
#include "vp_mem.h"
#include "vp_opt.h"
#include "vp_regalloc.h"
#include "vp_ast.h"
#include "vp_ir.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_tab.h"
#include "vp_target.h"
#include "vp_target_x64.h"
#include "vp_type.h"
#include "vp_var.h"
#include "vp_vec.h"

#include "vp_dump.h"

/* Generate storage location for variable */
static VReg* gen_varinfo(VarInfo* vi)
{
    VReg* vr;
    vp_assertX(vi, "empty variable info");
    if(!ty_isscalar(vi->type))
    {
        vi->fi = vp_frameinfo_new();
        vi->vreg = vr = NULL;
    }
    else
    {
        vi->vreg = vr = vp_vreg_new(vi->type);
        vi->fi = NULL;
    }
    return vr;
}

/* Zero a memory vreg */
static void gen_memzero(Type* ty, VReg* dst)
{
    uint32_t size = vp_type_sizeof(ty);
    uint32_t align = vp_type_alignof(ty);
    vp_assertX(size, "size == 0");
    VRSize velem = vp_vreg_elem(size, align);
    uint32_t count = size >> velem;
    vp_assertX(count, "count == 0");
    VReg* vzero = vp_vreg_ki(0, velem);
    if(count == 1)
    {
        vp_ir_store(dst, vzero, 0);
    }
    else
    {
        VReg* dstp = vp_ra_spawn(VRSize8, 0);
        vp_ir_mov(dstp, dst, IRF_UNSIGNED);

        VRSize vsize = VRSize8;
        VReg* vcount = vp_ra_spawn(vsize, 0);
        vp_ir_mov(vcount, vp_vreg_ki(count, vsize), IRF_UNSIGNED);
        VReg* vadd = vp_vreg_ki(1 << velem, vsize);

        BB* bbloop = vp_bb_new();
        vp_bb_setcurr(bbloop);
        /* *dstp = 0 */
        vp_ir_store(dstp, vzero, 0);
        /* dstp += velem */
        vp_ir_mov(dstp, vp_ir_binop(IR_ADD, dstp, vadd, dstp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount -= 1 */
        vp_ir_mov(vcount, vp_ir_binop(IR_SUB, vcount, vp_vreg_ki(1, vsize), vcount->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount != 0 */
        vp_ir_cjmp(vcount, vp_vreg_ki(0, vcount->vsize), COND_NEQ, bbloop);
        vp_bb_setcurr(vp_bb_new());
    }
}

static void gen_memcpy(Type* ty, VReg* dst, VReg* src)
{
    uint32_t size = vp_type_sizeof(ty);
    uint32_t align = vp_type_alignof(ty);
    vp_assertX(size, "size == 0");
    VRSize velem = vp_vreg_elem(size, align);
    uint32_t count = size >> velem;
    vp_assertX(count, "count == 0");
    if(count == 1)
    {
        VReg* tmp = vp_ir_load(src, velem, vp_vflag(ty), 0)->dst;
        vp_ir_store(dst, tmp, 0);
    }
    else
    {
        VReg* srcp = vp_ra_spawn(VRSize8, 0);
        vp_ir_mov(srcp, src, IRF_UNSIGNED);
        VReg* dstp = vp_ra_spawn(VRSize8, 0);
        vp_ir_mov(dstp, dst, IRF_UNSIGNED);

        VRSize vsize = VRSize8;
        VReg* vcount = vp_ra_spawn(vsize, 0);
        vp_ir_mov(vcount, vp_vreg_ki(count, vsize), IRF_UNSIGNED);
        VReg* vadd = vp_vreg_ki(1 << velem, vsize);

        BB* bbloop = vp_bb_new();
        vp_bb_setcurr(bbloop);
        VReg* tmp = vp_ir_load(srcp, velem, vp_vflag(ty), 0)->dst;
        /* srcp += velem */
        vp_ir_mov(srcp, vp_ir_binop(IR_ADD, srcp, vadd, srcp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* *dstp = *srcp */
        vp_ir_store(dstp, tmp, 0);
        /* dstp += velem */
        vp_ir_mov(dstp, vp_ir_binop(IR_ADD, dstp, vadd, dstp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount -= 1 */
        vp_ir_mov(vcount, vp_ir_binop(IR_SUB, vcount, vp_vreg_ki(1, vsize), vcount->vsize, IRF_UNSIGNED), IRF_UNSIGNED);
        /* vcount != 0 */
        vp_ir_cjmp(vcount, vp_vreg_ki(0, vcount->vsize), COND_NEQ, bbloop);
        vp_bb_setcurr(vp_bb_new());
    }
}

/* Generate store operation, based on type */
static void gen_store(VReg* dst, VReg* src, Type* ty)
{
    if(ty_isscalar(ty))
    {
        vp_ir_store(dst, src, ir_flag(ty));
    }
    else if(ty_isaggr(ty))
    {
        gen_memcpy(ty, dst, src);
    }
    else
    {
        vp_assertX(0, "unsupported type for assignment store");
    }
}

/* Forward declarations */
static void gen_block(Stmt* st);
static void gen_stmt(Stmt* st);
static VReg* gen_expr(Expr* e);
static VReg* gen_complit(Expr* e);
static VReg* gen_lval(Expr* e);
static void gen_var_assign(VarInfo* vi, Expr* e);
static VReg* gen_inline(Expr* e, Type* ret, Stmt* body, Code* code);

/* -- Gen expressions ----------------------------------------------- */

/* Generate 'nil' constant (0) */
static VReg* gen_nil(Expr* e)
{
    return vp_vreg_ki(0, vp_vsize(e->ty));
}

/* Generate 'true' constant (1) */
static VReg* gen_true(Expr* e)
{
    return vp_vreg_ki(e->b, vp_vsize(e->ty));
}

/* Generate 'false' constant (0) */
static VReg* gen_false(Expr* e)
{
    return vp_vreg_ki(e->b, vp_vsize(e->ty));
}

/* Generate integer constant */
static VReg* gen_int(Expr* e)
{
    return vp_vreg_ki(e->i, vp_vsize(e->ty));
}

/* Generate unsigned integer constant */
static VReg* gen_uint(Expr* e)
{
    return vp_vreg_ki(e->u, vp_vsize(e->ty));
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

static Str* str_data_get(Str* str)
{
    Str* sym = NULL;
    for(uint32_t i = 0; i < vec_len(V->globdata); i++)
    {
        DataEntry* de = V->globdata[i];
        if(de->kind == DATA_STR && de->size == (str->len + 1))
        {
            if(memcmp(de->data, str_data(str), str->len) == 0 &&
                de->data[str->len] == 0)
            {
                sym = de->name;
                break;
            }
        }
    }

    if(!sym)
    {
        sym = vp_str_label();

        uint32_t size = str->len + 1; /* + '\0' */
        DataEntry* de = vp_mem_calloc(1, sizeof(DataEntry));
        de->kind = DATA_STR;
        de->name = sym;
        de->ofs = 0;
        de->size = size;
        de->align = 2;
        de->data = vp_mem_calloc(1, size);
        memcpy(de->data, str_data(str), str->len);
        de->data[str->len] = 0;  /* '\0' */

        vec_push(V->globdata, de);
    }

    return sym;
}

/* Generate string literal */
static VReg* gen_str(Expr* e)
{
    vp_assertX(e->kind == EX_STR, "string literal");
    vp_assertX(e->ty, "missing string type");

    Str* str = e->str;

    if(ty_isarr(e->ty))
    {
        vp_assertX(e->ty->p == tyuint8, "bad str array type");

        FrameInfo* fi = vp_frameinfo_new();
        VReg* base = vp_ir_bofs(fi)->dst;

        Slot sl = {.type = e->ty, .fi = fi};
        vec_push(V->fncode->slots, sl);

        gen_memzero(e->ty, base);

        /* Generate stores for each char in the string */
        for(uint32_t i = 0; i < str->len; i++)
        {
            /* Calculate index address */
            VReg* idx;
            if(i == 0)
            {
                idx = base;
            }
            else
            {
                VReg* offset_reg = vp_vreg_ki(i, VRSize8);
                idx = vp_ir_binop(IR_ADD, base, offset_reg, VRSize8, IRF_UNSIGNED);
            }

            VReg* value = vp_vreg_ki(str_data(str)[i], VRSize1);
            gen_store(idx, value, tyuint8);
        }

        return base;
    }
    else if(ty_isptr(e->ty))
    {
        vp_assertX(e->ty->p && e->ty->p->kind == TY_uint8, "bad str ptr type");

        Str* sym = str_data_get(str);

        return vp_ir_iofs(sym, false, true, false)->dst;
    }
    vp_assertX(0, "bad str type");
    return NULL;
}

/* Generate name reference */
static VReg* gen_name(Expr* e)
{
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        VarInfo* vi = e->vi;
        vp_assertX(vi, "name not found");
        if(!vs_isglob(vi))
        {
            vp_assertX(vi->vreg, "missing vreg %s", str_data(vi->name));
            return vi->vreg;
        }
        VReg* src = gen_lval(e);
        if(ty_isfn(ty))
        {
            return src;
        }
        return vp_ir_load(src, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
    }
    else if(ty_isarr(ty) || ty_isaggr(ty))
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
    Type* ty = e->ty;
    uint8_t flag = ir_flag(ty);
    VReg* lhs = gen_expr(e->binop.lhs);
    VReg* rhs = gen_expr(e->binop.rhs);
    return vp_ir_binop(e->kind + (IR_ADD - EX_ADD), lhs, rhs, vp_vsize(ty), flag);
}

/* Generate unary operation */
static VReg* gen_unary(Expr* e)
{
    vp_assertX(EX_NEG <= e->kind && e->kind <= EX_BNOT, "not a unary operator");
    uint8_t flag = ir_flag(e->ty);
    VReg* src = gen_expr(e->unary);
    return vp_ir_unary(e->kind + (IR_NEG - EX_NEG), src, vp_vsize(e->ty), flag);
}

/* Generate unary ! */
static VReg* gen_not(Expr* e)
{
    vp_assertX(e->kind == EX_NOT, "! operator");
    VReg* src = gen_expr(e->unary);
    CondKind cond = COND_EQ;
    uint8_t flag = cond_flag(e->ty);
    VReg* zero = vp_vreg_ki(0, src->vsize);
    return vp_ir_cond(src, zero, cond | flag)->dst;
}

/* Generate pre/post increment/decrement */
static VReg* gen_incdec(Expr* e)
{
#define ISPOST(e) ((e)->kind >= EX_POSTINC)
#define ISDEC(e)  (((e)->kind - EX_PREINC) & 1)
    vp_assertX(e->kind == EX_PREINC || e->kind == EX_POSTINC || e->kind == EX_PREDEC || e->kind == EX_POSTDEC, "not pre/post inc/dec");
    vp_assertX(e->ty, "no type");
    Expr* unary = e->unary;
    VarInfo* vi = NULL;
    if(unary->kind == EX_NAME && !vs_isglob(unary->vi))
    {
        vi = unary->vi;
    }

    VRSize vs = vp_vsize(e->ty);
    VReg* before = NULL;
    VReg* lval = NULL;
    VReg* val;
    uint8_t irflag = ir_flag(e->ty);
    if(vi)
    {
        val = vi->vreg;
        if(ISPOST(e))
        {
            before = vp_vreg_new(unary->ty);
            vp_ir_mov(before, val, irflag);
        }
    }
    else
    {
        lval = gen_lval(unary);
        val = vp_ir_load(lval, vs, vp_vflag(e->ty), irflag)->dst;
        if(ISPOST(e))
        {
            before = val;
        }
    }

    VReg* addend;
    if(ty_isflo(unary->ty))
    {
        addend = vp_vreg_kf(1, vp_vsize(unary->ty));
    }
    else
    {
        addend = vp_vreg_ki(ty_isptrlike(e->ty) ? vp_type_sizeof(e->ty) : 1, vs);
    }
    VReg* after = vp_ir_binop(ISDEC(e) ? IR_SUB : IR_ADD, val, addend, vs, irflag);
    if(vi)
    {
        vp_ir_mov(vi->vreg, after, irflag);
    }
    else
    {
        vp_ir_store(lval, after, irflag);
    }
    return before ? before : after;
#undef ISPOST
#undef ISDEC
}

/* Generate reference/address */
static VReg* gen_ref(Expr* e)
{
    switch(e->kind)
    {
        case EX_NAME:
        {
            Str* name = e->name;
            VarInfo* vi = e->vi;
            vp_assertX(vi, "name not found");
            if(vs_isglob(vi))
            {
                return vp_ir_iofs(name, vi->storage & VS_FN, false, true)->dst;
            }
            if(!vi->fi)
            {
                if(!vrf_spill(vi->vreg))
                {
                    vi->vreg->flag |= VRF_REF;
                    vreg_spill(vi->vreg);
                    V->ra->flag |= RAF_STACK_FRAME;
                }
                vi->fi = &vi->vreg->fi;
            }
            return vp_ir_bofs(vi->fi)->dst;
        }
        case EX_DEREF:
            return gen_expr(e->unary);
        case EX_FIELD:
        {
            Type* basety = e->field.expr->ty;
            if(ty_isptr(basety))
            {
                basety = basety->p;
            }
            VReg* base = gen_expr(e->field.expr);
            uint32_t offset = vp_type_offset(basety, e->field.name);
            if(offset == 0)
                return base;

            /* base + offset */
            VReg* ofsreg = vp_vreg_ki(offset, VRSize8);
            return vp_ir_binop(IR_ADD, base, ofsreg, VRSize8, IRF_UNSIGNED);
        }
        case EX_IDX:
        {
            VReg* base = gen_expr(e->idx.expr);
            VReg* idx = gen_expr(e->idx.index);
            Type* elemty = e->idx.expr->ty->p;
            uint32_t elemsize = vp_type_sizeof(elemty);

            /* base + (idx * sizeof(base)) */
            if(elemsize > 1)
            {
                VReg* sizevr = vp_vreg_ki(elemsize, VRSize8);
                idx = vp_ir_binop(IR_MUL, idx, sizevr, VRSize8, IRF_UNSIGNED);
            }
            return vp_ir_binop(IR_ADD, base, idx, VRSize8, IRF_UNSIGNED);
        }
        case EX_COMPLIT:
        {
            return gen_complit(e);
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
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        return vp_ir_load(freg, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
    }
    return freg;
}

/* Generate array index */
static VReg* gen_idx(Expr* e)
{
    vp_assertX(e->kind == EX_IDX, "not an index expression");
    VReg* addr = gen_lval(e);
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        return vp_ir_load(addr, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
    }
    return addr;
}

/* Generate function call */
static VReg* gen_call(Expr* e)
{
    vp_assertX(e->kind == EX_CALL, "not a call expression");
    vp_assertX(e->ty, "call type");

    VReg* freg = NULL;
    Str* label = NULL;
    Code* fn = NULL;
    VarInfo* vi = NULL;
    bool isglob = false;

    /* Determine if this is a direct or indirect call */
    if(e->call.expr->kind == EX_NAME)
    {
        label = e->call.expr->name;
        vi = e->call.expr->vi;
        vp_assertX(vi, "'%s' not found", str_data(e->call.expr->name));
        vp_assertX(ty_isfn(vi->type), "'%s' not a function", str_data(vi->name));
        fn = vp_tab_get(&V->funcs, label);
        isglob = vs_isglob(vi) && !vs_isfn(vi);
    }

    if(!label)
    {
        freg = gen_expr(e->call.expr);
    }
    else if(isglob)
    {
        IR* ir = vp_ir_iofs(label, false, false, true);
        ir->iofs.got = true;
        freg = ir->dst;
        label = NULL;
    }

    /* Determine ABI to use */
    const ABIInfo* abi = fn ? fn->abi : V->T->abi;
    uint32_t shadow = (abi->flags & ABI_SHADOW) ? 32 : 0;

    Type* ret = e->ty;
    FrameInfo* fi = NULL;
    if(abi_isstack(ret))
    {
        fi = vp_frameinfo_new();
        Slot sl = {.type = ret, .fi = fi};
        vec_push(V->fncode->slots, sl);
    }

    typedef struct
    {
        ParamClass cls;    /* Parameter class */
        uint32_t idx;   /* Register index or stack offset */
        uint32_t size;
    } ArgInfo;

    vec_t(ArgInfo) arginfos = vec_init(ArgInfo);
    uint32_t argstart = (fi != NULL) ? 1 : 0;
    uint32_t iidx = argstart;
    uint32_t fidx = 0;
    uint32_t offset = 0;
    uint32_t regargs = 0;
    uint32_t argnum = vec_len(e->call.args);

    /* Classify arguments */
    for(uint32_t i = 0; i < argnum; i++)
    {
        Expr* arg = e->call.args[i];
        Type* argty = arg->ty;
        vp_assertX(argty, "missing arg type");

        uint32_t previidx = iidx, prevfidx = fidx;
        ParamClass cls = abi_classify(argty, abi, &iidx, &fidx);
        ArgInfo p = {.cls = cls, .size = vp_type_sizeof(argty)};
        switch(cls)
        {
            case PC_IREG:
                p.idx = abi->imap[previidx];
                regargs++;
                break;
            case PC_FREG:
                p.idx = abi->fmap[prevfidx];
                regargs++;
                break;
            case PC_STACK:
            case PC_MEM:
            {
                uint32_t align = vp_type_alignof(argty);
                offset = ALIGN_UP(offset, align);
                p.idx = offset;
                offset += ALIGN_UP(p.size, TARGET_PTR_SIZE);
                break;
            }
        }

        vec_push(arginfos, p);
    }

    uint32_t argtotal = argnum + argstart;
    VReg** args = argtotal == 0 ? NULL : vp_mem_calloc(argtotal, sizeof(*args));

    /* Pre-compute arguments */
    for(uint32_t i = 0; i < argnum; i++)
    {
        uint32_t vidx = i + argstart;
        args[vidx] = gen_expr(e->call.args[i]);
    }

    /* Generate arguments (in reverse, for stack) */
    for(uint32_t i = argnum; i-- > 0;)
    {
        Type* argty = e->call.args[i]->ty;
        VReg* src = args[i + argstart];
        ArgInfo* p = &arginfos[i];
        switch(p->cls)
        {
            case PC_IREG:
            case PC_FREG:
                vp_ir_pusharg(src, p->idx);
                break;
            case PC_STACK:
            {
                VReg* dst = vp_ir_sofs(p->idx + shadow)->dst;
                vp_ir_store(dst, src, ir_flag(argty));
                break;
            }
            case PC_MEM:
            {
                VReg* dst = vp_ir_sofs(p->idx + shadow)->dst;
                gen_memcpy(argty, dst, src);
                break;
            }
        }
    }
    if(fn && (fn->flags & FN_SYSCALL))
    {
        return gen_inline(NULL, ret, fn->body, fn);
    }

    /* Handle stack return */
    VReg* dst = NULL;
    if(fi != NULL)
    {
        VReg* dst = vp_ir_bofs(fi)->dst;
        vp_ir_pusharg(dst, abi->imap[0]);
        args[0] = dst;
        regargs++;
    }
    if(ret->kind != TY_void)
    {
        dst = vp_vreg_new(ret);
    }

    IRCallInfo* ci = vp_ircallinfo_new(args, argnum, label);
    ci->regargs = regargs;
    ci->stacksize = offset;
    ci->argtotal = argtotal;
    if(label)
    {
        ci->fn = fn;
        if(!ci->fn)
        {
            vp_tab_set(&V->ifuncs, label, NULL);
            ci->export = true;
        }
    }

    IR* ir = vp_ir_call(ci, dst, freg);
    vec_push(V->fncode->calls, ir);

    if(arginfos)
        vec_free(arginfos);

    return dst;
}

/* Generate cast expression */
static VReg* gen_cast(Expr* e)
{
    vp_assertX(e->kind == EX_CAST || e->kind == EX_INTCAST || e->kind == EX_FLOATCAST || e->kind == EX_PTRCAST, "not a cast expression");

    Type* dstty = e->ty;
    Type* srcty = e->cast.expr->ty;
    VReg* vr = gen_expr(e->cast.expr);
    switch(dstty->kind)
    {
        case TY_void:
        case TY_struct:
        case TY_union:
            return vr;
        default:
            break;
    }

    uint32_t dstsize = vp_type_sizeof(dstty);
    if(vrf_const(vr))
    {
        vp_assertX(!vrf_flo(vr), "const flo");
        int64_t i = vr->i64;
        if(dstsize < (1U << vr->vsize) && dstsize < sizeof(int64_t))
        {
            /* Assume two's complement */
            size_t bit = dstsize * 8;
            uint64_t mask = (-1ULL) << bit;
            if(!ty_isunsigned(dstty) && (i & ((int64_t)1 << (bit - 1))))    /* signed and negative */
                i |= mask;
            else
                i &= ~mask;
        }

        VRSize vsize = vp_vsize(dstty);
        return vp_vreg_ki(i, vsize);
    }

    uint32_t srcsize = 1U << vr->vsize;
    if(dstsize == srcsize &&
        ty_isflo(dstty) == (vrf_flo(vr) != 0) &&
        (ty_isflo(dstty) || ty_isunsigned(dstty) == ty_isunsigned(srcty)))
    {
        /* Ignore cast between equal integers/floats */
        return vr;
    }

    IR* ir = vp_ir_cast(vr, ty_isunsigned(srcty), vp_vsize(dstty), vp_vflag(dstty));
    ir->flag = ir_flag(dstty);
    return ir->dst;
}

static VReg* gen_bitcast(Expr* e)
{
    vp_assertX(e->kind == EX_BITCAST, "not a bitcast expression");

    /* Do nothing */
    return gen_expr(e->cast.expr);
}

typedef struct FlatField
{
    uint32_t offset;     /* Byte offset from base */
    Expr* init; /* Initializer expression */
    Type* type;
} FlatField;

/* Flatten a compound literal */
static void flatten_complit(Expr* e, uint32_t baseofs, vec_t(FlatField)* flat)
{
    vp_assertX(e->kind == EX_COMPLIT, "not a compound literal");

    Type* compty = e->ty;
    Field* fields = e->comp.fields;

    for(uint32_t i = 0; i < vec_len(fields); i++)
    {
        Field* field = &e->comp.fields[i];
        uint32_t field_offset = 0;

        /* Calculate field offset */
        switch(field->kind)
        {
            case FIELD_NAME:
            {
                uint32_t idx = vp_type_fieldidx(compty, field->name);
                field_offset = baseofs + compty->st.fields[idx].offset;
                break;
            }
            case FIELD_IDX:
            {
                vp_assertX(field->idx->kind == EX_INT || field->idx->kind == EX_UINT, "not constexpr");
                uint32_t idx_val = (field->idx->kind == EX_INT) ? field->idx->i : field->idx->u;
                Type* elemty = compty->p;
                uint32_t elemsize = vp_type_sizeof(elemty);
                field_offset = baseofs + (idx_val * elemsize);
                break;
            }
            case FIELD_DEFAULT:
            {
                Type* elemty = compty->p;
                uint32_t elemsize = vp_type_sizeof(elemty);
                field_offset = baseofs + (i * elemsize);
                break;
            }
            default:
                vp_assertX(0, "unknown field kind");
                break;
        }

        /* Check if the field initializer is another compound literal */
        if(field->init->kind == EX_COMPLIT)
        {
            flatten_complit(field->init, field_offset, flat);
        }
        else
        {
            FlatField ff = {
                .offset = field_offset,
                .init = field->init,
                .type = field->init->ty
            };
            vec_push(*flat, ff);
        }
    }
}

/* Generate compound literal */
static VReg* gen_complit(Expr* e)
{
    vp_assertX(e->kind == EX_COMPLIT, "compound literal");
    vp_assertX(e->ty, "missing compound type");

    FrameInfo* fi = vp_frameinfo_new();
    VReg* base = vp_ir_bofs(fi)->dst;

    Slot sl = {.type = e->ty, .fi = fi};
    vec_push(V->fncode->slots, sl);

    gen_memzero(e->ty, base);

    vec_t(FlatField) flat_fields = vec_init(FlatField);
    flatten_complit(e, 0, &flat_fields);

    /* Generate stores for all flattened fields */
    for(uint32_t i = 0; i < vec_len(flat_fields); i++)
    {
        FlatField* ff = &flat_fields[i];

        /* Calculate field address */
        VReg* field_addr;
        if(ff->offset == 0)
        {
            field_addr = base;
        }
        else
        {
            VReg* offset_reg = vp_vreg_ki(ff->offset, VRSize8);
            field_addr = vp_ir_binop(IR_ADD, base, offset_reg, VRSize8, IRF_UNSIGNED);
        }

        VReg* value = gen_expr(ff->init);
        gen_store(field_addr, value, ff->type);
    }

    if(flat_fields)
    {
        vec_free(flat_fields);
    }

    return base;
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
    uint8_t flag = cond_flag(e->ty);
    VReg* vlhs = gen_expr(lhs);
    VReg* vrhs = gen_expr(rhs);
    vp_assertX(ty_isscalar(e->ty), "complex type");
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
            VReg* src2 = vp_vreg_ki(0, vp_vsize(e->ty));
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
    Type* ty = e->ty;
    if(ty_isscalar(ty))
    {
        vr = vp_ir_load(vr, vp_vsize(ty), vp_vflag(ty), ir_flag(ty))->dst;
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
    vp_ir_mov(dst, vp_vreg_ki(true, VRSize1), 0);
    vp_ir_jmp(nbb);
    vp_bb_setcurr(fbb);
    vp_ir_mov(dst, vp_vreg_ki(false, VRSize1), 0);
    vp_bb_setcurr(nbb);
    return dst;
}

typedef VReg* (*GenExprFn)(Expr*);
static const GenExprFn genexprtab[] = {
    [EX_NIL] = gen_nil,
    [EX_TRUE] = gen_true, [EX_FALSE] = gen_false,
    [EX_CHAR] = gen_int,
    [EX_INT] = gen_int, [EX_UINT] = gen_uint,
    [EX_NUM] = gen_num, [EX_FLO] = gen_flo,
    [EX_STR] = gen_str,
    [EX_NAME] = gen_name,
    [EX_ADD] = gen_binop, [EX_SUB] = gen_binop,
    [EX_MUL] = gen_binop, [EX_DIV] = gen_binop, [EX_MOD] = gen_binop,
    [EX_BAND] = gen_binop, [EX_BOR] = gen_binop, [EX_BXOR] = gen_binop,
    [EX_LSHIFT] = gen_binop, [EX_RSHIFT] = gen_binop,
    [EX_PREINC] = gen_incdec, [EX_POSTINC] = gen_incdec,
    [EX_PREDEC] = gen_incdec, [EX_POSTDEC] = gen_incdec,
    [EX_NEG] = gen_unary, [EX_BNOT] = gen_unary, [EX_NOT] = gen_not,
    [EX_EQ] = gen_cond, [EX_NOTEQ] = gen_cond,
    [EX_LE] = gen_cond, [EX_LT] = gen_cond,
    [EX_GE] = gen_cond, [EX_GT] = gen_cond,
    [EX_REF] = gen_addr, [EX_DEREF] = gen_deref,
    [EX_AND] = gen_logical, [EX_OR] = gen_logical,
    [EX_COMPLIT] = gen_complit,
    [EX_FIELD] = gen_field, [EX_IDX] = gen_idx,
    [EX_CALL] = gen_call,
    [EX_CAST] = gen_cast,
    [EX_INTCAST] = gen_cast,
    [EX_FLOATCAST] = gen_cast,
    [EX_PTRCAST] = gen_cast,
    [EX_BITCAST] = gen_bitcast,
};

/* Generate expression */
static VReg* gen_expr(Expr* e)
{
    vp_assertX(e->ty, "missing type");
    vp_assertX(e->kind < (int)ARRSIZE(genexprtab), "out of bounds expression kind");
    vp_assertX(genexprtab[e->kind], "empty entry %d", e->kind);
    return (*genexprtab[e->kind])(e);
}

/* Inline body of a function */
static VReg* gen_inline(Expr* e, Type* ret, Stmt* body, Code* code)
{
    vp_assertX(body->kind == ST_BLOCK, "not a block");

    if(e)
    {
        vp_assertX(e->kind == EX_CALL, "call");
        vp_assertX(vec_len(e->call.args) == code->numparams, "mismatch arguments");
        /* Assign arguments to variables for embedding function arguments */
        for(uint32_t i = 0; i < code->numparams; i++)
        {
            VarInfo* vi = code->scopes[0]->vars[i];
            Expr* arg = e->call.args[i];
            gen_var_assign(vi, arg);
        }
    }

    /* Handle restore of any `return` for inlined functions */
    BB* inlbb = vp_bb_new();
    BB* saveretbb = V->fncode->retbb;
    VReg* saveretvr = V->fncode->retvr;
    V->fncode->retbb = inlbb;

    VReg* dst = NULL;
    /* Inline the body */
    gen_block(body);

    V->fncode->retbb = saveretbb;
    V->fncode->retvr = saveretvr;

    vp_bb_setcurr(inlbb);

    return dst;
}

/* -- Gen statements ------------------------------------------------ */

/* Generate expression statement */
static void gen_expr_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_EXPR, "not an expression statement");
    gen_expr(st->expr);
}

/* Generate compound assignment */
static void gen_comp_assign(Stmt* st)
{
    ExprKind op = st->kind - ST_ADD_ASSIGN + EX_ADD;
    Expr* lhs = st->lhs;
    Expr* rhs = st->rhs;
    if(lhs->kind == EX_NAME && ty_isscalar(lhs->ty))
    {
        VarInfo* vi = lhs->vi;
        if(vs_isloc(vi))
        {
            vp_assertX(vi->vreg, "empty vreg");
            VReg* vlhs = vi->vreg;
            VReg* vrhs = gen_expr(rhs);
            VReg* res = vp_ir_binop(op + (IR_ADD - EX_ADD), vlhs, vrhs, vp_vsize(lhs->ty), ir_flag(lhs->ty));
            vp_ir_mov(vlhs, res, ir_flag(lhs->ty));
            return;
        }
    }

    /* (ptr = &lhs, *ptr = *ptr + rhs) */
    VReg* pvlhs = gen_lval(lhs);
    VReg* lvlhs = vp_ir_load(pvlhs, vp_vsize(lhs->ty), vp_vflag(lhs->ty), ir_flag(lhs->ty))->dst;
    VReg* vrhs = gen_expr(rhs);
    VReg* res = vp_ir_binop(op + (IR_ADD - EX_ADD), lvlhs, vrhs, vp_vsize(lhs->ty), ir_flag(lhs->ty));
    gen_store(pvlhs, res, lhs->ty);
}

/* Generate assignment */
static void gen_assign(Stmt* st)
{
    Expr* lhs = st->lhs;
    Expr* rhs = st->rhs;
    VReg* src = gen_expr(rhs);
    if(lhs->kind == EX_NAME && ty_isscalar(lhs->ty))
    {
        VarInfo* vi = lhs->vi;
        if(vs_isloc(vi))
        {
            vp_assertX(vi->vreg, "empty vreg");
            vp_ir_mov(vi->vreg, src, ir_flag(rhs->ty));
            return;
        }
    }

    VReg* dst = gen_lval(lhs);
    gen_store(dst, src, lhs->ty);
}

/* Generate varinfo and assignment */
static void gen_var_assign(VarInfo* vi, Expr* e)
{
    vp_assertX(vi, "empty variable info");
    if(!gen_varinfo(vi))
    {
        Slot sl = {.type = vi->type, .fi = vi->fi};
        vec_push(V->fncode->slots, sl);
        V->ra->flag |= RAF_STACK_FRAME;
    }

    /* Generate initialization if provided */
    if(e)
    {
        if(ty_isscalar(vi->type))
        {
            vp_assertX(vi->vreg, "empty vreg");
            VReg* src = gen_expr(e);
            vp_ir_mov(vi->vreg, src, ir_flag(vi->type));
        }
        else
        {
            vp_assertX(vi->fi, "missing frame info");
            VReg* dst = vp_ir_bofs(vi->fi)->dst;
            VReg* src = gen_expr(e);
            gen_memcpy(vi->type, dst, src);
        }
    }
    else if(!ty_isscalar(vi->type))
    {
        vp_assertX(vi->fi, "missing frame info");
        VReg* dst = vp_ir_bofs(vi->fi)->dst;
        gen_memzero(vi->type, dst);
    }
}

/* Generate variable declaration */
static void gen_var(Stmt* st)
{
    Decl* d = st->decl;
    if(d->kind == DECL_NOTE) return;
    vp_assertX(d->kind == DECL_VAR || d->kind == DECL_LET, "var/let");

    gen_var_assign(d->var.vi, d->var.expr);
}

/* Generate return statement */
static void gen_ret(Stmt* st)
{
    vp_assertX(st->kind == ST_RETURN, "not return statement");
    BB* bb = vp_bb_new();
    if(st->expr)
    {
        vp_assertX(st->expr->ty, "missing return type");
        Type* ty = st->expr->ty;
        VReg* vreg = gen_expr(st->expr);

        if(ty_isscalar(ty))
        {
            vp_ir_ret(vreg, ir_flag(ty));
        }
        else if(ty != tyvoid)
        {
            VReg* retvr = V->fncode->retvr;
            if(retvr)
            {
                gen_memcpy(ty, retvr, vreg);
                vp_ir_ret(vreg, IRF_UNSIGNED);  /* Pointer is unsigned */
            }
            else
            {
                vp_assertX(0, "?");
            }
        }
    }
    vp_ir_jmp(V->fncode->retbb);
    vp_bb_setcurr(bb);
}

static void gen_stmt(Stmt* st);

static BB* breakbb;
static BB* continuebb;

static BB* bb_push_break(BB** bb)
{
    *bb = breakbb;
    BB* bb1 = vp_bb_new();
    breakbb = bb1;
    return bb1;
}

static BB* bb_push_continue(BB** bb)
{
    *bb = continuebb;
    BB* bb1 = vp_bb_new();
    continuebb = bb1;
    return bb1;
}

static void bb_pop_break(BB* bb)
{
    breakbb = bb;
}

static void bb_pop_continue(BB* bb)
{
    continuebb = bb;
}

/* Generate break statement */
static void gen_break(Stmt* st)
{
    UNUSED(st);
    vp_assertX(breakbb != NULL, "missing loop");
    BB* bb = vp_bb_new();
    vp_ir_jmp(breakbb);
    vp_bb_setcurr(bb);
}

/* Generate continue statement */
static void gen_continue(Stmt* st)
{
    UNUSED(st);
    vp_assertX(continuebb != NULL, "missing loop");
    BB* bb = vp_bb_new();
    vp_ir_jmp(continuebb);
    vp_bb_setcurr(bb);
}

/* Generate block of statements */
static void gen_block(Stmt* st)
{
    vec_t(Stmt*) block = st->block;
    for(uint32_t i = 0; i < vec_len(block); i++)
    {
        gen_stmt(block[i]);
    }
}

/* Generate if/else if/else statement */
static void gen_if_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_IF, "not an if statement");

    BB* tbb = vp_bb_new();
    BB* fbb = vp_bb_new();

    gen_cond_jmp(st->ifst.cond, tbb, fbb);
    vp_bb_setcurr(tbb);
    gen_stmt(st->ifst.tblock);

    if(st->ifst.fblock == NULL)
    {
        vp_bb_setcurr(fbb);
    }
    else
    {
        BB* nbb = vp_bb_new();
        vp_ir_jmp(nbb);
        vp_bb_setcurr(fbb);
        gen_stmt(st->ifst.fblock);
        vp_bb_setcurr(nbb);
    }
}

/* Generate for statement */
static void gen_for_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_FOR, "not a for statement");

    BB* breakbb1, *contbb1;
    BB* loopbb = vp_bb_new();
    BB* contbb = bb_push_continue(&contbb1);
    BB* condbb = vp_bb_new();
    BB* exitbb = bb_push_break(&breakbb1);

    gen_stmt(st->forst.init);

    vp_ir_jmp(condbb);

    vp_bb_setcurr(loopbb);
    gen_stmt(st->forst.body);

    vp_bb_setcurr(contbb);
    gen_stmt(st->forst.next);

    /* Set up the condition block */
    vp_bb_setcurr(condbb);
    gen_cond_jmp(st->forst.cond, loopbb, exitbb);

    vp_bb_setcurr(exitbb);
    bb_pop_continue(contbb1);
    bb_pop_break(breakbb1);
}

/* Generate while statement */
static void gen_while_stmt(Stmt* st)
{
    vp_assertX(st->kind == ST_WHILE, "not a while statement");

    BB* breakbb1, *contbb1;
    BB* loopbb = vp_bb_new();
    BB* condbb = bb_push_continue(&contbb1);
    BB* exitbb = bb_push_break(&breakbb1);

    vp_ir_jmp(condbb);

    vp_bb_setcurr(loopbb);
    gen_stmt(st->whst.body);

    /* Set up the condition block */
    vp_bb_setcurr(condbb);
    gen_cond_jmp(st->whst.cond, loopbb, exitbb);

    vp_bb_setcurr(exitbb);
    bb_pop_continue(contbb1);
    bb_pop_break(breakbb1);
}

/* Generate asm block statement */
static void gen_asm(Stmt* st)
{
    vp_assertX(st->kind == ST_ASM, "not asm statement");
    for(uint32_t i = 0; i < vec_len(st->asm_.insts); i++)
    {
        Inst* inst = st->asm_.insts[i];
        vp_ir_asm(inst);
    }
}

typedef void (*GenStmtFn)(Stmt*);
static const GenStmtFn gensttab[] = {
    [ST_DECL] = gen_var,
    [ST_BLOCK] = gen_block,
    [ST_ASSIGN] = gen_assign,
    [ST_ADD_ASSIGN] = gen_comp_assign, [ST_SUB_ASSIGN] = gen_comp_assign,
    [ST_MUL_ASSIGN] = gen_comp_assign, [ST_DIV_ASSIGN] = gen_comp_assign,
    [ST_MOD_ASSIGN] = gen_comp_assign,
    [ST_BAND_ASSIGN] = gen_comp_assign, [ST_BOR_ASSIGN] = gen_comp_assign,
    [ST_BXOR_ASSIGN] = gen_comp_assign,
    [ST_LSHIFT_ASSIGN] = gen_comp_assign, [ST_RSHIFT_ASSIGN] = gen_comp_assign,
    [ST_EXPR] = gen_expr_stmt,
    [ST_IF] = gen_if_stmt,
    [ST_FOR] = gen_for_stmt,
    [ST_WHILE] = gen_while_stmt,
    [ST_RETURN] = gen_ret,
    [ST_BREAK] = gen_break,
    [ST_CONTINUE] = gen_continue,
    [ST_ASM] = gen_asm,
};

/* Generate a statement */
static void gen_stmt(Stmt* st)
{
    vp_assertX(st->kind < (int)ARRSIZE(gensttab), "out of bounds statement kind");
    vp_assertX(gensttab[st->kind], "empty entry %d", st->kind);
    (*gensttab[st->kind])(st);
}

/* Set stack offsets for spills/slots */
static void gen_stack(Code* code)
{
    vp_assertX(code->framesize == 0, "?");
    uint32_t framesize = 0;

    /* Allocate stack frame */
    for(uint32_t i = 0; i < vec_len(code->slots); i++)
    {
        Slot* slot = &code->slots[i];
        FrameInfo* fi = slot->fi;
        Type* type = slot->type;

        uint32_t size = vp_type_sizeof(type);
        uint32_t align = vp_type_alignof(type);
        if(size < 1) size = 1;
        framesize = ALIGN_UP(framesize + size, align);
        fi->ofs = -(int32_t)framesize;
    }

    /* Allocate spilled variables onto stack frame */
    RegAlloc* ra = code->ra;
    for(uint32_t i = 0; i < vec_len(ra->vregs); i++)
    {
        LiveInterval* li = ra->sorted[i];
        if(li->state != LI_SPILL)
            continue;
        VReg* vr = ra->vregs[li->virt];
        vp_assertX(vrf_spill(vr), "not spilled vreg?");
        if(vr->flag & VRF_STACK_PARAM)
            continue;

        uint32_t size, align;
        size = align = 1 << vr->vsize;

        framesize = ALIGN_UP(framesize + size, align);
        vr->fi.ofs = -(int32_t)framesize;
    }

    code->framesize = framesize;
}

/* Generate function parameters */
static void gen_params(Decl* d, Code* code)
{
    Type* ret = d->fn.rett;
    const ABIInfo* abi = code->abi;
    uint32_t paramofs = TARGET_PTR_SIZE * 2;
    uint32_t iidx = 0, fidx = 0;

    /* Classify return value */
    if(abi_isstack(ret))
    {
        VReg* vr = vp_vreg_new(vp_type_ptr(ret));
        vr->flag |= VRF_STACK_PARAM;
        vr->param = REG_NO;
        code->retvr = vr;

        ParamLoc pl = {.cls = PC_IREG, .idx = abi->imap[0]};
        vec_push(code->plocs, pl);
        iidx = 1;

        /* Stack parameter offset */
        paramofs = ALIGN_UP(paramofs, TARGET_PTR_SIZE);
        paramofs += TARGET_PTR_SIZE;
    }

    /* Classify each parameter */
    for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
    {
        VarInfo* vi = d->fn.scopes[0]->vars[i];
        Type* ty = vi->type;
        uint32_t previidx = iidx, prevfidx = fidx;
        ParamClass cls = abi_classify(ty, abi, &iidx, &fidx);
        ParamLoc pl = {.cls = cls};
        VReg* vr = gen_varinfo(vi);
        switch(cls)
        {
            case PC_IREG:
                vp_assertX(vr, "not scalar param");
                vr->flag |= VRF_PARAM;
                vr->param = pl.idx = abi->imap[previidx];
                break;
            case PC_FREG:
                vp_assertX(vr, "not scalar param");
                vr->flag |= VRF_PARAM;
                vr->param = pl.idx = abi->fmap[prevfidx];
                break;
            case PC_STACK:
                vp_assertX(vr, "scalar param needs vreg");
                vr->flag |= VRF_STACK_PARAM;
                vreg_spill(vr);
                V->ra->flag = RAF_STACK_FRAME;
                /* Stack parameter offset */
                pl.idx = vr->fi.ofs = paramofs = ALIGN_UP(paramofs, TARGET_PTR_SIZE);
                paramofs += TARGET_PTR_SIZE;
                break;
            case PC_MEM:
            {
                vp_assertX(vi->fi, "aggregate param needs frame info");
                uint32_t size = vp_type_sizeof(ty);
                uint32_t align = vp_type_alignof(ty);
                V->ra->flag = RAF_STACK_FRAME;
                /* Stack parameter offset */
                pl.idx = vi->fi->ofs = paramofs = ALIGN_UP(paramofs, align);
                paramofs += ALIGN_UP(size, TARGET_PTR_SIZE);
                break;
            }
        }

        vec_push(code->plocs, pl);
    }
}

static const ABIInfo syscall_abi = {
    .imap = (uint32_t[]){RN_DI, RN_SI, RN_DX, RN_10, RN_8, RN_9},
    .fmap = NULL,
    .imax = 6,
    .fmax = 0,
};

/* Generate function body, if present */
static Code* gen_fn(Decl* d)
{
    vp_assertX(d->kind == DECL_FN, "not function declaration");

    Code* code = vp_mem_calloc(1, sizeof(*code));
    code->name = d->name;
    code->bbs = NULL;
    code->calls = vec_init(IR*);
    code->slots = vec_init(Slot);
    code->plocs = vec_init(ParamLoc);
    code->numparams = vec_len(d->fn.params);
    code->scopes = d->fn.scopes;
    code->flags = d->fn.flags;
    code->body = d->fn.body;
    code->abi = V->T->abi;
    if(code->flags & FN_SYSCALL)
    {
        code->abi = &syscall_abi;
    }
    vp_tab_set(&V->funcs, code->name, code);

    if(!d->fn.body)
    {
        return code;
    }

    RegAlloc* ra = V->ra = vp_ra_new((TargetInfo*)V->T);
    code->ra = ra;
    if(code->flags & FN_INLINE)
    {
        return code;
    }
    code->bbs = vec_init(BB*);
    V->fncode = code;

    vp_bb_setcurr(vp_bb_new());

    if(d->fn.params)
    {
        gen_params(d, code);
    }

    V->fncode->retbb = vp_bb_new();

    gen_block(d->fn.body);

    vp_bb_setcurr(V->fncode->retbb);
    V->bb = NULL;

    vp_bb_detect(code->bbs);
    vp_opt(code);

    vp_irX64_tweak(code);
    vp_bb_analyze(code->bbs);
    vp_ra_alloc(ra, code->bbs);
    gen_stack(code);

    vp_dump_bbs(code);

    V->fncode = NULL;
    V->ra = NULL;

    return code;
}

static VP_AINLINE void write_int(uint8_t* data, uint32_t offset, uint64_t val, uint32_t size)
{
    for(uint32_t i = 0; i < size; i++)
    {
        data[offset + i] = (val >> (i * 8)) & 0xFF;
    }
}

static void glob_eval(Type* ty, Expr* e, DataEntry* entry, uint32_t baseofs, bool isref)
{
    switch(e->kind)
    {
        case EX_NIL:
        {
            write_int(entry->data, baseofs, 0, vp_type_sizeof(ty));
            break;
        }
        case EX_TRUE:
        case EX_FALSE:
        {
            write_int(entry->data, baseofs, e->b, vp_type_sizeof(ty));
            break;
        }
        case EX_NUM:
        {
            union { double d; uint64_t u; } conv = {.d = e->n};
            write_int(entry->data, baseofs, conv.u, vp_type_sizeof(ty));
            break;
        }
        case EX_FLO:
        {
            union { float f; uint32_t u; } conv = {.f = e->f};
            write_int(entry->data, baseofs, conv.u, vp_type_sizeof(ty));
            break;
        }
        case EX_UINT:
        case EX_INT:
        {
            write_int(entry->data, baseofs, e->i, vp_type_sizeof(ty));
            break;
        }
        case EX_REF:
        {
            glob_eval(ty, e->unary, entry, baseofs, true);
            break;
        }
        case EX_NAME:
        {
            VarInfo* vi = e->vi;
            vp_assertX(vi, "name not found");
            if(isref || ty_isfn(e->ty))
            {
                Reloc rel = {
                    .entry = entry,
                    .ofs = baseofs,
                    .sym = e->name
                };
                vec_push(V->relocs, rel);
                write_int(entry->data, baseofs, 0, TARGET_PTR_SIZE);
            }
            break;
        }
        case EX_STR:
        {
            Str* s = e->str;
            if(ty_isarr(ty))
            {
                memcpy(entry->data + baseofs, str_data(s), s->len);
            }
            else
            {
                vp_assertX(ty_isptr(ty), "?");
                Str* sym = str_data_get(s);
                Reloc rel = {
                    .entry = entry,
                    .ofs = baseofs,
                    .sym = sym,
                    .isfn = false
                };
                vec_push(V->relocs, rel);
                write_int(entry->data, baseofs, 0, TARGET_PTR_SIZE);
            }
            break;
        }
        case EX_COMPLIT:
        {
            vec_t(FlatField) flat = vec_init(FlatField);
            flatten_complit(e, 0, &flat);
            if(isref)
            {
                Str* anon = vp_anon_label();

                uint32_t size = vp_type_sizeof(e->ty);
                DataEntry* newentry = vp_mem_calloc(1, sizeof(DataEntry));
                newentry->kind = DATA_ANON;
                newentry->name = anon;
                newentry->ofs = 0;
                newentry->size = size;
                newentry->align = vp_type_alignof(e->ty);
                newentry->data = vp_mem_calloc(1, size);

                Reloc rel = {
                    .entry = entry,
                    .ofs = baseofs,
                    .sym = anon,
                };
                vec_push(V->relocs, rel);

                for(uint32_t i = 0; i < vec_len(flat); i++)
                {
                    FlatField* ff = &flat[i];
                    glob_eval(ff->type, ff->init, newentry, ff->offset, false);
                }
                vec_push(V->globdata, newentry);

                write_int(entry->data, baseofs, 0, TARGET_PTR_SIZE);
                break;
            }
            for(uint32_t i = 0; i < vec_len(flat); i++)
            {
                FlatField* ff = &flat[i];
                glob_eval(ff->type, ff->init, entry, baseofs + ff->offset, false);
            }
            break;
        }
        default:
            vp_assertX(0, "bad initializer %d\n", e->kind);
            break;
    }
}

static void gen_glob(Decl* d)
{
    vp_assertX(d->kind == DECL_VAR || d->kind == DECL_LET, "not a var/let");

    Type* ty = d->var.vi->type;
    uint32_t size = vp_type_sizeof(ty);
    uint32_t align = vp_type_alignof(ty);

    uint8_t* data = vp_mem_calloc(1, size);
    DataEntry* entry = vp_mem_calloc(1, sizeof(DataEntry));
    entry->kind = DATA_VAR;
    entry->name = d->name;
    entry->ofs = 0;
    entry->size = size;
    entry->align = align;
    entry->data = data;
    vec_push(V->globdata, entry);

    if(d->var.expr)
    {
        glob_eval(ty, d->var.expr, entry, 0, false);
    }
}

/* Generate code IR for all declarations */
vec_t(Code*) vp_codegen(vec_t(Decl*) decls)
{
    vec_t(Code*) codes = vec_init(Code*);
    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        Decl* d = decls[i];
        if(d && d->kind == DECL_FN)
        {
            Code* code = gen_fn(d);
            vec_push(codes, code);
        }
        else if(d && (d->kind == DECL_VAR || d->kind == DECL_LET))
        {
            gen_glob(d);
        }
    }
    return codes;
}