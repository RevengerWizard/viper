/*
** vp_sema.c
** Semantic analyser
*/

#include <string.h>

#include "vp_sema.h"
#include "vp_ast.h"
#include "vp_def.h"
#include "vp_lex.h"
#include "vp_mem.h"
#include "vp_err.h"
#include "vp_str.h"
#include "vp_tab.h"
#include "vp_type.h"
#include "vp_var.h"
#include "vp_vec.h"

#include "vp_dump.h"

/* Semantic state */
typedef struct
{
    vec_t(Decl*) sorted;
    Tab globsyms;
    vec_t(Sym*) syms;
    vec_t(Sym*) localsyms;
    Decl* currfn;
} SemaState;

SemaState S;

/* -- Symbols ------------------------------------------------------- */

static Sym* sym_new(SymKind kind, Str* name, Decl* d)
{
    Sym* sym = vp_arena_alloc(&V->symarena, sizeof(*sym));
    memset(sym, 0, sizeof(*sym));
    sym->kind = kind;
    sym->name = name;
    sym->decl = d;
    return sym;
}

/* Add symbol to local scope */
static void sym_add(Str* name, Type* ty)
{
    Sym* sym = sym_new(SYM_VAR, name, NULL);
    sym->state = SYM_DONE;
    sym->type = ty;
    vec_push(S.localsyms, sym);
}

/* Enter symbol scope */
static uint32_t sym_enter()
{
    return vec_len(S.localsyms);
}

/* Leave symbol scope */
static void sym_leave(uint32_t len)
{
    if(S.localsyms)
    {
        vec_hdr(S.localsyms)->len = len;
    }
}

/* Add a global symbol */
static void sym_glob_put(Sym* sym)
{
    vp_tab_set(&S.globsyms, sym->name, sym);
    vec_push(S.syms, sym);
}

static Sym* sym_find(Str* name)
{
    uint32_t len = vec_len(S.localsyms);
    for(uint32_t i = len; i > 0; i--)
    {
        Sym* sym = S.localsyms[i - 1];
        if(sym->name == name)
            return sym;
    }
    return vp_tab_get(&S.globsyms, name);
}

static void sema_resolve(Sym* sym);
static Type* sema_typespec(TypeSpec* spec);
static void sema_enum(Decl* d, Sym* sym);

/* Find and resolve a local/global symbol */
static Sym* sym_name(Str* name)
{
    Sym* sym = sym_find(name);
    if(!sym)
        return NULL;
    sema_resolve(sym);
    return sym;
}

/* Create symbols for top-level declarations */
static Sym* sym_decl(Decl* d)
{
    SymKind kind = SYM_NONE;
    switch(d->kind)
    {
        case DECL_TYPE:
        case DECL_STRUCT:
        case DECL_UNION:
            kind = SYM_TYPE;
            break;
        case DECL_ENUM:
            kind = SYM_ENUM;
            break;
        case DECL_FN:
            kind = SYM_FN;
            break;
        case DECL_VAR:
            kind = SYM_VAR;
            break;
        default:
            vp_assertX(0, "unknown symbol declaration");
            break;
    }
    Sym* sym = sym_new(kind, d->name, d);
    if(d->kind == DECL_STRUCT || d->kind == DECL_UNION)
    {
        sym->state = SYM_DONE;
        sym->type = vp_type_none(sym);
    }
    return sym;
}

/* Complete a symbolic type */
static void sym_complete(Type* ty)
{
    if(ty->kind == TY_name)
    {
        vp_err_error(ty->sym->decl->loc, "type completion cycle");
    }
    else if(ty->kind != TY_none)
        return;

    Decl* d = ty->sym->decl;
    ty->kind = TY_name;
    vp_assertX(d->kind == DECL_STRUCT || d->kind == DECL_UNION, "struct/union");

    vec_t(TypeField) fields = NULL;
    for(uint32_t i = 0; i < vec_len(d->agr->items); i++)
    {
        AggregateItem* item = &d->agr->items[i];
        Type* tyitem = sema_typespec(item->type);
        tyitem = vp_type_decayempty(tyitem);
        sym_complete(tyitem);
        if(vp_type_sizeof(tyitem) == 0)
        {
            if(tyitem->kind != TY_array || vp_type_sizeof(tyitem->p) == 0)
            {
                vp_err_error(item->loc, "field type of size 0 is not allowed");
            }
        }
        for(uint32_t j = 0; j < vec_len(item->names); j++)
        {
            vec_push(fields, (TypeField){item->names[j], tyitem, 0});
        }
    }
    if(vec_len(fields) == 0)
    {
        vp_err_error(d->loc, "no fields");
    }
    if(vp_type_isdupfield(fields))
    {
        vp_err_error(d->loc, "duplicate fields");
    }
    if(d->kind == DECL_STRUCT)
    {
        vp_type_struct(d->name, ty, fields);
    }
    else
    {
        vp_assertX(d->kind == DECL_UNION, "union");
        vp_type_union(d->name, ty, fields);
    }
}

/* -- Constant folding ---------------------------------------------- */

/* Fold unary operator to i64 */
static int64_t fold_unary_i64(ExprKind op, int64_t val)
{
    switch(op)
    {
        case EX_NEG: return -val;
        case EX_BNOT: return ~val;
        case EX_NOT: return !val;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

/* Fold unary operator to u64 */
static uint64_t fold_unary_u64(ExprKind op, uint64_t val)
{
    switch(op)
    {
        case EX_NEG: return -val;
        case EX_BNOT: return ~val;
        case EX_NOT: return !val;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

/* Fold unary operator to float (double) */
static double fold_unary_f(ExprKind op, double val)
{
    switch(op)
    {
        case EX_NEG: return -val;
        case EX_NOT: return !val;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

/* Fold binary operator to i64 */
static int64_t fold_binop_i64(ExprKind op, int64_t l, int64_t r)
{
    switch(op)
    {
        case EX_ADD: return l + r;
        case EX_SUB: return l - r;
        case EX_MUL: return l * r;
        case EX_DIV: return r != 0 ? l / r : 0;
        case EX_MOD: return r != 0 ? l % r : 0;
        case EX_BAND: return l & r;
        case EX_BOR: return l | r;
        case EX_BXOR: return l ^ r;
        case EX_LSHIFT: return l << r;
        case EX_RSHIFT: return l >> r;
        case EX_EQ: return l == r;
        case EX_NOTEQ: return l != r;
        case EX_LT: return l < r;
        case EX_LE: return l <= r;
        case EX_GT: return l > r;
        case EX_GE: return l >= r;
        case EX_AND: return l && r;
        case EX_OR: return l || r;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

/* Fold binary operator to u64 */
static uint64_t fold_binop_u64(ExprKind op, uint64_t l, uint64_t r)
{
    switch(op)
    {
        case EX_ADD: return l + r;
        case EX_SUB: return l - r;
        case EX_MUL: return l * r;
        case EX_DIV: return r != 0 ? l / r : 0;
        case EX_MOD: return r != 0 ? l % r : 0;
        case EX_BAND: return l & r;
        case EX_BOR: return l | r;
        case EX_BXOR: return l ^ r;
        case EX_LSHIFT: return l << r;
        case EX_RSHIFT: return l >> r;
        case EX_EQ: return l == r;
        case EX_NOTEQ: return l != r;
        case EX_LT: return l < r;
        case EX_LE: return l <= r;
        case EX_GT: return l > r;
        case EX_GE: return l >= r;
        case EX_AND: return l && r;
        case EX_OR: return l || r;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

/* Fold binary operator to float (double) */
static double fold_binop_f(ExprKind op, double l, double r)
{
    switch(op)
    {
        case EX_ADD: return l + r;
        case EX_SUB: return l - r;
        case EX_MUL: return l * r;
        case EX_DIV: return r == 0 ? 0 : l / r;
        case EX_EQ: return l == r;
        case EX_NOTEQ: return l != r;
        case EX_LT: return l < r;
        case EX_LE: return l <= r;
        case EX_GT: return l > r;
        case EX_GE: return l >= r;
        default:
            vp_assertX(0, "%s", ast_binname(op));
            break;
    }
    return 0;
}

/* -- Operand types ------------------------------------------------- */

/* Literal value operand */
static Operand opr_lit(Type* ty, Val val, bool untyped)
{
    return (Operand){.ty = ty, .val = val, .islit = true, .isconst = true, .untyped = untyped};
}

/* Constant value operand */
static Operand opr_const(Type* ty, Val val)
{
    return (Operand){.ty = ty, .val = val, .isconst = true};
}

/* lvalue operand */
static Operand opr_lval(Type* ty)
{
    return (Operand){.ty = ty, .islval = true};
}

/* rvalue operand */
static Operand opr_rval(Type* ty)
{
    return (Operand){.ty = ty};
}

/* Decay operand */
static Operand opr_decay(Operand opr)
{
    opr.ty = vp_type_decay(opr.ty);
    opr.islval = false;
    return opr;
}

#define CASE(k, t) \
    case k: \
        switch(ty->kind) \
        { \
        case TY_bool: \
            opr->val.b = (bool)opr->val.b; \
            break; \
        case TY_uint8: \
            opr->val.u8 = (uint8_t)opr->val.t; \
            break; \
        case TY_int8: \
            opr->val.i8 = (int8_t)opr->val.t; \
            break; \
        case TY_uint16: \
            opr->val.u16 = (uint16_t)opr->val.t; \
            break; \
        case TY_int16: \
            opr->val.i16 = (int16_t)opr->val.t; \
            break; \
        case TY_uint32: \
            opr->val.u32 = (uint32_t)opr->val.t; \
            break; \
        case TY_int32: \
            opr->val.i32 = (int32_t)opr->val.t; \
            break; \
        case TY_uint64: \
            opr->val.u64 = (uint64_t)opr->val.t; \
            break; \
        case TY_int64: \
            opr->val.i64 = (int64_t)opr->val.t; \
            break; \
        case TY_float: \
            opr->val.f = (float)opr->val.t; \
            break; \
        case TY_double: \
            opr->val.d = (double)opr->val.t; \
            break; \
        default: \
            opr->isconst = false; \
            break; \
        } \
        break;

/* Explicit operand type casting */
static bool opr_cast(Operand* opr, Type* ty)
{
    if(opr->ty == ty)
        return true;

    if(!vp_type_iscast(ty, opr->ty))
        return false;

    if(opr->isconst)
    {
        switch(opr->ty->kind)
        {
        CASE(TY_bool, b)
        CASE(TY_uint8, u8)
        CASE(TY_int8, i8)
        CASE(TY_uint16, u16)
        CASE(TY_int16, i16)
        CASE(TY_uint32, u32)
        CASE(TY_int32, i32)
        CASE(TY_uint64, u64)
        CASE(TY_int64, i64)
        CASE(TY_float, f)
        CASE(TY_double, d)
        default:
            opr->isconst = false;
            break;
        }
    }
    opr->ty = ty;
    return true;
}

#undef CASE

/* Implicit operand type conversion */
static void opr_conv(SrcLoc loc, Operand* opr, Type* ty)
{
    if(!vp_type_isconv(ty, opr->ty))
    {
        vp_err_error(loc, "implicit %s to %s", type_name(opr->ty), type_name(ty));
    }
    opr_cast(opr, ty);
    opr->islval = false;
}

/* Check for overflow of integer literals, signed or unsigned */
static bool val_overflow(uint64_t v, Type* ty)
{
    switch(ty->kind)
    {
        /* Unsigned */
        case TY_uint8: return v > UINT8_MAX;
        case TY_uint16: return v > UINT16_MAX;
        case TY_uint32: return v > UINT32_MAX;
        case TY_uint64: return false;
        /* Signed */
        case TY_int8: return ((int64_t)v) < INT8_MIN  || ((int64_t)v) > INT8_MAX;
        case TY_int16: return ((int64_t)v) < INT16_MIN || ((int64_t)v) > INT16_MAX;
        case TY_int32: return ((int64_t)v) < INT32_MIN || ((int64_t)v) > INT32_MAX;
        case TY_int64: return false;
        default: vp_assertX(0, "not an int"); return false;
    }
}

/* Get value from type, overflow checking */
static Val val_fromtype(SrcLoc loc, Type* ret, uint64_t u64)
{
    Val val;
    if(ty_isint(ret))
    {
        if(val_overflow(u64, ret))
        {
            vp_err_error(loc, "literal out of range for '%s'", type_name(ret));
        }
        if(ty_isunsigned(ret))
        {
            val = (Val){.u64 = u64};
        }
        else
        {
            val = (Val){.i64 = u64};
        }
    }
    else
    {
        if(ret->kind == TY_double)
        {
            val = (Val){.d = u64};
        }
        else
        {
            vp_assertX(ret->kind == TY_float, "float");
            val = (Val){.f = u64};
        }
    }
    return val;
}

#define CHECKOVF(op, castty, tt) \
    if(op == EX_ADD) \
    { \
        castty res; \
        of = __builtin_add_overflow((castty)lval.tt, (castty)rval.tt, &res); \
    } \
    else if(op == EX_SUB) \
    { \
        castty res; \
        of = __builtin_sub_overflow((castty)lval.tt, (castty)rval.tt, &res); \
    } \
    else if(op == EX_MUL) \
    { \
        castty res; \
        of = __builtin_mul_overflow((castty)lval.tt, (castty)rval.tt, &res); \
    }

/* Detect binary operator overflow */
static void val_binop_overflow(SrcLoc loc, ExprKind op, Type* ty, Val lval, Val rval)
{
    bool of = false;
    switch(ty->kind)
    {
        case TY_uint8:
            CHECKOVF(op, uint8_t, u8)
            break;
        case TY_int8:
            CHECKOVF(op, int8_t, i8)
            break;
        case TY_uint16:
            CHECKOVF(op, uint16_t, u16)
            break;
        case TY_int16:
            CHECKOVF(op, int16_t, i16)
            break;
        case TY_uint32:
            CHECKOVF(op, uint32_t, u32)
            break;
        case TY_int32:
            CHECKOVF(op, int32_t, i32)
            break;
        case TY_uint64:
            CHECKOVF(op, uint64_t, u64)
            break;
        case TY_int64:
            CHECKOVF(op, int64_t, i64)
            break;
        default:
            vp_assertX(0, "not an int");
            break;
    }
    if ((op == EX_DIV || op == EX_MOD) &&
    ty_issigned(ty) &&
    ((ty->kind == TY_int32 && lval.i32 == INT32_MIN && rval.i32 == -1) ||
     (ty->kind == TY_int64 && lval.i64 == INT64_MIN && rval.i64 == -1)))
    {
        of = true;
    }
    if((op == EX_LSHIFT || op == EX_RSHIFT) && rval.u64 >= (vp_type_sizeof(ty) * 8))
    {
        of = true;
    }
    if(of)
    {
        vp_err_error(loc, "arithmetic operation overflow of '%s'", ast_binname(op));
    }
}

#undef CHECKOVF

/* Value unary operator folding */
static Val val_unary(Expr* e, Type* ty, Val val)
{
    ExprKind op = e->kind;
    Operand res = opr_const(ty, val);
    opr_cast(&res, ty);
    if(ty_isbool(ty))
    {
        vp_assertX(e->kind == EX_NOT, "fold ! bool");
        res.val.b = !res.val.b;
    }
    else if(ty_isunsigned(ty))
    {
        if(op == EX_NEG)
        {
            vp_err_error(e->loc, "cannot apply '-' to type '%s'", type_name(ty));
        }
        res.val.u64 = fold_unary_u64(op, res.val.u64);
    }
    else if(ty_issigned(ty))
    {
        res.val.i64 = fold_unary_i64(op, res.val.i64);
    }
    else if(ty_isflo(ty))
    {
        if(ty->kind == TY_double)
        {
            res.val.d = fold_unary_f(op, res.val.d);
        }
        else
        {
            vp_assertX(ty->kind == TY_float, "float");
            res.val.f = fold_unary_f(op, res.val.f);
        }
    }
    else
    {
        vp_assertX(0, "unknown type to fold");
    }
    opr_cast(&res, ty);
    return res.val;
}

/* Value of binary operator folding */
static Val val_binop(SrcLoc loc, ExprKind op, Type* ty, Val lval, Val rval)
{
    Operand lop = opr_const(ty, lval);
    Operand rop = opr_const(ty, rval);
    Operand res;
    opr_cast(&lop, ty);
    opr_cast(&rop, ty);
    if(ty_isunsigned(ty))
    {
        val_binop_overflow(loc, op, ty, lop.val, rop.val);
        uint64_t u64 = fold_binop_u64(op, lop.val.u64, rop.val.u64);
        res = opr_const(ty, (Val){.u64 = u64});
    }
    else if(ty_issigned(ty))
    {
        val_binop_overflow(loc, op, ty, lop.val, rop.val);
        int64_t i64 = fold_binop_i64(op, lop.val.i64, rop.val.i64);
        res = opr_const(ty, (Val){.i64 = i64});
    }
    else if(ty_isflo(ty))
    {
        if(ty->kind == TY_double)
        {
            double d = fold_binop_f(op, lop.val.d, rop.val.d);
            res = opr_const(ty, (Val){.d = d});
        }
        else
        {
            vp_assertX(ty->kind == TY_float, "float");
            float f = fold_binop_f(op, lop.val.f, rop.val.f);
            res = opr_const(ty, (Val){.f = f});
        }
    }
    else
    {
        vp_assertX(0, "unknown type to fold");
    }
    opr_cast(&res, ty);
    return res.val;
}

/* Apply constant folding to the AST */
static void opr_fold(SrcLoc loc, Expr** e, Operand opr)
{
    Type* ty = opr.ty;
    if(opr.isconst)
    {
        if(opr.islit)
        {
            if(opr.untyped)
            {
                opr.untyped = false;
                opr.val = val_fromtype(loc, opr.ty, opr.val.u64);
            }
            return;
        }
        if(ty_isbool(ty))
        {
            *e = opr.val.b ? vp_expr_true(loc) : vp_expr_false(loc);
        }
        else if(ty_isint(ty))
        {
            if(ty_issigned(ty))
            {
                *e = vp_expr_ilit(loc, opr.val.i64);
            }
            else
            {
                *e = vp_expr_ulit(loc, opr.val.u64);
            }
        }
        else if(ty_isflo(ty))
        {
            if(ty->kind == TY_double)
            {
                *e = vp_expr_nlit(loc, opr.val.d);
            }
            else
            {
                vp_assertX(ty->kind == TY_float, "float");
                *e = vp_expr_flit(loc, opr.val.f);
            }
        }
        (*e)->ty = ty;
    }
}

static Operand opr_unary(Expr* e, Operand opr)
{
    if(opr.untyped)
    {
        opr.untyped = false;
        opr.val = val_fromtype(e->loc, opr.ty, opr.val.u64);
    }
    if(opr.isconst)
    {
        return opr_const(opr.ty, val_unary(e, opr.ty, opr.val));
    }
    else
    {
        return opr;
    }
}

static Operand opr_binop(SrcLoc loc, ExprKind op, Operand lop, Operand rop, Type* ret)
{
    if(lop.untyped && !rop.untyped)
    {
        /* Left is untyped, right is typed */
        lop.ty = rop.ty;
        lop.untyped = false;
        lop.val = val_fromtype(loc, rop.ty, lop.val.u64);
    }
    else if(rop.untyped && !lop.untyped)
    {
        /* Right is untyped, left is typed */
        rop.ty = lop.ty;
        rop.untyped = false;
        rop.val = val_fromtype(loc, lop.ty, rop.val.u64);
    }
    else if(lop.untyped && rop.untyped)
    {
        lop.untyped = false;
        rop.untyped = false;
        lop.val = val_fromtype(loc, lop.ty, lop.val.u64);
        rop.val = val_fromtype(loc, rop.ty, rop.val.u64);
    }

    /* Unify operands of different types */
    if(!ret)
    {
        ret = vp_type_common(lop.ty, rop.ty);
    }
    opr_conv(loc, &lop, ret);
    opr_conv(loc, &rop, ret);

    if(lop.isconst && rop.isconst)
    {
        return opr_const(ret, val_binop(loc, op, ret, lop.val, rop.val));
    }
    else
    {
        return opr_rval(ret);
    }
}

/* -- Expression semantics ------------------------------------------ */

/* Forward declarations */
static Operand sema_expr(Expr* e, Type* ret);

/* Resolve rval expression */
static Operand sema_expr_rval(Expr* e, Type* ret)
{
    return opr_decay(sema_expr(e, ret));
}

/* Resolve constant expression */
static Operand sema_constexpr(Expr* e, Type* ret)
{
    Operand res = sema_expr(e, ret);
    if(!res.isconst)
    {
        vp_err_error(e->loc, "expected constant expression");
    }
    return res;
}

/* Resolve unary expression */
static Operand sema_expr_unary(Expr* e, Type* ret)
{
    Operand opr = sema_expr_rval(e->unary, ret);
    Type* ty = opr.ty;
    switch(e->kind)
    {
        case EX_DEREF:
            if(!ty_isptr(ty))
            {
                vp_err_error(e->loc, "cannot deref non-pointer type");
            }
            return opr_lval(ty->p);
        case EX_NOT:
            if(!ty_isscalar(ty))
            {
                vp_err_error(e->loc, "can only use 'not' with scalar types");
            }
            return opr_unary(e, opr);
        case EX_NEG:
            if(!ty_isnum(ty))
            {
                vp_err_error(e->loc, "can only use unary '-' with arithmetic types");
            }
            return opr_unary(e, opr);
        case EX_BNOT:
            if(!ty_isint(ty))
            {
                vp_err_error(e->loc, "can only use '~' with integer types");
            }
            return opr_unary(e, opr);
        default:
            vp_assertX(0, "?");
            break;
    }
    return (Operand){};
}

/* Resolve binary expression operator */
static Operand sema_expr_binop(SrcLoc loc, ExprKind op, Operand lop, Operand rop, Type* ret)
{
    const char* opname = ast_binname(op);
    switch(op)
    {
        case EX_ADD:
            if(ty_isnum(lop.ty) && ty_isnum(rop.ty))
            {
                return opr_binop(loc, op, lop, rop, ret);
            }
            else if(ty_isptr(lop.ty) && ty_isint(rop.ty))
            {
                sym_complete(lop.ty->p);
                return opr_rval(lop.ty);
            }
            else if(ty_isint(lop.ty) && ty_isptr(rop.ty))
            {
                sym_complete(rop.ty->p);
                return opr_rval(rop.ty);
            }
            else
            {
                vp_err_error(loc, "operands of '+' must both have arithmetic type, or pointer and integer type");
            }
            break;
        case EX_SUB:
            if(ty_isnum(lop.ty) && ty_isnum(rop.ty))
            {
                return opr_binop(loc, op, lop, rop, ret);
            }
            else if(ty_isptr(lop.ty) && ty_isint(rop.ty))
            {
                return opr_rval(lop.ty);
            }
            else if(ty_isptr(lop.ty) && ty_isptr(rop.ty))
            {
                if(!vp_type_isptrcomp(lop.ty, rop.ty))
                {
                    vp_err_error(loc, "cannot subtract pointers of different types");
                }
                return opr_rval(tyuint64);
            }
            else
            {
                vp_err_error(loc, "operands of '-' must both have arithmetic type, pointer and integer, or compatible pointer types");
            }
            break;
        case EX_MUL:
        case EX_DIV:
            if(!ty_isnum(lop.ty))
            {
                vp_err_error(loc, "left operand of '%s' must have arithmetic type", opname);
            }
            if(!ty_isnum(rop.ty))
            {
                vp_err_error(loc, "right operand of '%s' must have arithmetic type", opname);
            }
            return opr_binop(loc, op, lop, rop, ret);
        case EX_MOD:
            if(!ty_isint(lop.ty))
            {
                vp_err_error(loc, "left operand of '%%' must have integer type");
            }
            if(!ty_isint(rop.ty))
            {
                vp_err_error(loc, "right operand of '%%' must have integer type");
            }
            return opr_binop(loc, op, lop, rop, ret);
        case EX_LE:
        case EX_LT:
        case EX_GE:
        case EX_GT:
            if(ty_isnum(lop.ty) && ty_isnum(rop.ty))
            {
                Operand res = opr_binop(loc, op, lop, rop, ret);
                opr_cast(&res, tybool);
                return res;
            }
            else if(ty_isptr(lop.ty) && ty_isptr(rop.ty))
            {
                if(lop.ty->p != rop.ty->p)
                {
                    vp_err_error(loc, "cannot compare pointers of different types");
                }
                return opr_rval(tybool);
            }
            else
            {
                vp_err_error(loc, "operands of '%s' must be arithmetic types or compatible pointer types", opname);
            }
            break;
        case EX_EQ:
        case EX_NOTEQ:
            if(ty_isnum(lop.ty) && ty_isnum(rop.ty))
            {
                Operand res = opr_binop(loc, op, lop, rop, ret);
                opr_cast(&res, tybool);
                return res;
            }
            else if(ty_isptr(lop.ty) && ty_isptr(rop.ty))
            {
                if(lop.ty->p != rop.ty->p)
                {
                    vp_err_error(loc, "cannot compare pointers of different types");
                }
                return opr_rval(tybool);
            }
            else if((ty_isnil(lop.ty) && ty_isptr(rop.ty)) ||
                    (ty_isptr(lop.ty) && ty_isnil(rop.ty)))
            {
                return opr_rval(tybool);
            }
            else
            {
                vp_err_error(loc, "operands of '%s' must be arithmetic types or compatible pointer types", opname);
            }
            break;
        case EX_BAND:
        case EX_BOR:
        case EX_BXOR:
            if(ty_isint(lop.ty) && ty_isint(rop.ty))
            {
                return opr_binop(loc, op, lop, rop, ret);
            }
            else
            {
                vp_err_error(loc, "operands of '%s' must have arithmetic types", opname);
            }
            break;
        case EX_LSHIFT:
        case EX_RSHIFT:
            if(ty_isint(lop.ty) && ty_isint(rop.ty))
            {
                Type* ty = lop.ty;
                Operand res = opr_binop(loc, op, lop, rop, ret);
                opr_cast(&res, ty);
                return res;
            }
            else
            {
                vp_err_error(loc, "operands of '%s' must both have integer types", opname);
            }
            break;
        case EX_AND:
        case EX_OR:
            if(ty_isscalar(lop.ty) && ty_isscalar(rop.ty))
            {
                Operand res = opr_binop(loc, op, lop, rop, ret);
                res.ty = tybool;
                return res;
            }
            else
            {
                vp_err_error(loc, "operands of '%s' must have scalar types", opname);
            }
        default:
            vp_assertX(0, "?");
            break;
    }
    return (Operand){};
}

/* Resolve a binary expression */
static Operand sema_expr_binary(Expr* e, Type* ret)
{
    Operand lop = sema_expr_rval(e->binop.lhs, ret);
    Operand rop = sema_expr_rval(e->binop.rhs, ret);
    ExprKind op = e->kind;
    return sema_expr_binop(e->loc, op, lop, rop, ret);
}

/* Resolve named expression */
static Operand sema_expr_name(Expr* e)
{
    vp_assertX(e->kind == EX_NAME, "name");
    Sym* sym = sym_name(e->name);
    if(!sym)
    {
        vp_err_error(e->loc, "unresolved name '%s'", str_data(e->name));
    }
    Scope* scope;
    vp_scope_find(V->currscope, e->name, &scope);
    e->scope = scope;
    if(sym->kind == SYM_VAR)
        return opr_lval(sym->type);
    else if(sym->kind == SYM_FN)
        return opr_rval(sym->type);
    else
    {
        vp_err_error(e->loc, "%s must be a var", str_data(e->name));
        return (Operand){};
    }
}

/* Resolve variable or compound initializer */
static Operand sema_init(Type* ty, Expr* e)
{
    Operand opr = sema_expr(e, ty);
    if(ty_isarrempty(ty))
    {
        if(opr.ty->kind == TY_array && ty->p == opr.ty->p)
        {
            /* Empty array, infer size from initializer expression type */
            ty->len = opr.ty->len;
            return opr_rval(ty);
        }
        else if(ty_isptr(opr.ty) && ty->p == opr.ty->p)
        {
            return opr;
        }
    }
    if(ty && ty_isptr(ty))
    {
        opr = opr_decay(opr);
    }
    return opr;
}

/* Resolve compound literal */
static Operand sema_expr_complit(Expr* e, Type* ret)
{
    vp_assertX(e->kind == EX_COMPLIT, "compound");
    if(!ret && !e->comp.spec)
    {
        vp_err_error(e->loc, "implicitly typed compound literal used in context without expected type");
    }
    uint32_t numfields = vec_len(e->comp.fields);
    Type* ty = NULL;
    if(e->comp.spec)
    {
        ty = sema_typespec(e->comp.spec);
    }
    else
    {
        ty = ret;
    }
    sym_complete(ty);
    if(ty_isaggr(ty))
    {
        uint32_t idx = 0;
        for(uint32_t i = 0; i < numfields; i++)
        {
            Field* field = &e->comp.fields[i];
            if(field->kind == FIELD_IDX)
            {
                vp_err_error(field->loc, "index field init not allowed for struct/union compound literals");
            }
            else if(field->kind == FIELD_NAME)
            {
                idx = vp_type_fieldidx(ty, field->name);
                if(idx == ((uint32_t)-1))
                {
                    vp_err_error(field->loc, "named field in compound literal does not exist");
                }
            }
            if(idx >= numfields)
            {
                vp_err_error(field->loc, "field init in struct/union compound literal out of range");
            }
            Type* fieldty = ty->st.fields[idx].ty;
            Operand res = sema_init(fieldty, field->init);
            opr_conv(field->loc, &res, fieldty);
            opr_fold(field->loc, &field->init, res);
            idx++;
        }
    }
    else if(ty->kind == TY_array)
    {
        int32_t idx = 0, maxidx = 0;
        for(uint32_t i = 0; i < numfields; i++)
        {
            Field* field = &e->comp.fields[i];
            if(field->kind == FIELD_NAME)
            {
                vp_err_error(field->loc, "named field init not allowed for array compound literals");
            }
            else if(field->kind == FIELD_IDX)
            {
                Operand opr = sema_constexpr(field->idx, NULL);
                if(!ty_isint(opr.ty))
                {
                    vp_err_error(field->loc, "field init index expression must have type int");
                }
                if(!opr_cast(&opr, tyint32))
                {
                    vp_err_error(field->loc, "illegal conversion in field initializer index");
                }
                if(opr.val.i32 < 0)
                {
                    vp_err_error(field->loc, "field init index cannot be negative");
                }
                idx = opr.val.i32;
                opr_fold(field->loc, &field->idx, opr);
            }
            if(ty->len && (uint32_t)idx >= ty->len)
            {
                vp_err_error(field->loc, "field init in array compound literal out of range");
            }
            Operand res = sema_init(ty->p, field->init);
            opr_conv(field->loc, &res, ty->p);
            opr_fold(field->loc, &field->init, res);
            maxidx = MAX(maxidx, idx);
            idx++;
        }
        if(ty->len == 0)
        {
            sym_complete(ty->p);
            ty = vp_type_arr(ty->p, maxidx + 1);
        }
    }
    else
    {
        vp_err_error(e->loc, "compound literal for scalar type %s", type_name(ty));
    }
    return opr_lval(ty);
}

/* Resolve call */
static Operand sema_expr_call(Expr* e)
{
    vp_assertX(e->kind == EX_CALL, "call");
    Operand fn = sema_expr_rval(e->call.expr, NULL);
    if(fn.ty->kind != TY_func)
    {
        vp_err_error(e->loc, "cannot call non-function value");
    }
    uint32_t numarg = vec_len(e->call.args);
    uint32_t numparam = vec_len(fn.ty->fn.params);
    if(numarg < numparam)
    {
        vp_err_error(e->loc, "function call with too few arguments");
    }
    if(numarg > numparam)
    {
        vp_err_error(e->loc, "function call with too many arguments");
    }
    for(uint32_t i = 0; i < numparam; i++)
    {
        Type* typaram = fn.ty->fn.params[i];
        Operand arg = sema_expr_rval(e->call.args[i], typaram);
        opr_conv(e->loc, &arg, typaram);
    }
    e->ty = fn.ty->fn.ret;
    return opr_rval(fn.ty->fn.ret);
}

/* Resolve index */
static Operand sema_expr_idx(Expr* e)
{
    vp_assertX(e->kind == EX_IDX, "index");
    Operand opr = opr_decay(sema_expr(e->idx.expr, NULL));
    if(opr.ty->kind != TY_ptr)
    {
        vp_err_error(e->loc, "can only index arrays or pointers");
    }
    Operand idx = sema_expr_rval(e->idx.index, NULL);
    if(!ty_isint(idx.ty))
    {
        vp_err_error(e->loc, "index expression must have type int");
    }
    return opr_lval(opr.ty->p);
}

/* Resolve named field */
static Operand sema_expr_field(Expr* e)
{
    vp_assertX(e->kind == EX_FIELD, "field");
    Operand lop = sema_expr(e->field.expr, NULL);
    Type* ty = lop.ty;
    sym_complete(ty);
    if(ty_isptr(ty))
    {
        lop = opr_lval(ty->p);
        ty = lop.ty;
        sym_complete(ty);
    }
    if(ty->kind != TY_struct && ty->kind != TY_union)
    {
        vp_err_error(e->loc, "can only access fields on struct/union types");
    }
    for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
    {
        TypeField* field = &ty->st.fields[i];
        if(field->name == e->field.name)
        {
            return lop.islval ? opr_lval(field->ty) : opr_rval(field->ty);
        }
    }
    vp_err_error(e->loc, "no field named '%s'", str_data(e->field.name));
    return (Operand){};
}

/* Resolve access */
static Operand sema_expr_access(Expr* e)
{
    vp_assertX(e->kind == EX_ACCESS, "access");

    if(e->access.expr->kind != EX_NAME)
    {
        vp_err_error(e->loc, "left side of '::' must be an enum type name");
    }

    Str* name = e->access.name;
    Decl* decl = sym_find(e->access.expr->name)->decl;
    if (decl->kind != DECL_ENUM)
    {
        vp_err_error(e->loc, "'%s' is not an enum and does not support '::' access", str_data(name));
    }

    EnumItem* item = NULL;
    for(uint32_t i = 0; i < vec_len(decl->enm.items); i++)
    {
        EnumItem* curr_item = &decl->enm.items[i];
        if(curr_item->name == name)
        {
            item = curr_item;
            break;
        }
    }

    if(!item)
    {
        vp_err_error(e->loc, "enum '%s' has no constant '%s'", str_data(decl->name), str_data(name));
    }

    Operand opr;
    if(ty_isunsigned(item->ty))
    {
        opr = opr_const(item->ty, (Val){.u64 = item->val});
    }
    else
    {
        opr = opr_const(item->ty, (Val){.i64 = item->val});
    }
    e->access.val = item->val;
    return opr;
}

/* Resolve cast */
static Operand sema_expr_cast(Expr* e)
{
    vp_assertX(e->kind == EX_CAST, "cast");
    Type* ty = sema_typespec(e->cast.spec);
    Operand opr = sema_expr_rval(e->cast.expr, ty);
    bool isconst = opr.isconst;
    if(!opr_cast(&opr, ty))
    {
        vp_err_error(e->loc, "illegal type cast from %s to %s", type_name(opr.ty), type_name(ty));
    }
    return isconst ? opr_const(opr.ty, opr.val) : opr_lval(opr.ty);
}

/* Resolve intcast */
static Operand sema_expr_intcast(Expr* e)
{
    vp_assertX(e->kind == EX_INTCAST, "intcast");
    Type* ty = sema_typespec(e->cast.spec);
    Operand opr = sema_expr_rval(e->cast.expr, NULL);
    bool isconst = opr.isconst;
    if(!ty_isint(ty) || ty_isint(opr.ty))
    {
        vp_err_error(e->loc, "illegal 'intcast' from '%s' to '%s'", type_name(opr.ty), type_name(ty));
    }
    return isconst ? opr_const(opr.ty, opr.val) : opr_lval(opr.ty);
}

/* Resolve floatcast */
static Operand sema_expr_floatcast(Expr* e)
{
    vp_assertX(e->kind == EX_FLOATCAST, "floatcast");
    Type* ty = sema_typespec(e->cast.spec);
    Operand opr = sema_expr_rval(e->cast.expr, NULL);
    bool isconst = opr.isconst;
    if(!ty_isnum(ty) || !ty_isnum(opr.ty) || (!ty_isflo(ty) && !ty_isflo(opr.ty)))
    {
        vp_err_error(e->loc, "illegal 'floatcast' from '%s' to '%s'", type_name(opr.ty), type_name(ty));
    }
    return isconst ? opr_const(opr.ty, opr.val) : opr_lval(opr.ty);
}

/* Resolve ptrcast */
static Operand sema_expr_ptrcast(Expr* e)
{
    vp_assertX(e->kind == EX_PTRCAST, "ptrcast");
    Type* ty = sema_typespec(e->cast.spec);
    Operand opr = sema_expr_rval(e->cast.expr, NULL);
    bool isconst = opr.isconst;

    bool srcvalid = ty_isptrlike(opr.ty) || ty_isint(opr.ty);
    bool dstvalid = ty_isptrlike(ty);
    if(!srcvalid || !dstvalid)
    {
        vp_err_error(e->loc, "invalid 'ptrcast' between '%s' and '%s'; requires a combination of pointer, function pointer, or integer types.", type_name(opr.ty), type_name(ty));
    }
    return isconst ? opr_const(ty, opr.val) : opr_lval(ty);
}

/* Resolve bitcast */
static Operand sema_expr_bitcast(Expr* e, Type* ret)
{
    vp_assertX(e->kind == EX_BITCAST, "bitcast");
    Type* ty = sema_typespec(e->cast.spec);
    Operand opr = sema_expr_rval(e->cast.expr, NULL);
    bool isconst = opr.isconst;

    uint32_t lsize = vp_type_sizeof(ty);
    uint32_t rsize = vp_type_sizeof(opr.ty);
    if(lsize != rsize)
    {
        vp_err_error(e->loc, "bitcast size mismatch: destination type '%s' has %d bits but source type '%s' has %d bits", type_name(ty), lsize, type_name(opr.ty), rsize);
    }
    return isconst ? opr_const(ret, opr.val) : opr_lval(ty);
}

/* Resolve integer literal */
static Operand sema_expr_int(Expr* e, Type* ret, uint64_t u64)
{
    Operand res;
    Type* ity = tyint32;
    bool untyped = true;
    if(e->kind == EX_UINTT)
    {
        ity = vp_type_builtin(e->uintt.mod);
        untyped = false;
        if(ret && ity != ret)
        {
            vp_err_error(e->loc, "mismatched types");
        }
    }
    if(ret && ty_isnum(ret))
    {
        Val val = val_fromtype(e->loc, ret, u64);
        res = opr_lit(ret, val, false);
    }
    else
    {
        res = opr_lit(ity, (Val){.u64 = u64}, untyped);
    }
    return res;
}

/* Resolve number literal */
static Operand sema_expr_num(Expr* e, Type* ret)
{
    Operand res;
    if(ret && ty_isflo(ret))
    {
        Val val;
        if(ret->kind == TY_double)
        {
            val = (Val){.d = e->n};
        }
        else
        {
            val = (Val){.f = e->n};
        }
        res = opr_lit(ret, val, false);
    }
    else
    {
        res = opr_lit(tyfloat, (Val){.f = e->n}, false);
    }
    return res;
}

/* Resolve string literal */
static Operand sema_expr_str(Expr* e, Type* ret)
{
    Operand res;
    if(ret && ret->kind == TY_array)
    {
        res = opr_rval(vp_type_arr(tyuint8, e->str->len + 1));
    }
    else
    {
        res = opr_rval(vp_type_ptr(tyuint8));
    }
    return res;
}

/* Resolve expression */
static Operand sema_expr(Expr* e, Type* ret)
{
    Operand res;
    switch(e->kind)
    {
        case EX_TRUE:
        case EX_FALSE:
            res = opr_lit(tybool, (Val){.b = e->b}, false);
            break;
        case EX_NIL:
            res = opr_rval(tynil);
            break;
        case EX_CHAR:
            res = opr_lit(tyuint8, (Val){.u8 = e->i}, false);
            break;
        case EX_UINTT:
            res = sema_expr_int(e, ret, e->uintt.u);
            break;
        case EX_UINT:
            res = sema_expr_int(e, ret, e->u);
            break;
        case EX_NUM:
            res = sema_expr_num(e, ret);
            break;
        case EX_STR:
            res = sema_expr_str(e, ret);
            break;
        case EX_NAME:
            res = sema_expr_name(e);
            break;
        case EX_REF:
        {
            Operand opr;
            if(ret && ty_isptr(ret))
            {
                opr = sema_expr(e->unary, ret->p);
            }
            else
            {
                opr = sema_expr(e->unary, ret);
            }
            if(!opr.islval)
            {
                vp_err_error(e->loc, "cannot take address of non-lvalue");
            }
            res = opr_rval(vp_type_ptr(opr.ty));
            break;
        }
        case EX_NEG:
        case EX_NOT:
        case EX_BNOT:
        case EX_DEREF:
            res = sema_expr_unary(e, ret);
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
            res = sema_expr_binary(e, ret);
            break;
        case EX_COMPLIT:
            res = sema_expr_complit(e, ret);
            break;
        case EX_FIELD:
            res = sema_expr_field(e);
            break;
        case EX_ACCESS:
            res = sema_expr_access(e);
            break;
        case EX_IDX:
            res = sema_expr_idx(e);
            break;
        case EX_CALL:
            res = sema_expr_call(e);
            break;
        case EX_CAST:
            res = sema_expr_cast(e);
            break;
        case EX_INTCAST:
            res = sema_expr_intcast(e);
            break;
        case EX_FLOATCAST:
            res = sema_expr_floatcast(e);
            break;
        case EX_PTRCAST:
            res = sema_expr_ptrcast(e);
            break;
        case EX_BITCAST:
            res = sema_expr_bitcast(e, ret);
            break;
        case EX_SIZEOF:
        {
            Type* ty = sema_typespec(e->spec);
            sym_complete(ty);
            res = opr_const(tyuint64, (Val){.u64 = vp_type_sizeof(ty)});
            break;
        }
        case EX_ALIGNOF:
        {
            Type* ty = sema_typespec(e->spec);
            sym_complete(ty);
            res = opr_const(tyuint64, (Val){.u64 = vp_type_alignof(ty)});
            break;
        }
        case EX_OFFSETOF:
        {
            Type* ty = sema_typespec(e->spec);
            sym_complete(ty);
            if(ty->kind != TY_struct && ty->kind != TY_union)
            {
                vp_err_error(e->loc, "offset can only be used with struct/union types");
            }
            uint32_t idx = vp_type_fieldidx(ty, e->ofst.name);
            if(idx == (uint32_t)-1)
            {
                vp_err_error(e->loc, "no field '%s' in type", str_data(e->ofst.name));
            }
            res = opr_const(tyuint64, (Val){.u64 = ty->st.fields[idx].offset});
            break;
        }
        default:
            vp_assertX(0, "unknown expression");
            res = (Operand){};
            break;
    }
    if(res.ty)
    {
        vp_assertX(!e->ty || e->ty == res.ty, "invalid type");
        if(e->kind != EX_CAST)
        {
            if(ret && vp_type_isconv(ret, res.ty))
            {
                res.ty = ret;
            }
        }
        e->ty = res.ty;
    }
    return res;
}

/* Resolve static assert */
static void sema_staticassert(Note* note)
{
    uint32_t argsnum = vec_len(note->args);
    if(argsnum != 1)
    {
        vp_err_error(note->loc, "#staticassert takes 1 argument");
    }
    Operand res = sema_constexpr(note->args[0].e, NULL);
    if(!res.val.u64)
    {
        vp_err_error(note->loc, "#staticassert failed");
    }
}

/* Resolve note */
static void sema_note(Note* note)
{
    Str* name = note->name;
    if(strncmp(str_data(name), "staticassert", strlen("staticassert")) == 0)
    {
        sema_staticassert(note);
    }
    else
    {
        vp_err_error(note->loc, "unknown directive #%s", str_data(name));
    }
}

/* Resolve a typespec */
static Type* sema_typespec(TypeSpec* spec)
{
    Type* ty = NULL;
    switch(spec->kind)
    {
        case SPEC_TYPE:
            ty = spec->ty;
            break;
        case SPEC_NAME:
        {
            Sym* sym = sym_name(spec->name);
            if(!sym)
            {
                vp_err_error(spec->loc, "unresolved type name '%s'", str_data(spec->name));
            }
            switch(sym->kind)
            {
                case SYM_ENUM:
                case SYM_TYPE:
                    ty = sym->type;
                    break;
                default:
                    vp_err_error(spec->loc, "'%s' must denote a type", str_data(spec->name));
                    break;
            }
            break;
        }
        case SPEC_PTR:
            ty = vp_type_ptr(sema_typespec(spec->ptr));
            break;
        case SPEC_ARRAY:
        {
            int32_t len = 0;
            if(spec->arr.expr)
            {
                Operand opr = sema_constexpr(spec->arr.expr, NULL);
                if(!ty_isint(opr.ty))
                {
                    vp_err_error(spec->loc, "array size constant expression must have integer type");
                }
                opr_cast(&opr, tyint32);
                len = opr.val.i32;
                if(len <= 0)
                {
                    vp_err_error(spec->loc, "non-positive array length");
                }
                opr_fold(spec->loc, &spec->arr.expr, opr);
            }
            Type* tyarr = sema_typespec(spec->arr.base);
            sym_complete(tyarr);
            ty = vp_type_arr(tyarr, len);
            break;
        }
        case SPEC_FUNC:
        {
            vec_t(Type*) args = NULL;
            for(uint32_t i = 0; i < vec_len(spec->fn.args); i++)
            {
                Type* targ = sema_typespec(spec->fn.args[i]);
                vec_push(args, targ);
            }
            Type* ret = sema_typespec(spec->fn.ret);
            ty = vp_type_func(ret, args);
            break;
        }
        case SPEC_TYPEOF:
            ty = sema_expr(spec->expr, NULL).ty;
            break;
        case SPEC_CONST:
        {
            Type* tyconst = sema_typespec(spec->ptr);
            /*sym_complete(tyconst);*/
            ty = vp_type_const(tyconst);
            break;
        }
        default:
            vp_assertX(0, "unknown typespec");
            break;
    }
    return ty;
}

/* Resolve type declaration */
static Type* sema_typedef(Decl* d)
{
    vp_assertX(d->kind == DECL_TYPE, "type declaration");
    return sema_typespec(d->ts.spec);
}

/* Resolve fn declaration */
static Type* sema_fn(Decl* d)
{
    vp_assertX(d->kind == DECL_FN, "fn declaration");

    vec_push(S.sorted, d);
    vec_t(Type*) params = NULL;
    for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
    {
        Type* typaram = sema_typespec(d->fn.params[i].spec);
        typaram = vp_type_decayempty(typaram);
        sym_complete(typaram);
        if(typaram == tyvoid)
        {
            vp_err_error(d->loc, "function parameter type cannot be 'void'");
        }
        vec_push(params, typaram);
    }
    Type* ret = vp_type_decayempty(sema_typespec(d->fn.ret));
    if(ret->kind == TY_array)
    {
        vp_err_error(d->loc, "function return type cannot be array");
    }
    return vp_type_func(ret, params);
}

static void sema_stmt(Stmt* st, Type* ret);
static Type* sema_var(Decl* d);

/* Resolve a condition expression */
static void sema_cond(Expr* e)
{
    Operand cond = sema_expr(e, NULL);
    if(!ty_isscalar(cond.ty))
    {
        vp_err_error(e->loc, "condition expression must have scalar type");
    }
}

/* Resolve block of statements */
static void sema_block(Stmt** stmts, Type* ret)
{
    Scope* scope = vp_scope_begin();
    vec_push(S.currfn->fn.scopes, scope);
    uint32_t len = sym_enter();

    for(uint32_t i = 0; i < vec_len(stmts); i++)
    {
        Stmt* st = stmts[i];
        sema_stmt(st, ret);
    }
    sym_leave(len);
    vp_scope_end();
}

/* Resolve assignment */
static void sema_stmt_assign(Stmt* st)
{
    vp_assertX(st->kind >= ST_ASSIGN && st->kind <= ST_RSHIFT_ASSIGN, "assignment");
    Operand lop = sema_expr(st->lhs, NULL);
    if(!lop.islval)
    {
        vp_err_error(st->loc, "cannot assign to non-lvalue");
    }
    if(lop.ty->kind == TY_array)
    {
        vp_err_error(st->loc, "cannot assign to array");
    }
    /*if(ty_isconst(lop.ty))
    {
        vp_err_error(st->loc, "left-hand side of assign is const");
    }*/
    Operand rop = sema_expr_rval(st->rhs, lop.ty);
    if(st->kind != ST_ASSIGN)
    {
        ExprKind kind = st->kind - ST_ADD_ASSIGN + EX_ADD;
        rop = sema_expr_binop(st->loc, kind, lop, rop, lop.ty);
    }
    opr_conv(st->loc, &rop, lop.ty);
    opr_fold(st->loc, &st->rhs, rop);
}

/* Resolve a statement */
static void sema_stmt(Stmt* st, Type* ret)
{
    switch(st->kind)
    {
        case ST_RETURN:
            if(st->expr)
            {
                Operand res = sema_expr_rval(st->expr, ret);
                opr_conv(st->expr->loc, &res, ret);
                opr_fold(st->expr->loc, &st->expr, res);
            }
            else if(ret != tyvoid)
            {
                vp_err_error(st->loc, "empty return expression for function with non-void return type");
            }
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
            sema_stmt_assign(st);
            break;
        case ST_BLOCK:
            sema_block(st->block, ret);
            break;
        case ST_IF:
            sema_cond(st->ifst.cond);
            sema_stmt(st->ifst.tblock, ret);
            if(st->ifst.fblock)
            {
                sema_stmt(st->ifst.fblock, ret);
            }
            break;
        case ST_ASM:
        case ST_BREAK:
        case ST_CONTINUE:
            /* Ignore */
            break;
        case ST_WHILE:
            sema_cond(st->whst.cond);
            sema_stmt(st->whst.body, ret);
            break;
        case ST_EXPR:
        {
            Operand res = sema_expr(st->expr, NULL);
            opr_fold(st->loc, &st->expr, res);
            break;
        }
        case ST_DECL:
        {
            Decl* d = st->decl;
            if(st->decl->kind == DECL_VAR)
            {
                Type* ty = sema_var(d);
                sym_add(d->name, ty);
            }
            else if(st->decl->kind == DECL_NOTE)
            {
                sema_note(&st->decl->note);
            }
            else
            {
                vp_err_error(d->loc, "unimplemented local type declarations");
            }
            break;
        }
        default:
            vp_assertX(0, "unknown statement");
            break;
    }
}

/* Resolve function body */
static void sema_fn_body(Sym* sym)
{
    Decl* d = sym->decl;
    vp_assertX(d->kind == DECL_FN, "fn declaration");
    vp_assertX(sym->state == SYM_DONE, "unresolved symbol");

    /* Add function name to global scope */
    VarInfo* vi = vp_scope_add(V->globscope, d->name, sym->type);
    vi->storage |= VS_FN;

    if(!d->fn.body)
        return;

    Scope* scope = vp_scope_begin();
    vec_push(d->fn.scopes, scope);
    S.currfn = d;

    uint32_t len = sym_enter();
    for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
    {
        Param* param = &d->fn.params[i];
        Type* pty = sema_typespec(param->spec);
        sym_add(param->name, pty);

        vp_scope_add(V->currscope, param->name, pty);
    }
    Type* ret = sema_typespec(d->fn.ret);
    d->fn.rett = ret;
    for(uint32_t i = 0; i < vec_len(d->fn.body->block); i++)
    {
        sema_stmt(d->fn.body->block[i], ret);
    }
    sym_leave(len);
    vp_scope_end();
}

/* Resolve var declaration */
static Type* sema_var(Decl* d)
{
    vp_assertX(d->kind == DECL_VAR, "var declaration");
    Type* ty = NULL;
    Type* inferty = NULL;
    Type* declty = NULL;
    if(d->var.spec)
    {
        declty = ty = sema_typespec(d->var.spec);
        if(d->var.expr)
        {
            Operand res = sema_init(declty, d->var.expr);
            inferty = ty = res.ty;
            opr_conv(d->loc, &res, declty);
            opr_fold(d->loc, &d->var.expr, res);
            d->var.expr->ty = declty;
        }
    }
    else
    {
        vp_assertX(d->var.expr, "expression");
        Operand res = sema_expr(d->var.expr, NULL);
        inferty = ty = res.ty;
        if(ty->kind == TY_array && d->var.expr->kind != EX_COMPLIT)
        {
            ty = vp_type_decay(ty);
        }
        opr_fold(d->loc, &d->var.expr, res);
        d->var.expr->ty = inferty;
    }
    sym_complete(ty);
    if(!d->var.expr || ty_isptr(inferty))
    {
        ty = vp_type_decayempty(ty);
    }
    if(ty_isnil(ty))
    {
        vp_err_error(d->loc, "cannot infer type of nil");
    }
    if(vp_type_sizeof(ty) == 0)
    {
        vp_err_error(d->loc, "variable of size 0");
    }
    d->var.vi = vp_scope_add(V->currscope, d->name, ty);
    return ty;
}

/* Resolve enum declaration */
static void sema_enum(Decl* d, Sym* sym)
{
    vp_assertX(d->kind == DECL_ENUM, "enum declaration");

    Type* ty = sema_typespec(d->enm.spec);
    if(!ty_isint(ty))
    {
        vp_err_error(d->loc, "enum base type must be an integer type");
    }

    uint64_t val = 0;
    for(uint32_t i = 0; i < vec_len(d->enm.items); i++)
    {
        EnumItem* item = &d->enm.items[i];
        item->ty = ty;

        if(item->init)
        {
            Operand opr = sema_constexpr(item->init, ty);
            if(!ty_isint(opr.ty))
            {
                vp_err_error(item->loc, "enum constant initializer must have an integer type");
            }

            opr_conv(item->loc, &opr, ty);
            opr_fold(item->loc, &item->init, opr);

            if(ty_isunsigned(ty))
            {
                val = opr.val.u64;
            }
            else
            {
                val = (uint64_t)opr.val.i64;
            }
        }
        else
        {
            if(val_overflow(val, ty))
            {
                vp_err_error(item->loc, "literal out of range for '%s'", type_name(ty));
            }
        }

        item->val = val;
        val++;
    }

    sym->type = ty;
    sym->state = SYM_DONE;
}

/* Resolve symbols */
static void sema_resolve(Sym* sym)
{
    if(sym->state == SYM_DONE)
        return;
    else if(sym->state == SYM_PROGRESS)
    {
        vp_err_error(sym->decl->loc, "cyclic dependency");
    }
    vp_assertX(sym->state == SYM_PENDING, "bad symbol state");
    sym->state = SYM_PROGRESS;
    switch(sym->kind)
    {
        case SYM_TYPE:
            sym->type = sema_typedef(sym->decl);
            break;
        case SYM_VAR:
            sym->type = sema_var(sym->decl);
            break;
        case SYM_FN:
            sym->type = sema_fn(sym->decl);
            break;
        default:
            vp_assertX(0, "unknown symbol");
            break;
    }
    sym->state = SYM_DONE;
}

vec_t(Decl*) vp_sema(vec_t(Decl*) decls)
{
    /* Create symbols */
    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        Decl* d = decls[i];
        if(d->kind == DECL_NOTE)
        {
            sema_note(&d->note);
        }
        else
        {
            Sym* sym = sym_decl(d);
            sym_glob_put(sym);
            if(d->kind == DECL_ENUM)
            {
                sema_enum(d, sym);
            }
        }
    }
    /* Resolve symbols */
    for(Sym** p = S.syms; p != vec_end(S.syms); p++)
    {
        Sym* sym = *p;
        sema_resolve(sym);
        switch(sym->kind)
        {
            case SYM_TYPE:
                sym_complete(sym->type);
                break;
            case SYM_FN:
                sema_fn_body(sym);
                break;
            default:
                break;
        }
    }

    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        vp_dump_ast(decls[i]);
    }
    //vp_dump_typecache();

    return S.sorted;
}