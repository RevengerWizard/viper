/*
** vp_sema.c
** Semantic analyser
*/

#include <string.h>

#include "vp_sema.h"
#include "vp_ast.h"
#include "vp_def.h"
#include "vp_mem.h"
#include "vp_parse.h"
#include "vp_print.h"
#include "vp_str.h"
#include "vp_tab.h"
#include "vp_type.h"
#include "vp_vec.h"

#include "vp_print.h"

#define MAX_LOCAL_SYMS 1024

Sym** sorted;
Tab globs;
Sym** syms;
Sym localsyms[MAX_LOCAL_SYMS];
Sym* localend = localsyms;

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

static void sym_push_var(Str* name, Type* ty)
{
    if(localend == localsyms + MAX_LOCAL_SYMS)
    {
        vp_assertX(0, "too many local symbols");
    }
    *localend++ = (Sym){
        .name = name,
        .kind = SYM_VAR,
        .state = SYM_DONE,
        .type = ty,
    };
}

/* Enter scope */
static Sym* sym_enter()
{
    return localend;
}

/* Leave scope */
static void sym_leave(Sym* sym)
{
    localend = sym;
}

/* Get a local or global symbol */
static Sym* sym_get(Str* name)
{
    for(Sym* it = localend; it != localsyms; it--)
    {
        Sym* sym = it - 1;
        if(sym->name == name)
            return sym;
    }
    return vp_tab_get(&globs, name);
}

static void sema_resolve(Sym* sym);
static Type* sema_typespec(TypeSpec* spec);

/* Find and resolve a local/global symbol */
static Sym* sym_name(Str* name)
{
    Sym* sym = sym_get(name);
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

/* This probably should be improved */
static bool field_duplicates(TypeField* fields)
{
    for(uint32_t i = 0; i < vec_len(fields); i++)
    {
        for(uint32_t j = i + 1; j < vec_len(fields); j++)
        {
            if(fields[i].name == fields[j].name)
            {
                return true;
            }
        }
    }
    return false;
}

/* Complete a (symbolic) type */
static void type_complete(Type* ty)
{
    if(ty->kind == TY_name)
    {
        vp_parse_error(ty->sym->decl->loc, "type completion cycle");
    }
    else if(ty->kind != TY_none)
        return;
    
    Decl* d = ty->sym->decl;
    ty->kind = TY_name;
    vp_assertX(d->kind == DECL_STRUCT || d->kind == DECL_UNION, "struct/union");
    
    TypeField* fields = NULL;
    for(uint32_t i = 0; i < vec_len(d->agr->items); i++)
    {
        AggregateItem* item = &d->agr->items[i];
        Type* tyitem = sema_typespec(item->type);
        tyitem = vp_type_decayempty(tyitem);
        type_complete(tyitem);
        if(vp_type_sizeof(tyitem) == 0)
        {
            if(tyitem->kind != TY_array || vp_type_sizeof(tyitem->p) == 0)
            {
                vp_parse_error(item->loc, "field type of size 0 is not allowed");
            }
        }
        for(uint32_t j = 0; j < vec_len(item->names); j++)
        {
            vec_push(fields, (TypeField){item->names[j], tyitem});
        }
    }
    if(vec_len(fields) == 0)
    {
        vp_parse_error(d->loc, "no fields");
    }
    if(field_duplicates(fields))
    {
        vp_parse_error(d->loc, "duplicate fields");
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
    vec_push(sorted, ty->sym);
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
static Operand opr_lit(Type* ty, Val val)
{
    return (Operand){.ty = ty, .val = val, .islit = true, .isconst = true};
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

    if(!vp_type_iscast(opr->ty, ty))
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

/* Implicit operand type conversion */
static bool opr_conv(Operand* opr, Type* ty)
{
    if(vp_type_isconv(ty, opr->ty))
    {
        opr_cast(opr, ty);
        opr->islval = false;
        return true;
    }
    return false;
}

#undef CASE

/* Unify operands of different types */
static Type* opr_unify(Expr* e, Operand* lop, Operand* rop, Type* ret)
{
    if(!ret)
    {
        Type* lty = lop->ty;
        Type* rty = rop->ty;
        opr_conv(lop, rty);
        opr_conv(rop, lty);
    }
    else
    {
        opr_conv(lop, ret);
        opr_conv(rop, ret);
    }
    if(lop->ty != rop->ty)
    {
        const char* opname = ast_binname(e->kind);
        vp_parse_error(e->loc, 
            "incompatible operand types for '%s': %s and %s",
            opname, type_name(lop->ty), type_name(rop->ty));
    }
    return lop->ty;
}

/* Value unary operator folding */
static Val val_unary(ExprKind op, Type* ty, Val val)
{
    Operand opr = opr_const(ty, val);
    opr_cast(&opr, ty);
    if(type_issigned(ty))
    {
        opr.val.i64 = fold_unary_i64(op, opr.val.i64);
    }
    else if(type_isunsigned(ty))
    {
        opr.val.u64 = fold_unary_u64(op, opr.val.u64);
    }
    else if(type_isflo(ty))
    {
        opr.val.d = fold_unary_f(op, opr.val.d);
    }
    else
    {
        vp_assertX(0, "unknown type to fold");
    }
    opr_cast(&opr, ty);
    return opr.val;
}

/* Value of binary operator folding */
static Val val_binop(ExprKind op, Type* ty, Val lval, Val rval)
{
    Operand lop = opr_const(ty, lval);
    Operand rop = opr_const(ty, rval);
    opr_cast(&lop, ty);
    opr_cast(&rop, ty);
    Operand res;
    if(type_isunsigned(ty))
    {
        uint64_t val = fold_binop_u64(op, lop.val.u64, rop.val.u64);
        res = opr_const(ty, (Val){.u64 = val});
    }
    else if(type_issigned(ty))
    {
        int64_t val = fold_binop_i64(op, lop.val.i64, rop.val.i64);
        res = opr_const(ty, (Val){.i64 = val});
    }
    else if(type_isflo(ty))
    {
        double val = fold_binop_f(op, lop.val.d, rop.val.d);
        res = opr_const(ty, (Val){.d = val});
    }
    else
    {
        vp_assertX(0, "unknown type to fold");
    }
    opr_cast(&res, ty);
    return res.val;
}

/*static void opr_fold(SrcLoc loc, Expr** e, Operand opr)
{
    if(opr.isconst && !opr.islit)
    {
        if(type_isint(opr.ty))
        {
            if(type_issigned(opr.ty))
            {
                *e = vp_expr_ilit(loc, opr.val.i64);
            }
            else
            {
                *e = vp_expr_ulit(loc, opr.val.u64);
            }
        }
        else if(type_isflo(opr.ty))
        {
            if(opr.ty->kind == TY_double)
            {
                *e = vp_expr_nlit(loc, opr.val.d);
            }
            else
            {
                *e = vp_expr_flit(loc, opr.val.f);
            }
        }
    }
}*/

static Operand opr_unary(ExprKind op, Operand opr)
{
    if(opr.isconst)
    {
        return opr_const(opr.ty, val_unary(op, opr.ty, opr.val));
    }
    else
    {
        return opr;
    }
}

static Operand opr_binop(Expr* e, Operand lop, Operand rop, Type* ret)
{
    ExprKind op = e->kind;
    ret = opr_unify(e, &lop, &rop, ret);
    if(lop.isconst && rop.isconst)
    {
        return opr_const(ret, val_binop(op, ret, lop.val, rop.val));
    }
    else
    {
        return opr_rval(ret);
    }
}

/* -- Expression semantics ------------------------------------------ */

/* Forward declarations */
static Operand sema_expr(Expr* e, Type* ret);
static Type* sema_var(Decl* d);

/* Resolve rval expression */
static Operand sema_expr_rval(Expr* e, Type* ret)
{
    return opr_decay(sema_expr(e, ret));
}

/* Resolve constant expression */
static Operand sema_constexpr(Expr* e)
{
    Operand res = sema_expr(e, NULL);
    if(!res.isconst)
    {
        vp_parse_error(e->loc, "expected constant expression");
    }
    return res;
}

/* Resolve unary expression */
static Operand sema_expr_unary(Expr* e, Type* ret)
{
    Operand opr = sema_expr_rval(e->unary, ret);
    Type* ty = opr.ty;
    ExprKind op = e->kind;
    switch(op)
    {
        case EX_DEREF:
            if(!type_isptr(ty))
            {
                vp_parse_error(e->loc, "cannot deref non-pointer type");
            }
            return opr_lval(ty->p);
        case EX_NOT:
            if(!type_isscalar(ty))
            {
                vp_parse_error(e->loc, "can only use 'not' with scalar types");
            }
            return opr_unary(op, opr);
        case EX_NEG:
            if(!type_isnum(ty))
            {
                vp_parse_error(e->loc, "can only use unary '-' with arithmetic types");
            }
            return opr_unary(op, opr);
        case EX_BNOT:
            if(!type_isint(ty))
            {
                vp_parse_error(e->loc, "can only use '~' with integer types");
            }
            return opr_unary(op, opr);
        default:
            vp_assertX(0, "?");
            break;
    }
    return (Operand){};
}

/* Resolve binary expression */
static Operand sema_expr_binop(Expr* e, Type* ret)
{
    Operand lop = sema_expr_rval(e->binop.lhs, ret);
    Operand rop = sema_expr_rval(e->binop.rhs, ret);
    ExprKind op = e->kind;
    const char* opname = ast_binname(op);
    switch(op)
    {
        case EX_ADD:
            if(type_isnum(lop.ty) && type_isnum(rop.ty))
            {
                return opr_binop(e, lop, rop, ret);
            }
            else if(type_isptr(lop.ty) && type_isint(rop.ty))
            {
                type_complete(lop.ty->p);
                return opr_rval(lop.ty);
            }
            else if(type_isptr(rop.ty) && type_isint(lop.ty))
            {
                type_complete(rop.ty->p);
                return opr_rval(rop.ty);
            }
            else
            {
                vp_parse_error(e->loc, "operands of '+' must both have arithmetic type, or pointer and integer type");
            }
            break;
        case EX_SUB:
            if(type_isnum(lop.ty) && type_isnum(rop.ty))
            {
                return opr_binop(e, lop, rop, ret);
            }
            else if(type_isptr(lop.ty) && type_isint(rop.ty))
            {
                return opr_rval(lop.ty);
            }
            else if(type_isptr(lop.ty) && type_isptr(rop.ty))
            {
                if(!vp_type_isptrcomp(lop.ty, rop.ty))
                {
                    vp_parse_error(e->loc, "cannot subtract pointers of different types");
                }
                return opr_rval(tyuint64);
            }
            else
            {
                vp_parse_error(e->loc, "operands of '-' must both have arithmetic type, pointer and integer, or compatible pointer types");
            }
            break;
        case EX_MUL:
        case EX_DIV:
            if(!type_isnum(lop.ty))
            {
                vp_parse_error(e->loc, "left operand of '%s' must have arithmetic type", opname);
            }
            if(!type_isnum(rop.ty))
            {
                vp_parse_error(e->loc, "right operand of '%s' must have arithmetic type", opname);
            }
            return opr_binop(e, lop, rop, ret);
        case EX_LE:
        case EX_LT:
        case EX_GE:
        case EX_GT:
            if(type_isnum(lop.ty) && type_isnum(rop.ty))
            {
                Operand res = opr_binop(e, lop, rop, ret);
                opr_cast(&res, tybool);
                return res;
            }
            else if(type_isptr(lop.ty) && type_isptr(rop.ty))
            {
                if(lop.ty->p != rop.ty->p)
                {
                    vp_parse_error(e->loc, "cannot compare pointers of different types");
                }
                return opr_rval(tybool);
            }
            else
            {
                vp_parse_error(e->loc, "operands of '%s' must be arithmetic types or compatible pointer types", opname);
            }
            break;
        case EX_EQ:
        case EX_NOTEQ:
            if(type_isnum(lop.ty) && type_isnum(rop.ty))
            {
                Operand res = opr_binop(e, lop, rop, ret);
                opr_cast(&res, tyint32);
                return res;
            }
            else if(type_isptr(lop.ty) && type_isptr(rop.ty))
            {
                if(lop.ty->p != rop.ty->p)
                {
                    vp_parse_error(e->loc, "cannot compare pointers of different types");
                }
                return opr_rval(tyint32);
            }
            else if((type_isnil(lop.ty) && type_isptr(rop.ty)) ||
                    (type_isnil(rop.ty) && type_isptr(lop.ty)))
            {
                return opr_rval(tyint32);
            }
            else
            {
                vp_parse_error(e->loc, "operands of '%s' must be arithmetic types or compatible pointer types", opname);
            }
            break;
        case EX_BAND:
        case EX_BOR:
        case EX_BXOR:
            if(type_isint(lop.ty) && type_isint(rop.ty))
            {
                return opr_binop(e, lop, rop, ret);
            }
            else
            {
                vp_parse_error(e->loc, "operands of '%s' must have arithmetic types", opname);
            }
            break;
        case EX_LSHIFT:
        case EX_RSHIFT:
            if(type_isint(lop.ty) && type_isint(rop.ty))
            {
                Type* ty = lop.ty;
                Operand res = opr_binop(e, lop, rop, ret);
                opr_cast(&res, ty);
                return res;
            }
            else
            {
                vp_parse_error(e->loc, "operands of '%s' must both have integer types", opname);
            }
            break;
        case EX_AND:
        case EX_OR:
            if(type_isscalar(lop.ty) && type_isscalar(rop.ty))
            {
                if(lop.isconst && rop.isconst)
                {
                    opr_cast(&lop, tybool);
                    opr_cast(&rop, tybool);
                    bool i;
                    if(op == EX_AND)
                    {
                        i = lop.val.b && rop.val.b;
                    }
                    else
                    {
                        i = lop.val.b || rop.val.b;
                    }
                    return opr_const(tyint32, (Val){.i32 = i});
                }
                else
                {
                    return opr_rval(tyint32);
                }
            }
            else
            {
                vp_parse_error(e->loc, "operands of '%s' must have scalar types", opname);
            }
        default:
            vp_assertX(0, "?");
            break;
    }
    return (Operand){};
}

/* Resolve named expression */
static Operand sema_expr_name(Expr* e)
{
    vp_assertX(e->kind == EX_NAME, "name");
    Sym* sym = sym_name(e->name);
    if(!sym)
    {
        vp_parse_error(e->loc, "unresolved name '%s'", str_data(e->name));
    }
    if(sym->kind == SYM_VAR)
        return opr_lval(sym->type);
    else if(sym->kind == SYM_FN)
        return opr_rval(sym->type);
    else
    {
        vp_parse_error(e->loc, "%s must be a var", str_data(e->name));
        return (Operand){};
    }
}

/* Resolve variable or compound initializer */
static Operand sema_init(Type* ty, Expr* e)
{
    Operand opr = sema_expr(e, ty);
    if(type_isarrempty(ty))
    {
        if(opr.ty->kind == TY_array && ty->p == opr.ty->p)
        {
            /* Empty array, infer size from initializer expression type */
            ty->len = opr.ty->len;
            return opr_rval(ty);
        }
        else if(type_isptr(opr.ty) && ty->p == opr.ty->p)
        {
            return opr;
        }
    }
    if(ty && type_isptr(ty))
    {
        opr = opr_decay(opr);
    }
    return opr;
}

/* Resolve compound literal */
static Operand sema_expr_comp(Expr* e, Type* ret)
{
    vp_assertX(e->kind == EX_COMPOUND, "compound");
    if(!ret && !e->comp.spec)
    {
        vp_parse_error(e->loc, "implicitly typed compound literal used in context without expected type");
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
    type_complete(ty);
    if(type_isaggr(ty))
    {
        uint32_t idx = 0;
        for(uint32_t i = 0; i < numfields; i++)
        {
            Field* field = &e->comp.fields[i];
            if(field->kind == FIELD_IDX)
            {
                vp_parse_error(field->loc, "index field init not allowed for struct/union compound literals");
            }
            else if(field->kind == FIELD_NAME)
            {
                idx = vp_type_fieldidx(ty, field->name);
                if(idx == ((uint32_t)-1))
                {
                    vp_parse_error(field->loc, "named field in compound literal does not exist");
                }
            }
            if(idx >= numfields)
            {
                vp_parse_error(field->loc, "field init in struct/union compound literal out of range");
            }
            Type* fieldty = ty->st.fields[idx].ty;
            Operand res = sema_init(fieldty, field->init);
            if(!opr_conv(&res, fieldty))
            {
                vp_parse_error(field->loc, "implicit %s to %s", type_name(res.ty), type_name(fieldty));
            }
            //opr_fold(field->loc, &field->init, res);
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
                vp_parse_error(field->loc, "named field init not allowed for array compound literals");
            }
            else if(field->kind == FIELD_IDX)
            {
                Operand opr = sema_constexpr(field->idx);
                if(!type_isint(opr.ty))
                {
                    vp_parse_error(field->loc, "field init index expression must have type int");
                }
                if(!opr_cast(&opr, tyint32))
                {
                    vp_parse_error(field->loc, "illegal conversion in field initializer index");
                }
                if(opr.val.i32 < 0)
                {
                    vp_parse_error(field->loc, "field init index cannot be negative");
                }
                idx = opr.val.i32;
                //opr_fold(field->loc, &field->idx, opr);
            }
            if(ty->len && idx >= ty->len)
            {
                vp_parse_error(field->loc, "field init in array compound literal out of range");
            }
            Operand res = sema_init(ty->p, field->init);
            if(!opr_conv(&res, ty->p))
            {
                vp_parse_error(field->loc, "implicit %s to %s", type_name(res.ty), type_name(ty->p));
            }
            //opr_fold(field->loc, &field->init, res);
            maxidx = MAX(maxidx, idx);
            idx++;
        }
        if(ty->len == 0)
        {
            type_complete(ty->p);
            ty = vp_type_arr(ty->p, maxidx + 1);
        }
    }
    else
    {
        vp_parse_error(e->loc, "compound literal for scalar type %s", type_name(ty));
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
        vp_parse_error(e->loc, "cannot call non-function value");
    }
    uint32_t numarg = vec_len(e->call.args);
    uint32_t numparam = vec_len(fn.ty->fn.params);
    if(numarg < numparam)
    {
        vp_parse_error(e->loc, "function call with too few arguments");
    }
    if(numarg > numparam)
    {
        vp_parse_error(e->loc, "function call with too many arguments");
    }
    for(uint32_t i = 0; i < numparam; i++)
    {
        Type* typaram = fn.ty->fn.params[i];
        Operand arg = sema_expr_rval(e->call.args[i], typaram);
        if(!opr_conv(&arg, typaram))
        {
            vp_parse_error(e->loc, "illegal conversion in call argument expression");
        }
    }
    return opr_rval(fn.ty->fn.ret);
}

/* Resolve index */
static Operand sema_expr_idx(Expr* e)
{
    vp_assertX(e->kind == EX_IDX, "index");
    Operand opr = opr_decay(sema_expr(e->idx.expr, NULL));
    if(opr.ty->kind != TY_ptr)
    {
        vp_parse_error(e->loc, "can only index arrays or pointers");
    }
    Operand idx = sema_expr_rval(e->idx.index, NULL);
    if(!type_isint(idx.ty))
    {
        vp_parse_error(e->loc, "index expression must have type int");
    }
    return opr_lval(opr.ty->p);
}

/* Resolve named field */
static Operand sema_expr_field(Expr* e)
{
    vp_assertX(e->kind == EX_FIELD, "field");
    Operand lop = sema_expr(e->field.expr, NULL);
    Type* ty = lop.ty;
    type_complete(ty);
    if(ty->kind != TY_struct && ty->kind != TY_union)
    {
        vp_parse_error(e->loc, "can only access fields on struct/union types");
    }
    for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
    {
        TypeField* field = &ty->st.fields[i];
        if(field->name == e->field.name)
        {
            return lop.islval ? opr_lval(field->ty) : opr_rval(field->ty);
        }
    }
    vp_parse_error(e->loc, "no field named '%s'", e->field.name);
    return (Operand){};
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
        vp_parse_error(e->loc, "illegal type cast from %s to %s", type_name(opr.ty), type_name(ty));
    }
    return isconst ? opr_const(opr.ty, opr.val) : opr_lval(opr.ty);
}

/* Resolve integer literal */
static Operand sema_expr_int(Expr* e, Type* ret)
{
    Operand res;
    if(ret && type_isnum(ret))
    {
        res = opr_lit(ret, (Val){.i64 = e->i});
    }
    else
    {
        res = opr_lit(tyint32, (Val){.i64 = e->i});
    }
    return res;
}

/* Resolve number literal */
static Operand sema_expr_num(Expr* e, Type* ret)
{
    Operand res;
    if(ret && type_isflo(ret))
    {
        res = opr_lit(ret, (Val){.d = e->n});
    }
    else
    {
        res = opr_lit(tyfloat, (Val){.d = e->n});
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
            res = opr_rval(tybool);
            break;
        case EX_NIL:
            res = opr_rval(tynil);
            break;
        case EX_CHAR:
            res = opr_lit(tyuint8, (Val){.u8 = e->i});
            break;
        case EX_INT:
            res = sema_expr_int(e, ret);
            break;
        case EX_NUM:
            res = sema_expr_num(e, ret);
            break;
        case EX_STR:
            res = opr_rval(vp_type_arr(tyuint8, e->name->len + 1));
            break;
        case EX_NAME:
            res = sema_expr_name(e);
            break;
        case EX_REF:
        {
            Operand opr;
            if(ret && type_isptr(ret))
            {
                opr = sema_expr(e->unary, ret->p);
            }
            else
            {
                opr = sema_expr(e->unary, ret);
            }
            if(!opr.islval)
            {
                vp_parse_error(e->loc, "cannot take address of non-lvalue");
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
            res = sema_expr_binop(e, ret);
            break;
        case EX_COMPOUND:
            res = sema_expr_comp(e, ret); 
            break;
        case EX_FIELD:
            res = sema_expr_field(e);
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
        case EX_SIZEOF:
        {
            Type* ty = sema_typespec(e->spec);
            type_complete(ty);
            res = opr_const(tyuint64, (Val){.u64 = vp_type_sizeof(ty)});
            break;
        }
        case EX_ALIGNOF:
        {
            Type* ty = sema_typespec(e->spec);
            type_complete(ty);
            res = opr_const(tyuint64, (Val){.u64 = vp_type_alignof(ty)});
            break;
        }
        case EX_OFFSETOF:
        {
            Type* ty = sema_typespec(e->spec);
            type_complete(ty);
            if(ty->kind != TY_struct && ty->kind != TY_union)
            {
                vp_parse_error(e->loc, "offset can only be used with struct/union types");
            }
            uint32_t idx = vp_type_fieldidx(ty, e->ofst.name);
            if(idx == (uint32_t)-1)
            {
                vp_parse_error(e->loc, "no field '%s' in type", str_data(e->ofst.name));
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
        if(ret && vp_type_isconv(ret, res.ty))
        {
            res.ty = ret;
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
        vp_parse_error(note->loc, "#staticassert takes 1 argument");
    }
    Operand res = sema_constexpr(note->args[0].e);
    if(!res.val.u64)
    {
        vp_parse_error(note->loc, "#staticassert failed");
    }
}

/* Resolve notes */
static void sema_notes(Note* notes)
{
    for(uint32_t i = 0; i < vec_len(notes); i++)
    {
        Note* note = &notes[i];
        Str* name = note->name;
        if(strncmp(str_data(name), "staticassert", strlen("staticassert")) == 0)
        {
            sema_staticassert(note);
        }
        else
        {
            vp_parse_error(note->loc, "unknown directive #%s", str_data(name));
        }
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
            if(sym->kind != SYM_TYPE)
            {
                vp_parse_error(spec->loc, "%s must denote a type", str_data(spec->name));
            }
            ty = sym->type;
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
                Operand opr = sema_constexpr(spec->arr.expr);
                if(!type_isint(opr.ty))
                {
                    vp_parse_error(spec->loc, "array size constant expression must have integer type");
                }
                opr_cast(&opr, tyint32);
                len = opr.val.i32;
                if(len <= 0)
                {
                    vp_parse_error(spec->loc, "non-positive array length");
                }
                //opr_fold(spec->loc, &spec->arr.expr, opr);
            }
            Type* tyarr = sema_typespec(spec->arr.base);
            type_complete(tyarr);
            ty = vp_type_arr(tyarr, len);
            break;
        }
        case SPEC_FUNC:
        {
            Type** args = NULL;
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
        {
            ty = sema_expr(spec->expr, NULL).ty;
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
    Type** params = NULL;
    for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
    {
        Type* typaram = sema_typespec(d->fn.params[i].spec);
        typaram = vp_type_decayempty(typaram);
        type_complete(typaram);
        if(typaram == tyvoid)
        {
            vp_parse_error(d->loc, "function parameter type cannot be 'void'");
        }
        vec_push(params, typaram);
    }
    Type* ret = vp_type_decayempty(sema_typespec(d->fn.ret));
    if(ret->kind == TY_array)
    {
        vp_parse_error(d->loc, "function return type cannot be array");
    }
    return vp_type_func(ret, params);
}

static void sema_stmt(Stmt* st, Type* ret);

/* Resolve block of statements */
static void sema_block(Stmt** stmts, Type* ret)
{
    Sym* scope = sym_enter();   /* Enter scope */
    for(uint32_t i = 0; i < vec_len(stmts); i++)
    {
        Stmt* st = stmts[i];
        sema_stmt(st, ret);
    }
    sym_leave(scope);   /* Leave scope */
}

/* Resolve assignment */
static void sema_stmt_assign(Stmt* st)
{
    vp_assertX(st->kind == ST_ASSIGN, "assignment");
    Operand lop = sema_expr(st->lhs, NULL);
    if(!lop.islval)
    {
        vp_parse_error(st->loc, "cannot assign to non-lvalue");
    }
    Operand rop = sema_expr(st->rhs, lop.ty);
    if(!opr_conv(&rop, lop.ty))
    {
        vp_parse_error(st->loc, "illegal conversion: %s to %s", type_name(lop.ty), type_name(rop.ty));
    }
    //opr_fold(st->loc, &st->rhs, rop);
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
                if(!opr_conv(&res, ret))
                {
                    vp_parse_error(st->expr->loc, "illegal conversion in return expression");
                }
                //opr_fold(st->expr->loc, &st->expr, res);
            }
            else if(ret != tyvoid)
            {
                vp_parse_error(st->loc, "empty return expression for function with non-void return type");
            }
            break;
        case ST_ASSIGN:
            sema_stmt_assign(st);
            break;
        case ST_BLOCK:
            sema_block(st->block, ret);
            break;
        case ST_EXPR:
            sema_expr(st->expr, NULL);
            break;
        case ST_DECL:
        {
            Decl* d = st->decl;
            if(st->decl->kind == DECL_VAR)
            {
                Type* ty = sema_var(d);
                sym_push_var(d->name, ty);
            }
            else if(st->decl->kind == DECL_NOTE)
            {
                sema_notes(st->decl->notes);
            }
            else
            {
                vp_parse_error(d->loc, "unimplemented local type declarations");
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
    
    if(!d->fn.body)
        return;

    Sym* scope = sym_enter();   /* Enter scope */
    for(uint32_t i = 0; i < vec_len(d->fn.params); i++)
    {
        Param* param = &d->fn.params[i];
        sym_push_var(param->name, sema_typespec(param->spec));
    }
    for(uint32_t i = 0; i < vec_len(d->fn.body->block); i++)
    {
        sema_stmt(d->fn.body->block[i], sema_typespec(d->fn.ret));
    }
    sym_leave(scope);   /* Leave scope */
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
            if(!opr_conv(&res, declty))
            {
                vp_parse_error(d->loc, "implicit %s to %s", type_name(inferty), type_name(declty));
            }
            d->var.expr->ty = declty;
        }
    }
    else
    {
        vp_assertX(d->var.expr, "expression");
        Operand res = sema_expr(d->var.expr, NULL);
        inferty = ty = res.ty;
        if(ty->kind == TY_array && d->var.expr->kind != EX_COMPOUND)
        {
            ty = vp_type_decay(ty);
        }
        d->var.expr->ty = inferty;
    }
    type_complete(ty);
    if(!d->var.expr || type_isptr(inferty))
    {
        ty = vp_type_decayempty(ty);
    }
    if(type_isnil(ty))
    {
        vp_parse_error(d->loc, "cannot infer type of nil");
    }
    if(vp_type_sizeof(ty) == 0)
    {
        vp_parse_error(d->loc, "variable of size 0");
    }
    /*if(d->var.expr)
    {
        opr_fold(d->loc, &d->var.expr, rop);
    }*/
    return ty;
}

/* Resolve symbols */
static void sema_resolve(Sym* sym)
{
    if(sym->state == SYM_DONE)
        return;
    else if(sym->state == SYM_PROGRESS)
    {
        vp_parse_error(sym->decl->loc, "cyclic dependency");
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
    vec_push(sorted, sym);
}

void vp_sema(Decl** decls)
{
    /* Create symbols */
    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        Decl* d = decls[i];
        if(d->kind == DECL_NOTE)
        {
            sema_notes(d->notes);
        }
        else
        {
            Sym* sym = sym_decl(d);
            vp_tab_set(&globs, sym->name, sym);
            vec_push(syms, sym);
        }
    }
    /* Resolve symbols */
    for(Sym** p = syms; p != vec_end(syms); p++)
    {
        Sym* sym = *p;
        sema_resolve(sym);
        if(sym->kind == SYM_TYPE)
        {
            type_complete(sym->type);
        }
        else if(sym->kind == SYM_FN)
        {
            sema_fn_body(sym);
        }
    }

    for(uint32_t i = 0; i < vec_len(decls); i++)
    {
        vp_print_ast(decls[i]);
    }
    vp_print_typecache();
}