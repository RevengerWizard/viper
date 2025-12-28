/*
** vp_type.c
** Type information
*/

#include "vp_type.h"
#include "vp_buf.h"
#include "vp_def.h"
#include "vp_map.h"
#include "vp_mem.h"
#include "vp_str.h"
#include "vp_vec.h"
#include "vp_state.h"

/* Unsigned */
Type* tybool = &(Type){.kind = TY_bool};
Type* tyuint8 = &(Type){.kind = TY_uint8};
Type* tyuint16 = &(Type){.kind = TY_uint16};
Type* tyuint32 = &(Type){.kind = TY_uint32};
Type* tyuint64 = &(Type){.kind = TY_uint64};
Type* tyusize = &(Type){.kind = TY_usize};
/* Signed */
Type* tyint8 = &(Type){.kind = TY_int8};
Type* tyint16 = &(Type){.kind = TY_int16};
Type* tyint32 = &(Type){.kind = TY_int32};
Type* tyint64 = &(Type){.kind = TY_int64};
Type* tyisize = &(Type){.kind = TY_isize};
/* Floats */
Type* tyfloat32 = &(Type){.kind = TY_float32};
Type* tyfloat64 = &(Type){.kind = TY_float64};

Type* tyvoid = &(Type){.kind = TY_void};
Type* tynil = &(Type){.kind = TY_nil};

const char* const vp_type_names[] = {
#define TYSTR(name) #name,
    TYDEF(TYSTR)
#undef TKSTR
};

static VP_AINLINE Type* type_alloc(TypeKind kind)
{
    Type* t = vp_arena_alloc(&V->typearena, sizeof(*t));
    t->kind = kind;
    return t;
}

static uint32_t type_rank(Type* t)
{
    switch(t->kind)
    {
        case TY_bool: return 1;
        /* Unsigned integers */
        case TY_uint8: return 2;
        case TY_uint16: return 3;
        case TY_uint32: return 4;
        case TY_uint64: return 5;
        case TY_usize: return 6;
        /* Signed integers */
        case TY_int8: return 7;
        case TY_int16: return 8;
        case TY_int32: return 9;
        case TY_int64: return 10;
        case TY_isize: return 11;
        /* Floating */
        case TY_float32: return 12;
        case TY_float64: return 13;
        default: vp_assertX(0, "rank"); return 0;
    }
}

/* Get size of type */
uint32_t vp_type_sizeof(Type* t)
{
    switch(t->kind)
    {
        case TY_void:
            return 0;
        case TY_bool:
        case TY_uint8:
        case TY_int8:
            return 1;
        case TY_uint16:
        case TY_int16:
            return 2;
        case TY_uint32:
        case TY_int32:
        case TY_float32:
            return 4;
        case TY_uint64:
        case TY_int64:
        case TY_usize:
        case TY_isize:
        case TY_float64:
        case TY_ptr:
        case TY_func:
        case TY_nil:
            return 8;
        case TY_array:
            return vp_type_sizeof(t->p) * t->len;
        case TY_struct:
        case TY_union:
            return t->st.size;
        default:
            vp_assertX(0, "type '%s'", type_name(t));
            break;
    }
    return 0;
}

/* Get type alignment */
uint32_t vp_type_alignof(Type* t)
{
    switch(t->kind)
    {
        case TY_bool:
        case TY_uint8:
        case TY_int8:
            return 1;
        case TY_uint16:
        case TY_int16:
            return 2;
        case TY_uint32:
        case TY_int32:
        case TY_float32:
            return 4;
        case TY_uint64:
        case TY_int64:
        case TY_usize:
        case TY_isize:
        case TY_float64:
        case TY_ptr:
        case TY_func:
        case TY_nil:
            return 8;
        case TY_array:
            return vp_type_alignof(t->p);
        case TY_struct:
        case TY_union:
            return t->st.align;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

/* Check implicit type cast (dst <- src) is valid */
bool vp_type_isconv(Type* dst, Type* src)
{
    if(dst == src)
        return true;
    else if(dst == tyvoid)
        return true;
    else if(ty_isint(dst) && ty_isint(src))
    {
        /* Same signedness widening */
        if(ty_issigned(dst) == ty_issigned(src) &&
            vp_type_sizeof(dst) > vp_type_sizeof(src))
        {
            return true;
        }
        return false;
    }
    else if(ty_isflo(dst) && ty_isint(src))
    {
        /* Small enough integers to float */
        if(dst->kind == TY_float32 && (vp_type_sizeof(src) < 4))
        {
            return true;
        }
        /* Any small/medium integers to double */
        else if(dst->kind == TY_float64 && (vp_type_sizeof(src) < 8))
        {
            return true;
        }
        return false;
    }
    else if(ty_isint(dst) && ty_isflo(src))
    {
        return false;
    }
    else if(ty_isflo(dst) && ty_isflo(src))
    {
        return dst == src;
    }
    else if(ty_isptrlike(dst) && ty_isnil(src))
    {
        return true;
    }
    else if(ty_isptr(dst) && ty_isptr(src))
    {
        if(ty_isaggr(dst->p) && ty_isaggr(src->p) &&
            dst->p == src->p->st.fields[0].ty)
        {
            return true;
        }
        else
        {
            if(dst->p == src->p)
            {
                return true;
            }
            else
            {
                return dst->p == tyvoid || src->p == tyvoid;
            }
        }
    }
    else
    {
        return false;
    }
}

/* Check explicit type cast (dst <- src) is valid */
bool vp_type_iscast(Type* dst, Type* src)
{
    /* All implicit conversions are valid */
    if(vp_type_isconv(dst, src))
    {
        return true;
    }
    /* Bool to numeric */
    else if(ty_isnum(dst) && ty_isbool(src))
    {
        return true;
    }
    /* Numeric to bool */
    else if(ty_isbool(dst) && ty_isnum(src))
    {
        return true;
    }
    /* void* to any pointer type */
    else if(ty_isptr(dst) && ty_isptr(src) && src->p == tyvoid)
    {
        return true;
    }
    /* Any pointer to void* */
    else if(ty_isptr(dst) && ty_isptr(src) && dst->p == tyvoid)
    {
        return true;
    }
    else
    {
        return false;
    }
}

/* Check pointer compatibility */
bool vp_type_isptrcomp(Type* lty, Type* rty)
{
    if(ty_isptr(lty) && ty_isptr(rty))
    {
        Type* ltybase = lty->p;
        Type* rtybase = rty->p;
        if(ltybase == rtybase)
            return true;
        if((ltybase == tyuint8) || (rtybase == tyuint8))
        {
            return true;
        }
    }
    return false;
}

/* Get builtin type from index */
Type* vp_type_builtin(int i)
{
    switch(i)
    {
        case TY_bool: return tybool;
        case TY_uint8: return tyuint8;
        case TY_uint16: return tyuint16;
        case TY_uint32: return tyuint32;
        case TY_uint64: return tyuint64;
        case TY_usize: return tyusize;
        case TY_int8: return tyint8;
        case TY_int16: return tyint16;
        case TY_int32: return tyint32;
        case TY_int64: return tyint64;
        case TY_isize: return tyisize;
        default: vp_assertX(0, "? %d", i); return NULL;
    }
}

/* Find common type for left and right types, if any */
Type* vp_type_common(Type* lty, Type* rty)
{
    if(lty == rty)
        return lty;

    bool left = vp_type_isconv(lty, rty);
    bool right = vp_type_isconv(rty, lty);
    if(right && !left)
        return rty;
    if(left && !right)
        return lty;
    if(right && left)
    {
        uint32_t lrank = type_rank(lty);
        uint32_t rrank = type_rank(rty);
        return (rrank > lrank) ? rty : lty;
    }
    return lty;
}

/* Convert integer type to its unsigned version */
Type* vp_type_tounsigned(Type* t)
{
    switch(t->kind)
    {
        case TY_uint8:
        case TY_int8:
            return tyuint8;
        case TY_uint16:
        case TY_int16:
            return tyuint16;
        case TY_uint32:
        case TY_int32:
            return tyuint32;
        case TY_uint64:
        case TY_int64:
            return tyuint64;
        case TY_usize:
        case TY_isize:
            return tyusize;
        default:
            vp_assertX(0, "?");
            return NULL;
    }
}

/* Decay a pointer type, if present */
Type* vp_type_decay(Type* t)
{
    if(ty_isarr(t))
    {
        t = vp_type_ptr(t->p);
    }
    return t;
}

/* Decay an empty array type */
Type* vp_type_decayempty(Type* t)
{
    if(ty_isarr0(t) && ty_isptr(t))
    {
        return vp_type_ptr(vp_type_decayempty(t->p));
    }
    else
    {
        return t;
    }
}

Type* vp_type_none(struct Sym* sym)
{
    Type* ty = type_alloc(TY_none);
    ty->sym = sym;
    return ty;
}

Type* vp_type_ptr(Type* t)
{
    Type* ty = vp_map_get(&V->cacheptr, t);
    if(!ty)
    {
        ty = type_alloc(TY_ptr);
        ty->p = t;
        vp_map_put(&V->cacheptr, t, ty);
    }
    return ty;
}

Type* vp_type_arr(Type* t, uint32_t len)
{
    if(len)
    {
        for(uint32_t i = 0; i < vec_len(V->cachearr); i++)
        {
            Type* ct = V->cachearr[i];
            if(ct->p == t && ct->len == len)
                return ct;
        }
    }
    Type* ty = type_alloc(TY_array);
    ty->p = t;
    ty->len = len;
    if(len)
    {
        vec_push(V->cachearr, ty);
    }
    return ty;
}

Type* vp_type_func(Type* ret, vec_t(Type*) params)
{
    for(uint32_t i = 0; i < vec_len(V->cachefunc); i++)
    {
        Type* ct = V->cachefunc[i];
        if(vec_len(ct->fn.params) == vec_len(params) && ct->fn.ret == ret)
        {
            bool match = true;
            for(uint32_t j = 0; j < vec_len(ct->fn.params); j++)
            {
                if(params[j] != ct->fn.params[j])
                {
                    match = false;
                    break;
                }
            }
            if(match)
                return ct;
        }
    }
    Type* ty = type_alloc(TY_func);
    ty->fn.ret = ret;
    ty->fn.params = params;
    vec_push(V->cachefunc, ty);
    return ty;
}

Type* vp_type_qual(Type* t, uint8_t qual)
{
    if(ty_qual(t) == qual)
        return t;

    /* Composite types: qualify base recursively, rebuild in their cache */
    switch(t->kind)
    {
        case TY_ptr:
            return vp_type_ptr(vp_type_qual(t->p, qual));
        case TY_array:
            return vp_type_arr(vp_type_qual(t->p, qual), t->len);
        case TY_func:
        default:
            break;
    }

    /* Base types: use the qualified cache */
    Type* ty = vp_map_get(&V->cachequal, t);
    if(!ty || ty_qual(ty) != qual)
    {
        ty = type_alloc(t->kind);
        *ty = *t;
        ty->qual = qual;
        vp_map_put(&V->cachequal, t, ty);
    }
    return ty;
}

static void type_fieldsadd(vec_t(TypeField)* fields, Type* ty, uint32_t base)
{
    vp_assertX(ty_isaggr(ty), "struct/union");
    for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
    {
        TypeField f = ty->st.fields[i];
        f.offset += base;
        vec_push(*fields, f);
    }
}

void vp_type_struct(Str* name, Type* ty, vec_t(TypeField) fields)
{
    vp_assertX(ty->kind == TY_name, "type name");
    ty->kind = TY_struct;
    ty->st.name = name;
    vec_t(TypeField) newfields = vec_init(TypeField);
    uint32_t totalign = 0;
    uint32_t totsize = 0;
    for(TypeField* it = fields; it != vec_end(fields); it++)
    {
        uint32_t align = vp_type_alignof(it->ty);
        vp_assertX(IS_POW2(align), "power of 2");
        totsize = ALIGN_UP(totsize, align);
        if(it->name)
        {
            it->offset = totsize;
            vec_push(newfields, *it);
        }
        else
        {
            type_fieldsadd(&newfields, it->ty, totsize);
        }
        totsize += vp_type_sizeof(it->ty);
        totalign = MAX(totalign, align);
    }
    totsize = ALIGN_UP(totsize, totalign);
    ty->st.fields = newfields;
    ty->st.align = totalign;
    ty->st.size = totsize;
}

void vp_type_union(Str* name, Type* ty, TypeField* fields)
{
    vp_assertX(ty->kind == TY_name, "type name");
    ty->kind = TY_union;
    ty->st.name = name;
    vec_t(TypeField) newfields = vec_init(TypeField);
    uint32_t totalign = 0;
    uint32_t totsize = 0;
    for(TypeField* it = fields; it != vec_end(fields); it++)
    {
        if(it->name)
        {
            it->offset = 0;
            vec_push(newfields, *it);
        }
        else
        {
            type_fieldsadd(&newfields, it->ty, 0);
        }
        totsize = MAX(totsize, vp_type_sizeof(it->ty));
        totalign = MAX(totalign, vp_type_alignof(it->ty));
    }
    totsize = ALIGN_UP(totsize, totalign);
    ty->st.fields = newfields;
    ty->st.align = totalign;
    ty->st.size = totsize;
}

/* Find index of struct/union field name */
uint32_t vp_type_fieldidx(Type* ty, Str* name)
{
    vp_assertX(ty_isaggr(ty), "struct/union");
    for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
    {
        if(ty->st.fields[i].name == name)
            return i;
    }
    return (uint32_t)-1;
}

/* Get offset of a field already present */
uint32_t vp_type_offset(Type* ty, Str* name)
{
    vp_assertX(ty_isaggr(ty), "struct/union");
    for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
    {
        if(ty->st.fields[i].name == name)
            return ty->st.fields[i].offset;
    }
    vp_assertX(0, "uknown field '%s'", str_data(name));
    return 0;
}

static void type_tostr(Type* ty, SBuf* sb)
{
    if(ty_isconst(ty))
    {
        vp_buf_putlit(sb, "const ");
    }
    switch(ty->kind)
    {
        case TY_none:
            vp_assertX(0, "none type");
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
        {
            const char* s = type_name(ty);
            vp_buf_putmem(sb, s, strlen(s));
            break;
        }
        case TY_ptr:
            type_tostr(ty->p, sb);
            vp_buf_putb(sb, '*');
            break;
        case TY_array:
            type_tostr(ty->p, sb);
            vp_buf_putb(sb, '[');
            if(ty->len)
            {
                char num[32];
                int len = snprintf(num, sizeof(num), "%u", ty->len);
                vp_buf_putmem(sb, num, len);
            }
            vp_buf_putb(sb, ']');
            break;
        case TY_func:
            vp_buf_putlit(sb, "fn(");
            for(uint32_t i = 0; i < vec_len(ty->fn.params); i++)
            {
                if(i > 0) vp_buf_putlit(sb, ", ");
                type_tostr(ty->fn.params[i], sb);
            }
            vp_buf_putlit(sb, ") : ");
            type_tostr(ty->fn.ret, sb);
            break;
        case TY_struct:
            vp_buf_putlit(sb, "struct ");
            if(ty->st.name)
                vp_buf_putmem(sb, str_data(ty->st.name), ty->st.name->len);
            break;
        case TY_union:
            vp_buf_putlit(sb, "union ");
            if(ty->st.name)
                vp_buf_putmem(sb, str_data(ty->st.name), ty->st.name->len);
            break;
        default:
            vp_assertX(0, "?");
            break;
    }
}

/* Convert type to readable string */
Str* vp_type_tostr(Type* ty)
{
    SBuf sb;
    vp_buf_init(&sb);
    type_tostr(ty, &sb);
    return vp_buf_str(&sb);
}