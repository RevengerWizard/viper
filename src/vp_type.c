/*
** vp_type.c
** Type information
*/

#include "vp_type.h"
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
/* Signed */
Type* tyint8 = &(Type){.kind = TY_int8};
Type* tyint16 = &(Type){.kind = TY_int16};
Type* tyint32 = &(Type){.kind = TY_int32};
Type* tyint64 = &(Type){.kind = TY_int64};
/* Floats */
Type* tyfloat = &(Type){.kind = TY_float};
Type* tydouble = &(Type){.kind = TY_double};

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

static int vp_type_rank(Type* t)
{
    switch(t->kind)
    {
        case TY_bool: return 1;
        /* Unsigned integers */
        case TY_uint8: return 2;
        case TY_uint16: return 3;
        case TY_uint32: return 4;
        case TY_uint64: return 5;
        /* Signed integers */
        case TY_int8: return 6;
        case TY_int16: return 7;
        case TY_int32: return 8;
        case TY_int64: return 9;
        /* Floating */
        case TY_float: return 10;
        case TY_double: return 11;
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
        case TY_float:
            return 4;
        case TY_uint64:
        case TY_int64:
        case TY_double:
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
            vp_assertX(0, "type %d", t->kind);
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
        case TY_float:
            return 4;
        case TY_uint64:
        case TY_int64:
        case TY_double:
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
            vp_type_sizeof(src) < vp_type_sizeof(dst))
        {
            return true;
        }
        return false;
    }
    else if(ty_isint(src) && ty_isflo(dst))
    {
        /* Small enough integers to float */
        if((vp_type_sizeof(src) <= 4) && dst->kind == TY_float)
        {
            return true;
        }
        /* Any small/medium integers to double */
        else if((vp_type_sizeof(src) <= 8) && dst->kind == TY_double)
        {
            return true;
        }
        return false;
    }
    else if(ty_isflo(src) && ty_isint(dst))
    {
        return false;
    }
    else if(ty_isflo(src) && ty_isflo(dst))
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
                return src->p == tyvoid || dst->p == tyvoid;
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
    if(vp_type_isconv(dst, src))
    {
        return true;
    }
    else if(ty_isint(dst))
    {
        return ty_isptrlike(src);
    }
    else if(ty_isint(src))
    {
        return ty_isptrlike(dst);
    }
    else if(ty_isptrlike(dst) && ty_isptrlike(src))
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
        if((ltybase == tyvoid && rtybase == tyuint8) ||
            (ltybase == tyuint8 && rtybase == tyvoid))
        {
            return true;
        }
    }
    return false;
}

/* Check if struct/union has duplicate fields */
bool vp_type_isdupfield(vec_t(TypeField) fields)
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
        case TY_int8: return tyint8;
        case TY_int16: return tyint16;
        case TY_int32: return tyint32;
        case TY_int64: return tyint64;
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
        int lrank = vp_type_rank(lty);
        int rrank = vp_type_rank(rty);
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
        default:
            vp_assertX(0, "?");
            return NULL;
    }
}

/* Decay a pointer type, if present */
Type* vp_type_decay(Type* t)
{
    if(t->kind == TY_array)
    {
        t = vp_type_ptr(t->p);
    }
    return t;
}

/* Decay an empty array type */
Type* vp_type_decayempty(Type* t)
{
    if(ty_isarrempty(t) && ty_isptr(t))
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

Type* vp_type_const(Type* t)
{
    /*if(ty_isconst(t))
    {
        return t;
    }*/
    return t;
}

void vp_type_struct(Str* name, Type* ty, vec_t(TypeField) fields)
{
    vp_assertX(ty->kind == TY_name, "type name");
    ty->kind = TY_struct;
    ty->st.name = name;
    ty->st.fields = fields;
    ty->st.align = 0;
    ty->st.size = 0;
    for(TypeField* it = fields; it != vec_end(fields); it++)
    {
        uint32_t align = vp_type_alignof(it->ty);
        vp_assertX(IS_POW2(align), "power of 2");
        ty->st.size = ALIGN_UP(ty->st.size, align);
        it->offset = ty->st.size;
        ty->st.size += vp_type_sizeof(it->ty);
        ty->st.align = MAX(ty->st.align, align);
    }
    ty->st.fields = fields;
    ty->st.size = ALIGN_UP(ty->st.size, ty->st.align);
}

void vp_type_union(Str* name, Type* ty, TypeField* fields)
{
    vp_assertX(ty->kind == TY_name, "type name");
    ty->kind = TY_union;
    ty->st.name = name;
    ty->st.fields = fields;
    ty->st.size = 0;
    for(TypeField* it = fields; it != vec_end(fields); it++)
    {
        it->offset = 0;
        ty->st.size = MAX(ty->st.size, vp_type_sizeof(it->ty));
        ty->st.align = MAX(ty->st.align, vp_type_alignof(it->ty));
    }
    ty->st.fields = fields;
}

/* Find index of struct/union field name */
uint32_t vp_type_fieldidx(Type* ty, Str* name)
{
    vp_assertX(ty->kind == TY_struct || ty->kind == TY_union, "struct/union");
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
    vp_assertX(ty->kind == TY_struct || ty->kind == TY_union, "struct/union");
    for(uint32_t i = 0; i < vec_len(ty->st.fields); i++)
    {
        if(ty->st.fields[i].name == name)
            return ty->st.fields[i].offset;
    }
    vp_assertX(0, "uknown field '%s'", str_data(name));
    return 0;
}