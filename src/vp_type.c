/*
** vp_type.c
** Type information
*/

#include "vp_type.h"
#include "vp_def.h"
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

static Type* type_alloc(TypeKind kind)
{
    Type* t = vp_arena_alloc(&V->typearena, sizeof(*t));
    t->kind = kind;
    return t;
}

uint32_t vp_type_sizeof(Type* t)
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
            return 8;
        case TY_array:
            return vp_type_sizeof(t->p) * t->len;
        case TY_struct:
        case TY_union:
            return t->st.size;
        default:
            vp_assertX(0, "?");
            break;
    }
    return 0;
}

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

bool vp_type_isconv(Type* dst, Type* src)
{
    if(dst == src) return true;
    else if(dst == tyvoid) return true;
    else if(type_isnum(dst) && type_isnum(src)) return true;
    else if(type_isptrlike(dst) && type_isnil(src)) return true;
    else if(type_isptr(dst) && type_isptr(src))
    {
        if(type_isaggr(dst->p) && type_isaggr(src->p) && 
            dst->p == src->p->st.fields[0].ty)
        {
            return true;
        }
        else
        {
            if(dst->p == src->p) return true;
            else return src->p == tyvoid;
        }
    }
    else return false;
}

bool vp_type_iscast(Type* dst, Type* src)
{
    if(vp_type_isconv(dst, src)) return true;
    else if(type_isint(dst)) return type_isptrlike(src);
    else if(type_isint(src)) return type_isptrlike(dst);
    else if(type_isptrlike(dst) && type_isptrlike(src)) return true;
    else return false;
}

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
    if(type_isarrempty(t) && type_isptr(t))
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
    Type* ty = NULL;
    for(uint32_t i = 0; i < vec_len(V->cacheptr); i++)
    {
        ty = V->cacheptr[i];
        if(ty == t)
            break;
    }
    if(!ty)
    {
        ty = type_alloc(TY_ptr);
        ty->p = t;
        vec_push(V->cacheptr, ty);
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

Type* vp_type_func(Type* ret, Type** params)
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

void vp_type_struct(Str* name, Type* ty, TypeField* fields)
{
    vp_assertX(ty->kind == TY_name, "type name");
    ty->kind = TY_struct;
    ty->st.name = name;
    ty->st.fields = fields;
    ty->st.size = 0;
    for(TypeField* it = fields; it != vec_end(fields); it++)
    {
        vp_assertX(IS_POW2(vp_type_alignof(it->ty)), "power of 2");
        it->offset = ty->st.size;
        ty->st.size = vp_type_sizeof(it->ty) + ALIGN_UP(ty->st.size, vp_type_alignof(it->ty));
        ty->st.align = MAX(ty->st.align, vp_type_alignof(it->ty));
    }
    ty->st.fields = fields;
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