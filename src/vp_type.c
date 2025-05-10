/*
** vp_type.c
** Type information
*/

#include <stdio.h>

#include "vp_type.h"
#include "vp_def.h"
#include "vp_mem.h"
#include "vp_str.h"
#include "vp_vec.h"

/* Unsigned */
Type* tybool = &(Type){.kind = TY_BOOL};
Type* tyuint8 = &(Type){.kind = TY_UINT8};
Type* tyuint16 = &(Type){.kind = TY_UINT16};
Type* tyuint32 = &(Type){.kind = TY_UINT32};
Type* tyuint64 = &(Type){.kind = TY_UINT64};
/* Signed */
Type* tyint8 = &(Type){.kind = TY_INT8};
Type* tyint16 = &(Type){.kind = TY_INT16};
Type* tyint32 = &(Type){.kind = TY_INT32};
Type* tyint64 = &(Type){.kind = TY_INT64};
/* Floats */
Type* tyfloat = &(Type){.kind = TY_FLOAT};
Type* tydouble = &(Type){.kind = TY_DOUBLE};

Type* tyvoid = &(Type){.kind = TY_VOID};
Type* tynil = &(Type){.kind = TY_NIL};

const char* const vp_type_names[] = {
    "none", "name",
    "bool",
    "uint8", "uint16", "uint32", "uint64",
    "int8", "int16", "int32", "int64",
    "float", "double",
    "ptr", 
    "func", "array", "struct", "union",
    "void",
    "nil"
};

uint32_t vp_type_sizeof(Type* t)
{
    switch(t->kind)
    {
        case TY_BOOL:
        case TY_UINT8:
        case TY_INT8:
            return 1;
        case TY_UINT16:
        case TY_INT16:
            return 2;
        case TY_UINT32:
        case TY_INT32:
        case TY_FLOAT:
            return 4;
        case TY_UINT64:
        case TY_INT64:
        case TY_DOUBLE:
        case TY_PTR:
        case TY_FUNC:
            return 8;
        case TY_ARRAY:
            return vp_type_sizeof(t->p) * t->len;
        case TY_STRUCT:
        case TY_UNION:
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
        case TY_BOOL:
        case TY_UINT8:
        case TY_INT8:
            return 1;
        case TY_UINT16:
        case TY_INT16:
            return 2;
        case TY_UINT32:
        case TY_INT32:
        case TY_FLOAT:
            return 4;
        case TY_UINT64:
        case TY_INT64:
        case TY_DOUBLE:
        case TY_PTR:
        case TY_FUNC:
            return 8;
        case TY_ARRAY:
            return vp_type_alignof(t->p);
        case TY_STRUCT:
        case TY_UNION:
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
        case TY_UINT8:
        case TY_INT8:
            return tyuint8;
        case TY_UINT16:
        case TY_INT16:
            return tyuint16;
        case TY_UINT32:
        case TY_INT32:
            return tyuint32;
        case TY_UINT64:
        case TY_INT64:
            return tyuint64;
        default:
            vp_assertX(0, "?");
            return NULL;
    }
}

/* Decay a pointer type, if present */
Type* vp_type_decay(Type* t)
{
    if(t->kind == TY_ARRAY)
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
    Type* ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
    ty->kind = TY_NONE;
    ty->sym = sym;
    return ty;
}

Type** cacheptr;
Type** cachefunc;
Type** cachearr;

Type* vp_type_ptr(Type* t)
{
    Type* ty = NULL;
    for(uint32_t i = 0; i < vec_len(cacheptr); i++)
    {
        ty = cacheptr[i];
        if(ty == t)
            break;
    }
    if(!ty)
    {
        ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
        ty->kind = TY_PTR;
        ty->p = t;
        vec_push(cacheptr, ty);
    }
    return ty;
}

Type* vp_type_arr(Type* t, uint32_t len)
{
    if(len)
    {
        for(uint32_t i = 0; i < vec_len(cachearr); i++)
        {
            Type* ct = cachearr[i];
            if(ct->p == t && ct->len == len)
                return ct;
        }
    }
    Type* ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
    ty->kind = TY_ARRAY;
    ty->p = t;
    ty->len = len;
    if(len)
    {
        vec_push(cachearr, ty);
    }
    return ty;
}

Type* vp_type_func(Type* ret, Type** params)
{
    for(uint32_t i = 0; i < vec_len(cachefunc); i++)
    {
        Type* ct = cachefunc[i];
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
    Type* ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
    ty->kind = TY_FUNC;
    ty->fn.ret = ret;
    ty->fn.params = params;
    vec_push(cachefunc, ty);
    return ty;
}

void vp_type_struct(Str* name, Type* ty, TypeField* fields)
{
    vp_assertX(ty->kind == TY_NAME, "type name");
    ty->kind = TY_STRUCT;
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
    vp_assertX(ty->kind == TY_NAME, "type name");
    ty->kind = TY_UNION;
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

void vp_type_print(Type* ty)
{
    switch(ty->kind)
    {
        case TY_BOOL:
        case TY_UINT8:
        case TY_INT8:
        case TY_UINT16:
        case TY_INT16:
        case TY_UINT32:
        case TY_INT32:
        case TY_UINT64:
        case TY_INT64:
        case TY_FLOAT:
        case TY_DOUBLE:
        case TY_VOID:
            printf("%s", type_name(ty->kind));
            break;
        case TY_PTR:
            vp_type_print(ty->p);
            printf("*");
            break;
        case TY_ARRAY:
            vp_type_print(ty->p);
            printf("[");
            printf("%d", ty->len);
            printf("]");
            break;
        case TY_FUNC:
            printf("fn(");
            for(uint32_t i = 0; i < vec_len(ty->fn.params); i++)
            {
                Type* pt = ty->fn.params[i];
                vp_type_print(pt);
                if(i != vec_len(ty->fn.params) - 1)
                {
                    printf(", ");
                }
            }
            printf(") : ");
            vp_type_print(ty->fn.ret);
            break;
        case TY_UNION:
        case TY_STRUCT:
            if(ty->kind == TY_UNION)
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
                printf("%s : ", str_data(ty->st.fields[i].name));
                vp_type_print(ty->st.fields[i].ty);
                printf("\n");
            }
            printf("}");
            break;
        default:
            vp_assertX(0, "?");
            break;
    }
}

void vp_type_printcache()
{
    printf("\n-- cache ptr --\n");
    for(uint32_t i = 0; i < vec_len(cacheptr); i++)
    {
        vp_type_print(cacheptr[i]);
        printf("\n");
    }
    printf("\n-- cache arr --\n");
    for(uint32_t i = 0; i < vec_len(cachearr); i++)
    {
        vp_type_print(cachearr[i]);
        printf("\n");
    }
    printf("\n-- cache func --\n");
    for(uint32_t i = 0; i < vec_len(cachefunc); i++)
    {
        vp_type_print(cachefunc[i]);
        printf("\n");
    }
}