/*
** vp_type.c
** Type information
*/

#include "vp_type.h"
#include "vp_def.h"
#include "vp_mem.h"
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
    "nil", "ptr", 
    "array", "func", "struct", "union",
    "void"
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
            return vp_type_sizeof(t) * t->len;
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
    else if(type_isnum(dst) && type_isnum(src)) return true;
    else if(type_isptr(dst) && type_isptr(src))
        return dst->p == tyvoid || src->p == tyvoid;
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

Type* vp_type_none(struct Sym* sym)
{
    Type* ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
    ty->kind = TY_NONE;
    ty->sym = sym;
    return ty;
}

Type** cptr;

Type* vp_type_ptr(Type* t)
{
    Type* ty = NULL;
    for(uint32_t i = 0; i < vec_len(cptr); i++)
    {
        ty = cptr[i];
        if(ty == t)
            break;
    }
    if(!ty)
    {
        ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
        ty->kind = TY_PTR;
        ty->qual = 0;
        ty->p = t;
        vec_push(cptr, ty);
    }
    return ty;
}

typedef struct CacheFuncType
{
    Type** params;
    Type* ret;
    Type* fn;
} CacheFuncType;

CacheFuncType* cfunc;

Type* vp_type_func(Type* ret, Type** params)
{
    for(CacheFuncType* p = cfunc; p != vec_end(cfunc); p++)
    {
        if(vec_len(p->params) == vec_len(params) && p->ret == ret)
        {
            bool match = false;
            for(uint32_t i = 0; i < vec_len(params); i++)
            {
                if(p->params[i] != params[i])
                {
                    match = false;
                    break;
                }
            }
            if(match)
                return p->fn;
        }
    }
    Type* ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
    ty->kind = TY_FUNC;
    ty->fn.ret = ret;
    ty->fn.params = vp_mem_dup(params, vec_len(params) * sizeof(*params));
    vec_push(cfunc, (CacheFuncType){params, ret, ty});
    return ty;
}

typedef struct CacheArrayType
{
    Type* elem;
    uint32_t len;
    Type* arr;
} CacheArrayType;

CacheArrayType* carr;

Type* vp_type_arr(Type* t, uint32_t len)
{
    for(CacheArrayType* p = carr; p != vec_end(carr); p++)
    {
        if(p->elem == t && p->len == len)
            return p->arr;
    }
    Type* ty = (Type*)vp_mem_calloc(1, sizeof(*ty));
    ty->kind = TY_PTR;
    ty->p = t;
    ty->len = len;
    vec_push(carr, (CacheArrayType){t, len, ty});
    return ty;
}

void vp_type_struct(Type* ty, TypeField* fields)
{
    vp_assertX(ty->kind == TY_NAME, "type name");
    ty->kind = TY_STRUCT;
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

void vp_type_union(Type* ty, TypeField* fields)
{
    vp_assertX(ty->kind == TY_NAME, "type name");
    ty->kind = TY_UNION;
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