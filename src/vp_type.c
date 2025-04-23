/*
** vp_type.c
** Type information
*/

#include <stdlib.h>
#include <string.h>

#include "vp_type.h"

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

const char* const vp_type_names[] = {
    "bool", "enum",
    "uint8", "uint16", "uint32", "uint64",
    "int8", "int16", "int32", "int64",
    "float", "double",
    "ptr", "array", "func", "struct",
    "void"
};

Type* vp_type_ptrof(Type* t)
{
    Type* ty = (Type*)malloc(sizeof(*ty));
    ty->kind = TY_PTR;
    ty->qual = 0;
    ty->p = t;
    return ty;
}

Type* vp_type_func(Type* ret)
{
    Type* ty = (Type*)malloc(sizeof(*ty));
    ty->kind = TY_FUNC;
    ty->fn.ret = ret;
    vec_init(&ty->fn.params);
    return ty;
}