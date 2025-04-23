/*
** vp_type.h
** Type information
*/

#ifndef _VP_TYPE_H
#define _VP_TYPE_H

#include "vp_def.h"
#include "vp_vec.h"

/* Viper types */
typedef enum TypeKind
{
    /* Integers */
    TY_BOOL,
    TY_ENUM,
    /* Unsigned */
    TY_UINT8,
    TY_UINT16,
    TY_UINT32,
    TY_UINT64,
    /* Signed */
    TY_INT8,
    TY_INT16,
    TY_INT32,
    TY_INT64,
    /* Floats */
    TY_FLOAT,
    TY_DOUBLE,
    TY_PTR,
    TY_ARRAY,
    TY_FUNC,
    TY_STRUCT,
    TY_VOID,
} TypeKind;

enum
{
    TQ_CONST = 1 << 0
};

typedef vec_t(struct Type*) vec_Type_t;

typedef struct Type
{
    TypeKind kind;
    uint8_t qual;
    union
    {
        struct Type* p;
        struct
        {
            struct Type* ret;
            vec_Type_t params;
        } fn;
    };
} Type;

static inline bool type_isbool(const Type* t)
{
    return t->kind == TY_BOOL;
}

static inline bool type_isunsigned(const Type* t)
{
    return (t->kind >= TY_UINT8 && t->kind <= TY_UINT64) ||
            t->kind == TY_PTR;
}

static inline bool type_isint(const Type* t)
{
    return t->kind >= TY_UINT8 && t->kind <= TY_INT64;
}

static inline bool type_isflo(const Type* t)
{
    return t->kind == TY_FLOAT || t->kind == TY_DOUBLE;
}

static inline bool type_isnum(const Type* t)
{
    return type_isint(t) || type_isflo(t);
}

static inline bool type_ispri(const Type* t)
{
    return type_isnum(t) || t->kind == TY_PTR;
}

/* Unsigned */
extern Type* tybool;
extern Type* tyuint8;
extern Type* tyuint16;
extern Type* tyuint32;
extern Type* tyuint64;
/* Signed */
extern Type* tyint8;
extern Type* tyint16;
extern Type* tyint32;
extern Type* tyint64;
/* Floats */
extern Type* tyfloat;
extern Type* tydouble;
extern Type* tyvoid;

extern const char* const vp_type_names[];

Type* vp_type_ptrof(Type* t);
Type* vp_type_func(Type* ret);

#endif