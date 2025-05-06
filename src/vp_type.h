/*
** vp_type.h
** Type information
*/

#ifndef _VP_TYPE_H
#define _VP_TYPE_H

#include "vp_str.h"

/* Viper types */
typedef enum TypeKind
{
    TY_NONE,    /* Unresolved type */
    TY_NAME,    /* Unknown type */
    /* Integers */
    TY_BOOL,
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
    TY_NIL,
    TY_PTR,
    TY_ARRAY,
    TY_FUNC,
    TY_STRUCT,
    TY_UNION,
    TY_VOID,
} TypeKind;

enum
{
    TQ_CONST = 1 << 0
};

typedef struct TypeField
{
    Str* name;
    struct Type* ty;
    uint32_t offset;
} TypeField;

typedef struct Type
{
    TypeKind kind;
    uint8_t qual;
    struct Sym* sym;
    union
    {
        struct
        {
            struct Type* p;
            uint32_t len;
        };
        struct
        {
            struct Type* ret;
            struct Type** params;
        } fn;
        struct
        {
            Str* name;
            TypeField* fields;
            uint32_t size;
            uint32_t align;
        } st;
    };
} Type;

static inline bool type_isbool(const Type* t)
{
    return t->kind == TY_BOOL;
}

static inline bool type_issigned(const Type* t)
{
    return t->kind >= TY_INT8 && t->kind <= TY_INT64;
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

static inline bool type_isptr(const Type* t)
{
    return t->kind == TY_PTR || t->kind == TY_NIL;
}

static inline bool type_ispri(const Type* t)
{
    return type_isnum(t) || t->kind == TY_PTR || t->kind == TY_BOOL;
}

static inline int type_rank(const Type* t)
{
    return (t->kind - TY_UINT8) + 1;
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
extern Type* tynil;

extern const char* const vp_type_names[];

uint32_t vp_type_sizeof(Type* t);
uint32_t vp_type_alignof(Type* t);
bool vp_type_isconv(Type* dest, Type* src);
Type* vp_type_tounsigned(Type* t);
Type* vp_type_none(struct Sym* sym);
Type* vp_type_ptr(Type* t);
Type* vp_type_arr(Type* t, uint32_t size);
Type* vp_type_func(Type* ret, Type** params);
void vp_type_struct(Type* ty, TypeField* fields);
void vp_type_union(Type* ty, TypeField* fields);

#endif