/*
** vp_type.h
** Type information
*/

#ifndef _VP_TYPE_H
#define _VP_TYPE_H

#include "vp_str.h"

/* Viper types */
#define TYDEF(_) \
    _(none) /* Unresolved type */ \
    _(name) /* Unknown type */ \
    /* Integers */ \
    _(bool) \
    /* Unsigned */ \
    _(uint8) _(uint16) _(uint32) _(uint64) \
    /* Signed */ \
    _(int8) _(int16) _(int32) _(int64) \
    /* Floats */ \
    _(float) _(double) \
    _(ptr) _(func) _(array) \
    _(struct) _(union) \
    _(void) _(nil) \

typedef enum TypeKind
{
#define TYENUM(name) TY_##name,
    TYDEF(TYENUM)
#undef TKENUM
} TypeKind;

typedef struct TypeField
{
    Str* name;
    struct Type* ty;
    uint32_t offset;
} TypeField;

typedef struct Type
{
    TypeKind kind;
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

static inline bool type_isbool(const Type* t)
{
    return t->kind == TY_bool;
}

static inline bool type_issigned(const Type* t)
{
    return t->kind >= TY_int8 && t->kind <= TY_int64;
}

static inline bool type_isunsigned(const Type* t)
{
    return t->kind >= TY_uint8 && t->kind <= TY_uint64;
}

static inline bool type_isint(const Type* t)
{
    return t->kind >= TY_uint8 && t->kind <= TY_int64;
}

static inline bool type_isflo(const Type* t)
{
    return t->kind == TY_float || t->kind == TY_double;
}

static inline bool type_isnum(const Type* t)
{
    return type_isint(t) || type_isflo(t);
}

static inline bool type_isptrto(const Type* t, TypeKind kind)
{
    return t->kind == TY_ptr && t->p->kind == kind;
}

static inline bool type_isptr(const Type* t)
{
    return t->kind == TY_ptr;
}

static inline bool type_isptrlike(const Type* t)
{
    return t->kind == TY_ptr || t->kind == TY_func;
}

static inline bool type_isarrempty(const Type* t)
{
    return t->kind == TY_array && t->len == 0;
}

static inline bool type_isnil(const Type* t)
{
    return t == tynil;
}

static inline bool type_isscalar(const Type* t)
{
    return TY_bool <= t->kind && t->kind <= TY_func;
}

static inline bool type_isaggr(const Type* t)
{
    return t->kind == TY_struct || t->kind == TY_union;
}

static inline bool type_isfunc(const Type* t)
{
    return t->kind == TY_func;
}

static inline int type_rank(const Type* t)
{
    return (t->kind - TY_uint8) + 1;
}

#define type_name(i) (vp_type_names[(i)->kind])

extern const char* const vp_type_names[];

uint32_t vp_type_sizeof(Type* t);
uint32_t vp_type_alignof(Type* t);
bool vp_type_isconv(Type* dst, Type* src);
bool vp_type_iscast(Type* dst, Type* src);
bool vp_type_isptrcomp(Type* lty, Type* rty);
Type* vp_type_tounsigned(Type* t);
Type* vp_type_decay(Type* t);
Type* vp_type_decayempty(Type* t);
Type* vp_type_none(struct Sym* sym);
Type* vp_type_ptr(Type* t);
Type* vp_type_arr(Type* t, uint32_t size);
Type* vp_type_func(Type* ret, Type** params);
void vp_type_struct(Str* name, Type* ty, TypeField* fields);
void vp_type_union(Str* name, Type* ty, TypeField* fields);

#endif