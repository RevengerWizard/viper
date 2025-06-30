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

static VP_AINLINE bool ty_isbool(const Type* t)
{
    return t->kind == TY_bool;
}

static VP_AINLINE bool ty_issigned(const Type* t)
{
    return t->kind >= TY_int8 && t->kind <= TY_int64;
}

static VP_AINLINE bool ty_isunsigned(const Type* t)
{
    return t->kind >= TY_uint8 && t->kind <= TY_uint64;
}

static VP_AINLINE bool ty_isint(const Type* t)
{
    return t->kind >= TY_uint8 && t->kind <= TY_int64;
}

static VP_AINLINE bool ty_isflo(const Type* t)
{
    return t->kind == TY_float || t->kind == TY_double;
}

static VP_AINLINE bool ty_isnum(const Type* t)
{
    return ty_isint(t) || ty_isflo(t);
}

static VP_AINLINE bool ty_isptrto(const Type* t, TypeKind kind)
{
    return t->kind == TY_ptr && t->p->kind == kind;
}

static VP_AINLINE bool ty_isptr(const Type* t)
{
    return t->kind == TY_ptr;
}

static VP_AINLINE bool ty_isptrlike(const Type* t)
{
    return t->kind == TY_ptr || t->kind == TY_func;
}

static VP_AINLINE bool ty_isarrempty(const Type* t)
{
    return t->kind == TY_array && t->len == 0;
}

static VP_AINLINE bool ty_isnil(const Type* t)
{
    return t == tynil;
}

static VP_AINLINE bool ty_isscalar(const Type* t)
{
    return TY_bool <= t->kind && t->kind <= TY_func;
}

static VP_AINLINE bool ty_isaggr(const Type* t)
{
    return t->kind == TY_struct || t->kind == TY_union;
}

static VP_AINLINE bool ty_isfunc(const Type* t)
{
    return t->kind == TY_func;
}

#define type_name(i) (vp_type_names[(i)->kind])

extern const char* const vp_type_names[];

int vp_type_rank(Type* t);
uint32_t vp_type_sizeof(Type* t);
uint32_t vp_type_alignof(Type* t);
bool vp_type_isconv(Type* dst, Type* src);
bool vp_type_iscast(Type* dst, Type* src);
bool vp_type_isptrcomp(Type* lty, Type* rty);
bool vp_type_isdupfield(TypeField* fields);
Type* vp_type_builtin(int i);
Type* vp_type_common(Type* lty, Type* rty);
Type* vp_type_tounsigned(Type* t);
Type* vp_type_decay(Type* t);
Type* vp_type_decayempty(Type* t);
Type* vp_type_none(struct Sym* sym);
Type* vp_type_ptr(Type* t);
Type* vp_type_arr(Type* t, uint32_t size);
Type* vp_type_func(Type* ret, Type** params);
void vp_type_struct(Str* name, Type* ty, TypeField* fields);
void vp_type_union(Str* name, Type* ty, TypeField* fields);
uint32_t vp_type_fieldidx(Type* ty, Str* name);
uint32_t vp_type_offset(Type* ty, Str* name);

#endif