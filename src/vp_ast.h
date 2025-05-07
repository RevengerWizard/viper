/*
** vp_ast.h
** Abstract Syntax Tree
*/

#ifndef _VP_AST_H
#define _VP_AST_H

#include "vp_str.h"
#include "vp_type.h"

enum
{
    SPEC_NONE,
    SPEC_TYPE,
    SPEC_NAME,
    SPEC_FUNC,
    SPEC_ARRAY,
    SPEC_PTR,
};

typedef struct TypeSpec
{
    uint8_t kind;
    union
    {
        Type* ty;
        Str* name;
        struct TypeSpec* ptr;
        struct
        {
            struct TypeSpec* base;
            struct Expr* expr; /* Number of array elements */
        } arr;
        struct
        {
            struct TypeSpec** args;
            struct TypeSpec* ret;
        } fn;
    };
} TypeSpec;

typedef enum ExprKind
{
    /* Literals */
    EX_TRUE,
    EX_FALSE,
    EX_NIL,
    EX_INT,
    EX_NUM,
    EX_STR,
    EX_NAME,
    EX_COMPOUND,
    /* Binary operators */
    EX_BINOP,
    EX_ADD = EX_BINOP,
    EX_SUB,
    EX_MUL,
    EX_DIV,
    EX_MOD,
    EX_BAND,
    EX_BOR,
    EX_BXOR,
    EX_LSHIFT,
    EX_RSHIFT,
    EX_EQ,
    EX_NOTEQ,
    EX_LT,
    EX_LE,
    EX_GT,
    EX_GE,
    EX_AND,
    EX_OR,
    /* Unary operators */
    EX_UNARY,
    EX_NEG = EX_UNARY,
    EX_NOT,
    EX_BNOT,
    EX_REF,
    EX_DEREF,

    EX_CAST,
    EX_CALL,
    EX_IDX,
    EX_FIELD,
} ExprKind;

typedef enum FieldKind
{
    FIELD_DEFAULT,
    FIELD_NAME,
    FIELD_IDX
} FieldKind;

typedef struct Field
{
    FieldKind kind;
    struct Expr* init;
    union
    {
        Str* name;
        struct Expr* idx;
    };
} Field;

typedef struct Expr
{
    ExprKind kind;
    union
    {
        bool b;
        int64_t i;
        double n;
        Str* name;
        struct Expr* unary;
        struct
        {
            struct Expr* lhs;
            struct Expr* rhs;
        } binop;
        struct
        {
            Field* fields;
        } comp;
        struct
        {
            struct Expr* expr;
            struct Expr** args;
        } call;
        struct
        {
            struct Expr* expr;
            struct Expr* index;
        } idx;
        struct
        {
            struct Expr* expr;
            Str* name;
        } field;
        struct
        {
            TypeSpec* spec;
            struct Expr* expr;
        } cast;
    };
} Expr;

typedef enum StmtKind
{
    ST_ASSIGN,
    ST_EXPR,
    ST_DECL,
    ST_BLOCK,
    ST_RETURN,
} StmtKind;

typedef struct Stmt
{
    StmtKind kind;
    union
    {
        Expr* expr;
        struct
        {
            Expr* lhs;
            Expr* rhs;
        };
        struct Decl* decl;
        struct Stmt** block;
    };
} Stmt;

typedef struct Param
{
    Str* name;
    TypeSpec* spec;
} Param;

typedef enum AggregateItemKind
{
    AGR_ITEM_NONE,
    AGR_ITEM_FIELD,
    AGR_ITEM_SUB
} AggregateItemKind;

typedef struct AggregateItem
{
    AggregateItemKind kind;
    union
    {
        struct
        {
            Str** names;
            TypeSpec* type;
        };
        struct Aggregate* sub;
    };
} AggregateItem;

typedef enum AggregateKind
{
    AGR_NONE,
    AGR_STRUCT,
    AGR_UNION
} AggregateKind;

typedef struct Aggregate
{
    AggregateKind kind;
    AggregateItem* items;
} Aggregate;

typedef struct EnumItem
{
    Str* name;
    Expr* init;
} EnumItem;

typedef enum DeclKind
{
    DECL_VAR,
    DECL_FN,
    DECL_TYPE,
    DECL_STRUCT,
    DECL_UNION,
    DECL_ENUM
} DeclKind;

typedef struct Decl
{
    DeclKind kind;
    Str* name;
    union
    {
        struct
        {
            TypeSpec* spec;
        } ts;
        struct
        {
            TypeSpec* ret;
            Param* params;
            Stmt* body;
        } fn;
        struct
        {
            TypeSpec* spec;
            Expr* expr;
        } var;
        Aggregate* agr;
        struct
        {
            TypeSpec* spec;
            EnumItem* items;
        } enm;
    };
} Decl;

static inline bool expr_iszero(Expr* e)
{
    return e->kind == EX_INT && e->i == 0;
}

static inline bool expr_isconst(Expr* e)
{
    return e->kind == EX_INT || e->kind == EX_NUM;
}

extern Expr* expr_false;
extern Expr* expr_true;
extern Expr* expr_nil;

Expr* vp_expr_binop(ExprKind kind, Expr* lhs, Expr* rhs);
Expr* vp_expr_unary(ExprKind kind, Expr* expr);
Expr* vp_expr_ilit(int64_t i);
Expr* vp_expr_flit(double n);
Expr* vp_expr_str(Str* str);
Expr* vp_expr_name(Str* name);
Expr* vp_expr_comp(Field* fields);
Expr* vp_expr_call(Expr* e, Expr** args);
Expr* vp_expr_idx(Expr* e, Expr* idx);
Expr* vp_expr_field(Expr* e, Str* name);
Expr* vp_expr_cast(TypeSpec* spec, Expr* e);

Stmt* vp_stmt_assign(Expr* lhs, Expr* rhs);
Stmt* vp_stmt_expr(Expr* e);
Stmt* vp_stmt_decl(Decl* d);
Stmt* vp_stmt_block(Stmt** block);
Stmt* vp_stmt_return(Expr* e);

Decl* vp_decl_fn(TypeSpec* ret, Str* name);
Decl* vp_decl_var(Str* name, TypeSpec* spec, Expr* e);
Decl* vp_decl_type(Str* name, TypeSpec* spec);
Decl* vp_decl_aggr(DeclKind kind, Str* name, Aggregate* agr);
Decl* vp_decl_enum(Str* name, TypeSpec* spec);

Aggregate* vp_aggr_new(AggregateKind kind);

TypeSpec* vp_typespec_name(Str* name);
TypeSpec* vp_typespec_type(Type* ty);
TypeSpec* vp_typespec_ptr(TypeSpec* base);
TypeSpec* vp_typespec_array(TypeSpec* base, Expr* e);
TypeSpec* vp_typespec_fn(TypeSpec* ret);

void vp_ast_print(Decl* d);

#endif