/*
** vp_ast.h
** Abstract Syntax Tree
*/

#ifndef _VP_AST_H
#define _VP_AST_H

#include "vp_asm.h"
#include "vp_lex.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_var.h"
#include "vp_vec.h"

typedef enum TypeSpecKind
{
    SPEC_NONE,
    SPEC_TYPE,
    SPEC_NAME,
    SPEC_FUNC,
    SPEC_ARRAY,
    SPEC_PTR,
    SPEC_TYPEOF,
    SPEC_CONST
} TypeSpecKind;

typedef struct TypeSpec
{
    TypeSpecKind kind;
    SrcLoc loc;
    union
    {
        Type* ty;
        Str* name;
        struct TypeSpec* ptr;
        struct Expr* expr;
        struct
        {
            struct TypeSpec* base;
            struct Expr* expr; /* Number of array elements */
        } arr;
        struct
        {
            vec_t(struct TypeSpec*) args;
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
    EX_CHAR,
    EX_UINTT,
    EX_INT,
    EX_UINT,
    EX_NUM,
    EX_FLO,
    EX_STR,
    EX_NAME,
    EX_COMPLIT, /* Compound literals */
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
    EX_REF,     /* &x */
    EX_DEREF,   /* *x */
    EX_SIZEOF,
    EX_ALIGNOF,
    EX_OFFSETOF,
    EX_CAST,
    EX_BITCAST,
    EX_INTCAST,
    EX_FLOATCAST,
    EX_PTRCAST,
    EX_CALL,
    EX_IDX,
    EX_FIELD,
    EX_ACCESS,
    EX__MAX
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
    SrcLoc loc;
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
    SrcLoc loc;
    Type* ty;
    union
    {
        bool b;
        uint64_t u;
        int64_t i;
        double n;
        float f;
        Str* str;
        struct Expr* unary;
        TypeSpec* spec;
        struct
        {
            Str* name;
            Scope* scope;
        };
        struct
        {
            uint64_t u;
            NumMod mod;
        } uintt;
        struct
        {
            struct Expr* lhs;
            struct Expr* rhs;
        } binop;
        struct
        {
            TypeSpec* spec;
            vec_t(Field) fields;
        } comp;
        struct
        {
            struct Expr* expr;
            vec_t(struct Expr*) args;
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
            uint64_t val;
            struct Expr* expr;
            Str* name;
        } access;
        struct
        {
            TypeSpec* spec;
            struct Expr* expr;
        } cast;
        struct
        {
            TypeSpec* spec;
            Str* name;
        } ofst;
    };
} Expr;

typedef enum StmtKind
{
    ST_ASSIGN,
    ST_ADD_ASSIGN,
    ST_SUB_ASSIGN,
    ST_MUL_ASSIGN,
    ST_DIV_ASSIGN,
    ST_MOD_ASSIGN,
    ST_BAND_ASSIGN,
    ST_BOR_ASSIGN,
    ST_BXOR_ASSIGN,
    ST_LSHIFT_ASSIGN,
    ST_RSHIFT_ASSIGN,
    ST_EXPR,
    ST_DECL,
    ST_BLOCK,
    ST_RETURN,
    ST_BREAK,
    ST_CONTINUE,
    ST_IF,
    ST_WHILE,
    ST_ASM
} StmtKind;

typedef struct Stmt
{
    StmtKind kind;
    SrcLoc loc;
    union
    {
        Expr* expr;
        struct
        {
            Expr* lhs;
            Expr* rhs;
        };
        struct Decl* decl;
        vec_t(struct Stmt*) block;
        struct
        {
            Expr* cond;
            struct Stmt* tblock;   /* Then block */
            struct Stmt* fblock;   /* Else block */
        } ifst;
        struct
        {
            Expr* cond;
            struct Stmt* body;
        } whst;
        struct
        {
            vec_t(Inst*) insts;
        } asm_;
    };
} Stmt;

typedef struct Param
{
    SrcLoc loc;
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
    SrcLoc loc;
    union
    {
        struct
        {
            vec_t(Str)* names;
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
    SrcLoc loc;
    Str* name;
    Expr* init;
    Type* ty;
    uint64_t val;
} EnumItem;

typedef struct NoteArg
{
    SrcLoc loc;
    Expr* e;
} NoteArg;

typedef struct Note
{
    SrcLoc loc;
    Str* name;
    NoteArg* args;
} Note;

typedef struct AttrArg
{
    SrcLoc loc;
    Expr* e;
} AttrArg;

typedef struct Attr
{
    SrcLoc loc;
    Str* name;
    AttrArg* args;
} Attr;

typedef enum DeclKind
{
    DECL_VAR,
    DECL_DEF,
    DECL_FN,
    DECL_TYPE,
    DECL_ALIAS,
    DECL_STRUCT,
    DECL_UNION,
    DECL_ENUM,
    DECL_NOTE
} DeclKind;

typedef struct Decl
{
    DeclKind kind;
    SrcLoc loc;
    Str* name;
    union
    {
        Note note;
        struct
        {
            TypeSpec* spec;
        } ts;
        struct
        {
            vec_t(Attr) attrs;
            Type* rett;
            TypeSpec* ret;
            Param* params;
            Stmt* body;
            vec_t(Scope)* scopes;
        } fn;
        struct
        {
            TypeSpec* spec;
            Expr* expr;
        } def;
        struct
        {
            TypeSpec* spec;
            Expr* expr;
            VarInfo* vi;
        } var;
        Aggregate* agr;
        struct
        {
            TypeSpec* spec;
            EnumItem* items;
        } enm;
    };
} Decl;

#define ast_binname(i) (vp_ast_binop[(i) - EX_BINOP])
#define ast_assignname(i) (vp_ast_assign[(i) - ST_ASSIGN])
#define ast_unaryname(i) (vp_ast_unary[(i) - EX_UNARY])

extern const char* const vp_ast_binop[];
extern const char* const vp_ast_assign[];
extern const char* const vp_ast_unary[];

/* Expressions */
Expr* vp_expr_binop(SrcLoc loc, ExprKind kind, Expr* lhs, Expr* rhs);
Expr* vp_expr_unary(SrcLoc loc, ExprKind kind, Expr* expr);
Expr* vp_expr_false(SrcLoc loc);
Expr* vp_expr_true(SrcLoc loc);
Expr* vp_expr_nil(SrcLoc loc);
Expr* vp_expr_clit(SrcLoc loc, int64_t c);
Expr* vp_expr_ilit(SrcLoc loc, int64_t i);
Expr* vp_expr_ulitt(SrcLoc loc, uint64_t u, NumMod mod);
Expr* vp_expr_ulit(SrcLoc loc, uint64_t u);
Expr* vp_expr_nlit(SrcLoc loc, double n);
Expr* vp_expr_flit(SrcLoc loc, float f);
Expr* vp_expr_str(SrcLoc loc, Str* str);
Expr* vp_expr_name(SrcLoc loc, Str* name);
Expr* vp_expr_comp(SrcLoc loc, TypeSpec* spec, Field* fields);
Expr* vp_expr_call(SrcLoc loc, Expr* e, vec_t(Expr*) args);
Expr* vp_expr_idx(SrcLoc loc, Expr* e, Expr* idx);
Expr* vp_expr_field(SrcLoc loc, Expr* e, Str* name);
Expr* vp_expr_access(SrcLoc loc, Expr* e, Str* name);
Expr* vp_expr_cast(SrcLoc loc, ExprKind kind, TypeSpec* spec, Expr* e);
Expr* vp_expr_sizeof(SrcLoc loc, TypeSpec* spec);
Expr* vp_expr_alignof(SrcLoc loc, TypeSpec* spec);
Expr* vp_expr_offsetof(SrcLoc loc, TypeSpec* spec, Str* name);

/* Statements */
Stmt* vp_stmt_assign(SrcLoc loc, StmtKind kind, Expr* lhs, Expr* rhs);
Stmt* vp_stmt_expr(SrcLoc loc, Expr* e);
Stmt* vp_stmt_decl(SrcLoc loc, Decl* d);
Stmt* vp_stmt_block(SrcLoc loc, vec_t(Stmt*) block);
Stmt* vp_stmt_return(SrcLoc loc, Expr* e);
Stmt* vp_stmt_break(SrcLoc loc, StmtKind kind);
Stmt* vp_stmt_if(SrcLoc loc, Expr* cond, Stmt* tblock, Stmt* fblock);
Stmt* vp_stmt_while(SrcLoc loc, Expr* cond, Stmt* body);
Stmt* vp_stmt_asm(SrcLoc loc, vec_t(Inst*) insts);

/* Declarations */
Decl* vp_decl_fn(SrcLoc loc, vec_t(Attr) attrs, TypeSpec* ret, Str* name, vec_t(Param) params, Stmt* body);
Decl* vp_decl_var(SrcLoc loc, Str* name, TypeSpec* spec, Expr* e);
Decl* vp_decl_def(SrcLoc loc, Str* name, TypeSpec* spec, Expr* e);
Decl* vp_decl_type(SrcLoc loc, Str* name, TypeSpec* spec);
Decl* vp_decl_alias(SrcLoc loc, Str* name, TypeSpec* spec);
Decl* vp_decl_aggr(SrcLoc loc, DeclKind kind, Str* name, Aggregate* agr);
Decl* vp_decl_note(SrcLoc loc, Note note);
Decl* vp_decl_enum(SrcLoc loc, Str* name, TypeSpec* spec, vec_t(EnumItem) items);

Aggregate* vp_aggr_new(SrcLoc loc, AggregateKind kind, AggregateItem* items);

/* Type specs */
TypeSpec* vp_typespec_name(SrcLoc loc, Str* name);
TypeSpec* vp_typespec_type(SrcLoc loc, Type* ty);
TypeSpec* vp_typespec_ptr(SrcLoc loc, TypeSpec* base);
TypeSpec* vp_typespec_arr(SrcLoc loc, TypeSpec* base, Expr* e);
TypeSpec* vp_typespec_fn(SrcLoc loc, TypeSpec* ret, vec_t(TypeSpec*) args);
TypeSpec* vp_typespec_typeof(SrcLoc loc, Expr* e);
TypeSpec* vp_typespec_const(SrcLoc loc, TypeSpec* base);

#endif