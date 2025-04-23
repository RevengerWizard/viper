/*
** vp_ast.h
** Abstract Syntax Tree
*/

#ifndef _VP_AST_H
#define _VP_AST_H

#include "vp_def.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_vec.h"

typedef enum
{
    IK_SINGLE,
} InitKind;

typedef struct VarInit
{
    InitKind kind;
    struct Expr* expr;
} VarInit;

typedef struct VarInfo
{
    Str* name;
    Type* ty;
    uint16_t storage;
    union
    {
        struct
        {
            VarInit* init;
            struct VReg* vreg;
            FrameInfo ofs;  /* Local offset */
        } loc;
    };
} VarInfo;

typedef vec_t(VarInfo*) vec_VarInfo_t;

typedef struct Scope
{
    struct Scope* parent;
    vec_VarInfo_t vars;
} Scope;

typedef enum ExprKind
{
    /* Literals */
    EX_INT,
    EX_NUM,
    EX_STR,
    EX_NAME,
    /* Binary operators */
    EX_ADD,
    EX_SUB,
    EX_MUL,
    EX_DIV,
    EX_ASSIGN,
    EX_COMMA,
    /* Unary operators */
    EX_NEG,
} ExprKind;

typedef struct Expr
{
    ExprKind kind;
    Type* ty;
    union
    {
        uint64_t u;
        int64_t i;
        double n;
        struct Expr* unary;
        struct
        {
            struct Expr* lhs;
            struct Expr* rhs;
        } binop;
        struct name
        {
            Str* name;
            Scope* scope;
        } name;
    };
} Expr;

typedef enum StmtKind
{
    ST_EXPR,
    ST_BLOCK,
    ST_RETURN,
    ST_VAR,
} StmtKind;

typedef vec_t(struct Stmt*) vec_Stmt_t;

typedef struct Stmt
{
    StmtKind kind;
    union
    {
        Expr* expr;
        struct
        {
            Scope* scope;
            vec_Stmt_t stmts;
        } block;
        VarInfo* vi;
    };
} Stmt;

typedef struct Fn
{
    Type* type;
    Str* name;
    vec_VarInfo_t params;
    Stmt* body;
} Fn;

static inline bool expr_isconst(Expr* e)
{
    return e->kind == EX_INT || e->kind == EX_NUM;
}

Expr* vp_expr_binop(ExprKind kind, Type* ty, Expr* lhs, Expr* rhs);
Expr* vp_expr_unary(ExprKind kind, Type* ty, Expr* expr);
Expr* vp_expr_ilit(Type* ty, int64_t i);
Expr* vp_expr_flit(Type* ty, double n);
Expr* vp_expr_name(Str* name, Type* ty, Scope* scope);

Stmt* vp_stmt_expr(Expr* e);
Stmt* vp_stmt_block(Scope* scope);
Stmt* vp_stmt_return(Expr* e);
Stmt* vp_stmt_var(VarInfo* vi);

VarInit* vp_varinit_new(InitKind kind);

Fn* vp_fn_new(Type* type, Str* name);

#endif