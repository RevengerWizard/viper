/*
** vp_parse.c
** Syntax analyser
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vp_parse.h"
#include "vp_ast.h"
#include "vp_def.h"
#include "vp_lex.h"
#include "vp_state.h"
#include "vp_str.h"
#include "vp_type.h"
#include "vp_vec.h"

/* Precedence levels for operators */
typedef enum
{
    PREC_NONE,
    PREC_ASSIGN,     /*  =  */
    PREC_OR,         /*  or  */
    PREC_AND,        /*  and  */
    PREC_EQUALITY,   /*  == !=  */
    PREC_COMPARISON, /*  < > <= >=  */
    PREC_BOR,        /* | */
    PREC_BXOR,       /* ^ */
    PREC_BAND,       /* & */
    PREC_SHIFT,      /* << >> */
    PREC_TERM,       /*  + -  */
    PREC_FACTOR,     /*  * /  */
    PREC_UNARY,      /*  not ! - ~  */
    PREC_CALL,       /*  () */
} Prec;

typedef Expr* (*ParsePrefixFn)(LexState* ls); 
typedef Expr* (*ParseInfixFn)(LexState* ls, Expr* lhs);

typedef struct
{
    ParsePrefixFn prefix;
    ParseInfixFn infix;
    Prec prec;
} ParseRule;

static Scope* globscope;
static Scope* curscope;

static void print_stmt(Stmt* st);

static void parse_error(const char* msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    fputc('\n', stderr);
    va_end(args);
    exit(EXIT_FAILURE);
}

/* Check for matching token and consume it */
static void lex_consume(LexState* ls, LexToken t)
{
    if(ls->curr == t)
    {
        vp_lex_next(ls);
        return;
    }
    const char* tokstr = vp_lex_tok2str(ls, t);
    const char* prev = vp_lex_tok2str(ls, ls->curr);
    vp_lex_error("Expected %s, got %s\n", tokstr, prev);
}

/* Check for matching token */
static bool lex_check(LexState* ls, LexToken t)
{
    return ls->curr == t;
}

/* Check and consume token */
static bool lex_match(LexState* ls, LexToken t)
{
    if(ls->curr != t)
        return false;
    vp_lex_next(ls);
    return true;
}

/* Forward declarations */
static ParseRule expr_rule(LexToken t);
static Expr* expr_prec(LexState* ls, Prec prec);
static Expr* expr(LexState* ls);

static VarInit* var_init(LexState* ls)
{
    VarInit* vinit;
    Expr* e = expr(ls);
    vinit = vp_varinit_new(IK_SINGLE);
    vinit->expr = e;
    return vinit;
}

static VarInfo* var_add(vec_VarInfo_t* vars, Str* name, Type* type)
{
    VarInfo* vi = (VarInfo*)calloc(1, sizeof(*vi));
    vi->name = name;
    vi->ty = type;
    vec_push(vars, vi);
    return vi;
}

static int var_find(vec_VarInfo_t* vars, Str* name)
{
    for(uint32_t i = 0; i < vars->len; i++)
    {
        VarInfo* vi = vars->data[i];
        if((vi->name->len == name->len) && 
            memcmp(str_data(name), str_data(vi->name), name->len) == 0)
        {
            return i;
        }
    }
    return -1;
}

/* -- Scope handling ------------------------------------------------ */

static Scope* scope_begin(Scope* parent)
{
    Scope* scope = (Scope*)calloc(1, sizeof(*scope));
    scope->parent = parent;
    vec_init(&scope->vars);
    return scope;
}

static VarInfo* scope_find(Scope* scope, Str* name, Scope** pscope)
{
    VarInfo* vi = NULL;
    for(; scope != NULL; scope = scope->parent)
    {
        if(scope->vars.len)
        {
            int idx = var_find(&scope->vars, name);
            if(idx >= 0)
            {
                vi = scope->vars.data[idx];
                break;
            }
        }
    }
    if(pscope) *pscope = scope;
    return vi;
}

static VarInfo* scope_addvar(Scope* scope, Str* name, Type* type)
{
    if(scope->vars.len)
    {
        int idx = var_find(&scope->vars, name);
        if(idx != -1)
        {
            parse_error("%s already defined", str_data(name));
        }
    }

    VarInfo* vi = var_add(&scope->vars, name, type);
    return vi;
}

static Expr* expr_lit(LexState* ls)
{
    switch(ls->prev)
    {
        case TK_integer:
            return vp_expr_ilit(tyint32, ls->val.i);
        case TK_number:
            return vp_expr_flit(tydouble, ls->val.n);
        default:
            vp_assertX(false, "Unknown literal");
            break;
    }
    return NULL;    /* unreachable */
}

static Expr* expr_group(LexState* ls)
{
    Expr* e = expr(ls);
    lex_consume(ls, ')');
    return e;
}

/* Parse unary expression */
static Expr* expr_unary(LexState* ls)
{
    LexToken tok = ls->prev;
    Expr* expr = expr_prec(ls, PREC_UNARY);
    switch(tok)
    {
        case '-':
        {
            Type* type = expr->ty;
            if(!type_isnum(type))
            {
                parse_error("Cannot apply '-' except number types");
            }
            if(expr_isconst(expr))
            {
                expr->i = -expr->i;
                expr->ty = type;
            }
            return vp_expr_unary(EX_NEG, type, expr);
        }
        default:
            vp_assertX(false, "Unknown unary op");
            break;
    }
    return NULL;    /* unreachable */
}

static Expr* expr_new_term(ExprKind kind, Expr* lhs, Expr* rhs)
{
    Type* type = NULL;
    Type* ltype = lhs->ty;
    Type* rtype = rhs->ty;
    if(type_isnum(ltype) && type_isnum(rtype))
    {
        if(expr_isconst(lhs) && expr_isconst(rhs))
        {
            if(type_isflo(lhs->ty) || type_isflo(rhs->ty))
            {
                double lval = type_isflo(lhs->ty) ? lhs->n : (double)lhs->i;
                double rval = type_isflo(rhs->ty) ? rhs->n : (double)rhs->i;
                double val = 0;
                switch(kind)
                {
                case EX_ADD: val = lval + rval; break;
                case EX_SUB: val = lval - rval; break;
                default: vp_assertX(false, "?"); break;
                }
                type = lhs->ty;
                if(type_isflo(rhs->ty)) type = rhs->ty;
                if(type_isflo(type))
                    return vp_expr_flit(type, val);
                else
                    return vp_expr_ilit(type, (int64_t)val);
            }
            int64_t lval = lhs->i;
            int64_t rval = rhs->i;
            int64_t val = 0;
            switch(kind)
            {
            case EX_ADD: val = lval + rval; break;
            case EX_SUB: val = lval - rval; break;
            default: vp_assertX(false, "?"); break;
            }
            type = tyint32;
            return vp_expr_ilit(type, val);
        }
        type = lhs->ty;
    }
    if(type == NULL)
    {
        parse_error("Cannot apply %c", kind == EX_ADD ? '+' : '-');
    }
    return vp_expr_binop(kind, type, lhs, rhs);
}

#define CALC(kind, lval, rval, val) \
    switch(kind) \
    { \
        case EX_MUL: val = lval * rval; break; \
        case EX_DIV: val = lval / rval; break; \
        default: vp_assertX(false, "?"); break; \
    }

static Expr* expr_new_fact(ExprKind kind, Expr* lhs, Expr* rhs)
{
    if(expr_isconst(rhs) && type_isnum(rhs->ty))
    {
        if(expr_isconst(lhs) && type_isnum(lhs->ty))
        {
            if(type_isflo(lhs->ty))
            {
                vp_assertX(type_isflo(rhs->ty), "float");
                double lval = lhs->n;
                double rval = rhs->n;
                double val = 0;
                switch(kind)
                {
                case EX_MUL: val = lval * rval; break;
                case EX_DIV: val = lval / rval; break;
                default: vp_assertX(false, "?"); break;
                }
                Type* type = lhs->ty;
                if(type_isflo(rhs->ty)) type = rhs->ty;
                if(type_isflo(type))
                    return vp_expr_flit(type, val);
                else
                    return vp_expr_ilit(type, (int64_t)val);
            }

            if((kind == EX_DIV) && rhs->i == 0) goto end;

            int64_t val = 0;
            if(type_isunsigned(lhs->ty))
            {
                uint64_t lval = lhs->i;
                uint64_t rval = rhs->i;
                CALC(kind, lval, rval, val)
            }
            else
            {
                int64_t lval = lhs->i;
                int64_t rval = rhs->i;
                CALC(kind, lval, rval, val)
            }
            Type* type = lhs->ty->kind >= rhs->ty->kind ? lhs->ty : rhs->ty;
            return vp_expr_ilit(type, val);
        }
        else
        {
            if(type_isflo(rhs->ty))
            {
                vp_assertX(type_isflo(lhs->ty), "float");
                double rval = rhs->n;
                switch(kind)
                {
                case EX_MUL:
                    if(rval == 0.0)     /* 0.0 */
                        return vp_expr_binop(EX_COMMA, lhs->ty, lhs, rhs);
                    if(rval == -1.0)    /* -lhs */
                        return vp_expr_unary(EX_NEG, lhs->ty, lhs);
                    /* fallthrough */
                case EX_DIV:
                    if(rval == 1.0)
                        return lhs;
                    break;
                default: break;
                }
                goto end;
            }

            int64_t rval = rhs->i;
            switch(kind)
            {
            case EX_MUL:
                if(rval == 0)   /* 0 */
                    return vp_expr_binop(EX_COMMA, rhs->ty, lhs, rhs);
                if(rval == -1)
                    return vp_expr_unary(EX_NEG, lhs->ty, lhs);
                /* fallthrough */
            case EX_DIV:
                if(rval == 1)
                    return lhs;
            default: break;
            }
        }
    }
    else
    {
        if(expr_isconst(lhs) && type_isnum(lhs->ty))
        {
            if(type_isflo(lhs->ty))
            {
                vp_assertX(type_isflo(rhs->ty), "float");
                double lval = lhs->n;
                switch(kind)
                {
                case EX_MUL:
                    if(lval == 0.0)     /* 0.0 */
                        return vp_expr_binop(EX_COMMA, lhs->ty, rhs, lhs);
                    if(lval == 1.0)
                        return rhs;
                    if(lval == -1.0)    /* -rhs */
                        return vp_expr_unary(EX_NEG, rhs->ty, rhs);
                    break;
                default: break;
                }
                goto end;
            }

            int64_t lval = rhs->i;
            switch(kind)
            {
            case EX_MUL:
                if(lval == 0)   /* 0 */
                    return vp_expr_binop(EX_COMMA, lhs->ty, rhs, lhs);
                if(lval == -1)
                    return vp_expr_unary(EX_NEG, rhs->ty, rhs);
                /* fallthrough */
            case EX_DIV:
                if(lval == 1)
                    return rhs;
            default: break;
            }
        }
    }
end:
    if((kind == EX_DIV) && expr_isconst(rhs) 
        && type_isint(rhs->ty) && rhs->i == 0)
    {
        fprintf(stderr, "Divide by 0\n");
    }
    return vp_expr_binop(kind, lhs->ty, lhs, rhs);
}

#undef CALC

/* Parse binary expression */
static Expr* expr_binary(LexState* ls, Expr* lhs)
{
    LexToken tok = ls->prev;
    ParseRule rule = expr_rule(tok);
    Expr* rhs = expr_prec(ls, (Prec)(rule.prec + 1));
    switch(tok)
    {
        case '+':
            return expr_new_term(EX_ADD, lhs, rhs);
        case '-':
            return expr_new_term(EX_SUB, lhs, rhs);
        case '*':
            return expr_new_fact(EX_MUL, lhs, rhs);
        case '/':
            return expr_new_fact(EX_DIV, lhs, rhs);
        default:
            vp_assertX(false, "Unknown binary op");
            break;  /* Unreachable */
    }
    return NULL;
}

static Expr* expr_assign(LexState* ls, Expr* lhs)
{
    LexToken tok = ls->prev;
    ParseRule rule = expr_rule(tok);
    Expr* rhs = expr_prec(ls, (Prec)(rule.prec));
    if(tok == '=')
    {
        return vp_expr_binop(EX_ASSIGN, lhs->ty, lhs, rhs);
    }
    return NULL;
}

static Expr* expr_name(LexState* ls)
{
    Str* name = ls->val.name;
    Scope* scope;
    VarInfo* vi = scope_find(curscope, name, &scope);
    Type* type;
    if(vi == NULL)
    {
        parse_error("'%.*s' undeclared", name->len, str_data(name));
    }
    type = vi->ty;
    return vp_expr_name(name, type, scope);
}

#define NONE                    (ParseRule){ NULL, NULL, PREC_NONE }
#define RULE(pr, in, prec)      (ParseRule){ pr, in, prec }
#define INFIX(in)               (ParseRule){ NULL, in, PREC_NONE }
#define PREFIX(pr)              (ParseRule){ pr, NULL, PREC_NONE }
#define OPERATOR(in, prec)      (ParseRule){ NULL, in, prec }

/* Find the rule of a given token */
static ParseRule expr_rule(LexToken t)
{
    switch(t)
    {
        case TK_integer:
        case TK_number:
            return PREFIX(expr_lit);
        case '(':
            return RULE(expr_group, NULL, PREC_CALL);
        case '+':
            return OPERATOR(expr_binary, PREC_TERM);
        case '-':
            return RULE(expr_unary, expr_binary, PREC_TERM);
        case '*':
        case '/':
            return OPERATOR(expr_binary, PREC_FACTOR);
        case TK_name:
            return PREFIX(expr_name);
        case '=':
            return OPERATOR(expr_assign, PREC_ASSIGN);
        default:
            return NONE;
    }
}

#undef NONE
#undef RULE
#undef INFIX
#undef PREFIX
#undef OPERATOR

/* Parse precedence */
static Expr* expr_prec(LexState* ls, Prec prec)
{
    vp_lex_next(ls);
    ParsePrefixFn prefix = expr_rule(ls->prev).prefix;
    if(prefix == NULL)
    {
        parse_error("Expected expression");
    }

    Expr* expr = prefix(ls);
    while(prec <= expr_rule(ls->curr).prec)
    {
        vp_lex_next(ls);
        ParseInfixFn infix = expr_rule(ls->prev).infix;
        expr = infix(ls, expr);
    }
    return expr;
}

/* Parse expression */
static Expr* expr(LexState* ls)
{
    return expr_prec(ls, PREC_ASSIGN);
}

/* Parse type information (keyword or name) */
static Type* parse_type(LexState* ls)
{
    Type* type = NULL;
    switch(ls->curr)
    {
    case TK_bool:
        type = tybool;
        break;
    case TK_uint8:
        type = tyuint8;
        break;
    case TK_uint16:
        type = tyuint16;
        break;
    case TK_uint32:
        type = tyuint32;
        break;
    case TK_uint64:
        type = tyuint64;
        break;
    case TK_int8:
        type = tyint8;
        break;
    case TK_int16:
        type = tyint16;
        break;
    case TK_int32:
        type = tyint32;
        break;
    case TK_int64:
        type = tyint64;
        break;
    case TK_float:
        type = tyfloat;
        break;
    case TK_double:
        type = tydouble;
        break;
    default:
        break;
    }
    vp_assertX(type != NULL, "Unknown type");
    vp_lex_next(ls);    /* Skip type */
    return type;
}

/* Forward declaration */
static Stmt* parse_stmt(LexState* ls);

/* Parse code block {} */
static Stmt* parse_block(LexState* ls, Scope* scope)
{
    vec_Stmt_t stmts;
    vec_init(&stmts);

    lex_consume(ls, '{');
    while(!lex_check(ls, '}') && !lex_check(ls, TK_eof))
    {
        Stmt* st = parse_stmt(ls);
        vec_push(&stmts, st);
    }
    lex_consume(ls, '}');

    Stmt* block = vp_stmt_block(scope);
    block->block.stmts = stmts;
    return block;
}

/* Parse 'return' statement */
static Stmt* parse_return(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'return' */
    Expr* e = expr(ls);
    lex_consume(ls, ';');
    return vp_stmt_return(e);
}

/* Parse fn parameters */
static void parse_params(LexState* ls, vec_Type_t* types)
{
    lex_consume(ls, '(');
    if(!lex_check(ls, ')'))
    {
        do
        {
            lex_consume(ls, TK_name);
            lex_consume(ls, ':');
            Type* ty = parse_type(ls);
            vec_push(types, ty);
        }
        while(lex_match(ls, ','));
    }
    lex_consume(ls, ')');
}

/* Parse 'fn' declaration */
static void parse_fn(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'fn' */
    lex_consume(ls, TK_name);
    Str* name = ls->val.name;

    vec_Type_t types;
    vec_init(&types);

    parse_params(ls, &types);
    lex_consume(ls, ':');
    Type* ret = parse_type(ls);
    Type* type = vp_type_func(ret);
    if(types.len) type->fn.params = types;

    Fn* fn = vp_fn_new(type, name);

    printf("fn %s : %s\n", str_data(name), vp_type_names[ret->kind]);

    Scope* scope = scope_begin(curscope);
    Stmt* stmt = parse_block(ls, scope);
    print_stmt(stmt);
}

/* Parse 'var' */
static Stmt* parse_var(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'var' */
    lex_consume(ls, TK_name);
    Str* name = ls->val.name;

    lex_consume(ls, ':');
    Type* type = parse_type(ls);
    VarInit* vinit = lex_match(ls, '=') ? var_init(ls) : NULL;
    VarInfo* vi = scope_addvar(curscope, name, type);
    vi->ty = type;
    vi->loc.init = vinit;
    lex_consume(ls, ';');

    return vp_stmt_var(vi);
}

/* Parse expression statement */
static Stmt* parse_stmt_expr(LexState* ls)
{
    Expr* e = expr(ls);
    lex_consume(ls, ';');
    return vp_stmt_expr(e);
}

/* Parse a statement */
static Stmt* parse_stmt(LexState* ls)
{
    Stmt* st;
    switch(ls->curr)
    {
        case TK_var:
            st = parse_var(ls);
            break;
        case TK_return:
            st = parse_return(ls);
            break;
        case '{':
            st = parse_block(ls, NULL);
            break;
        default:
            st = parse_stmt_expr(ls);
            break;
    }
    return st;
}

/* Parse a declaration */
static void parse_decl(LexState* ls)
{
    switch(ls->curr)
    {
        case TK_fn:
            parse_fn(ls);
            break;
        default:
            break;
    }
}

static void print_expr(Expr* e)
{
    switch(e->kind)
    {
        case EX_INT:
        {
            int64_t i = e->i;
            printf("%lli", i);
            break;
        }
        case EX_NUM:
        {
            double n = e->n;
            printf("%g", n);
            break;
        }
        case EX_NAME:
        {
            printf("%.*s", e->name.name->len, str_data(e->name.name));
            break;
        }
        case EX_NEG:
        {
            printf("(");
            print_expr(e->unary);
            printf(")");
            break;
        }
        case EX_ADD:
        {
            printf("(");
            print_expr(e->binop.lhs);
            printf(" + ");
            print_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_SUB:
        {
            printf("(");
            print_expr(e->binop.lhs);
            printf(" - ");
            print_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_MUL:
        {
            printf("(");
            print_expr(e->binop.lhs);
            printf(" * ");
            print_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_DIV:
        {
            printf("(");
            print_expr(e->binop.lhs);
            printf(" / ");
            print_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_COMMA:
        {
            printf("(");
            print_expr(e->binop.lhs);
            printf(", ");
            print_expr(e->binop.rhs);
            printf(")");
            break;
        }
        case EX_ASSIGN:
        {
            print_expr(e->binop.lhs);
            printf(" = ");
            print_expr(e->binop.rhs);
        }
        default:
            break;
    }
}

static void print_stmt(Stmt* st)
{
    switch(st->kind)
    {
        case ST_VAR:
        {
            VarInfo* vi = st->vi;
            printf("var %s", str_data(vi->name));
            printf(" : %s = ", vp_type_names[vi->ty->kind]);
            print_expr(vi->loc.init->expr);
            printf("\n");
            break;
        }
        case ST_RETURN:
            printf("return ");
            print_expr(st->expr);
            printf("\n");
            break;
        case ST_EXPR:
        {
            print_expr(st->expr);
            printf("\n");
            break;
        }
        case ST_BLOCK:
        {
            for(int i = 0; i < st->block.stmts.len; i++)
            {
                Stmt* stm = st->block.stmts.data[i];
                print_stmt(stm);
            }
            break;
        }
        default:
            break;
    }
}

void vp_parse(VpState* V, LexState* ls)
{
    globscope = scope_begin(NULL);
    curscope = globscope;

    vp_lex_next(ls);    /* Read the first token into "next" */
    while(!lex_match(ls, TK_eof))
    {
        parse_decl(ls);
    }
}