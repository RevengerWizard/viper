/*
** vp_parse.c
** Syntax analyser
*/

#include "vp_parse.h"
#include "vp_ast.h"
#include "vp_def.h"
#include "vp_lex.h"
#include "vp_err.h"
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

typedef Expr* (*ParsePrefixFn)(LexState* ls, SrcLoc loc);
typedef Expr* (*ParseInfixFn)(LexState* ls, Expr* lhs, SrcLoc loc);

typedef struct
{
    ParsePrefixFn prefix;
    ParseInfixFn infix;
    Prec prec;
} ParseRule;

/* Lexical source position */
#define lex_srcloc(ls) \
    ((SrcLoc){.line = (ls)->linenumber, .ofs = (ls)->lineofst, .name = (ls)->name})

/* Check for matching token and consume it */
static void lex_consume(LexState* ls, LexToken t)
{
    if(ls->curr == t)
    {
        vp_lex_next(ls);
        return;
    }
    const char* tokstr = vp_lex_tok2str(ls, t);
    vp_lex_error(ls, "'%s' expected", tokstr);
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

/* Consume a single name and return its name */
static Str* lex_name(LexState* ls)
{
    lex_consume(ls, TK_name);
    return ls->val.name;
}

/* Forward declarations */
static Decl* parse_decl(LexState* ls);
static ParseRule expr_rule(LexToken t);
static Expr* expr_prec(LexState* ls, Prec prec);
static Expr* expr(LexState* ls);

/* Parse literal expression */
static Expr* expr_lit(LexState* ls, SrcLoc loc)
{
    switch(ls->prev)
    {
        case TK_true:
            return vp_expr_true(loc);
        case TK_false:
            return vp_expr_false(loc);
        case TK_nil:
            return vp_expr_nil(loc);
        case TK_char:
            return vp_expr_clit(loc, ls->val.i);
        case TK_integer:
        {
            if(ls->mod != NUM_NONE)
            {
                Expr* e = vp_expr_ulitt(loc, ls->val.u64, ls->mod);
                ls->mod = NUM_NONE;
                return e;
            }
            return vp_expr_ulit(loc, ls->val.u64);
        }
        case TK_number:
            return vp_expr_nlit(loc, ls->val.n);
        case TK_string:
            return vp_expr_str(loc, ls->val.name);
        default:
            vp_assertX(false, "unknown literal");
            break;
    }
    return NULL;    /* unreachable */
}

/* Parse grouping expression */
static Expr* expr_group(LexState* ls, SrcLoc loc)
{
    UNUSED(loc);
    Expr* e = expr(ls);
    lex_consume(ls, ')');
    return e;
}

/* Forward declaration */
static Type* tok2type(LexToken tok);
static TypeSpec* parse_type(LexState* ls);

/* Parse shorten cast expression T(expr) -> cast(T, expr) */
static Expr* expr_tycast(LexState* ls, SrcLoc loc)
{
    Type* type = tok2type(ls->prev);
    vp_assertX(type, "no type");
    TypeSpec* spec = vp_typespec_type(loc, type);
    lex_consume(ls, '(');
    Expr* e = expr(ls);
    lex_consume(ls, ')');
    return vp_expr_cast(loc, EX_CAST, spec, e);
}

/* Parse cast expression */
static Expr* expr_cast(LexState* ls, SrcLoc loc)
{
    LexToken tok = ls->prev;
    lex_consume(ls, '(');
    TypeSpec* spec = parse_type(ls);
    lex_consume(ls, ',');
    Expr* e = expr(ls);
    lex_consume(ls, ')');
    ExprKind kind = EX__MAX;
    switch(tok)
    {
        case TK_cast: kind = EX_CAST; break;
        case TK_bitcast: kind = EX_BITCAST; break;
        case TK_intcast: kind = EX_INTCAST; break;
        case TK_floatcast: kind = EX_FLOATCAST; break;
        case TK_ptrcast: kind = EX_PTRCAST; break;
        default: vp_assertX(0, "?");
    }
    return vp_expr_cast(loc, kind, spec, e);
}

/* Parse sizeof expression */
static Expr* expr_sizeof(LexState* ls, SrcLoc loc)
{
    lex_consume(ls, '(');
    TypeSpec* spec = parse_type(ls);
    lex_consume(ls, ')');
    return vp_expr_sizeof(loc, spec);
}

/* Parse alignof expression */
static Expr* expr_alignof(LexState* ls, SrcLoc loc)
{
    lex_consume(ls, '(');
    TypeSpec* spec = parse_type(ls);
    lex_consume(ls, ')');
    return vp_expr_alignof(loc, spec);
}

/* Parse offsetof expression */
static Expr* expr_offsetof(LexState* ls, SrcLoc loc)
{
    lex_consume(ls, '(');
    TypeSpec* spec = parse_type(ls);
    lex_consume(ls, ',');
    Str* name = lex_name(ls);
    lex_consume(ls, ')');
    return vp_expr_offsetof(loc, spec, name);
}

/* Parse call expression */
static Expr* expr_call(LexState* ls, Expr* lhs, SrcLoc loc)
{
    vec_t(Expr*) args = NULL;
    if(!lex_check(ls, ')'))
    {
        do
        {
            Expr* e = expr(ls);
            vec_push(args, e);
        }
        while(lex_match(ls, ','));
    }
    lex_consume(ls, ')');
    return vp_expr_call(loc, lhs, args);
}

/* Parse index subscript expression */
static Expr* expr_idx(LexState* ls, Expr* lhs, SrcLoc loc)
{
    Expr* idx = expr(ls);
    lex_consume(ls, ']');
    return vp_expr_idx(loc, lhs, idx);
}

/* Parse named field expression */
static Expr* expr_dot(LexState* ls, Expr* lhs, SrcLoc loc)
{
    Str* name = lex_name(ls);
    return vp_expr_field(loc, lhs, name);
}

/* Parse unary expression */
static Expr* expr_unary(LexState* ls, SrcLoc loc)
{
    LexToken tok = ls->prev;
    Expr* expr = expr_prec(ls, PREC_UNARY);
    ExprKind kind = 0;
    switch(tok)
    {
        case '&': kind = EX_REF; break;
        case '*': kind = EX_DEREF; break;
        case '-': kind = EX_NEG; break;
        case '~': kind = EX_BNOT; break;
        case '!':
        case TK_not:
            kind = EX_NOT;
            break;
        default:
            vp_assertX(false, "unknown unary op");
            break;
    }
    return vp_expr_unary(loc, kind, expr);
}

/* Parse binary expression */
static Expr* expr_binary(LexState* ls, Expr* lhs, SrcLoc loc)
{
    LexToken tok = ls->prev;
    ParseRule rule = expr_rule(tok);
    Expr* rhs = expr_prec(ls, (Prec)(rule.prec + 1));
    ExprKind kind = 0;
    switch(tok)
    {
        case '+': kind = EX_ADD; break;
        case '-': kind = EX_SUB; break;
        case '*': kind = EX_MUL; break;
        case '/': kind = EX_DIV; break;
        case '%': kind = EX_MOD; break;
        case '&': kind = EX_BAND; break;
        case '|': kind = EX_BOR; break;
        case '^': kind = EX_BXOR; break;
        case TK_lshift: kind = EX_LSHIFT; break;
        case TK_rshift: kind = EX_RSHIFT; break;
        case TK_eq: kind = EX_EQ; break;
        case TK_noteq: kind = EX_NOTEQ; break;
        case '<': kind = EX_LT; break;
        case TK_le: kind = EX_LE; break;
        case '>': kind = EX_GT; break;
        case TK_ge: kind = EX_GE; break;
        case TK_and: kind = EX_AND; break;
        case TK_or: kind = EX_OR; break;
        default:
            vp_assertX(false, "unknown binary op");
            break;  /* Unreachable */
    }
    return vp_expr_binop(loc, kind, lhs, rhs);
}

static Field expr_field(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    if(lex_match(ls, '['))
    {
        Expr* idx = expr(ls);
        lex_consume(ls, ']');
        lex_consume(ls, '=');
        Expr* e = expr(ls);
        return (Field){.loc = loc, .kind = FIELD_IDX, .init = e, .idx = idx};
    }
    else
    {
        Expr* e = expr(ls);
        if(lex_match(ls, '='))
        {
            if(e->kind != EX_NAME)
                vp_err_error(e->loc, "expected field name");
            Expr* init = expr(ls);
            return (Field){.loc = loc, .kind = FIELD_NAME, .init = init, .name = e->name};
        }
        else
        {
            return (Field){.loc = loc, .kind = FIELD_DEFAULT, .init = e};
        }
    }
}

static Expr* expr_comp_type(LexState* ls, TypeSpec* spec)
{
    SrcLoc loc = lex_srcloc(ls);
    vec_t(Field) fields = NULL;
    while(!lex_check(ls, '}'))
    {
        Field c = expr_field(ls);
        vec_push(fields, c);
        if(!lex_match(ls, ','))
            break;
    }
    lex_consume(ls, '}');
    return vp_expr_comp(loc, spec, fields);
}

/* Parse compound literal expression */
static Expr* expr_comp(LexState* ls, SrcLoc loc)
{
    UNUSED(loc);
    return expr_comp_type(ls, NULL);
}

/* Parse name expression */
static Expr* expr_name(LexState* ls, SrcLoc loc)
{
    Str* name = ls->val.name;
    if(lex_match(ls, '{'))
    {
        return expr_comp_type(ls, vp_typespec_name(loc, name));
    }
    return vp_expr_name(loc, name);
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
        /* Literals */
        case TK_true:
        case TK_false:
        case TK_nil:
        case TK_char:
        case TK_integer:
        case TK_number:
        case TK_string:
            return PREFIX(expr_lit);
        case TK_name:
            return PREFIX(expr_name);
        case '{':
            return PREFIX(expr_comp);
        case '(':
            return RULE(expr_group, expr_call, PREC_CALL);
        case '[':
            return RULE(NULL, expr_idx, PREC_CALL);
        case '.':
            return RULE(NULL, expr_dot, PREC_CALL);
        /* Unary */
        case TK_not:
        case '!':
        case '~':
            return PREFIX(expr_unary);
        /* Binary */
        case '+':
            return OPERATOR(expr_binary, PREC_TERM);
        case '-':
            return RULE(expr_unary, expr_binary, PREC_TERM);
        case '*':
            return RULE(expr_unary, expr_binary, PREC_CALL);
        case '/':
            return OPERATOR(expr_binary, PREC_FACTOR);
        case '%':
            return OPERATOR(expr_binary, PREC_FACTOR);
        case '&':
            return RULE(expr_unary, expr_binary, PREC_BAND);
        case '|':
            return OPERATOR(expr_binary, PREC_BOR);
        case '^':
            return OPERATOR(expr_binary, PREC_BXOR);
        case TK_lshift:
        case TK_rshift:
            return OPERATOR(expr_binary, PREC_SHIFT);
        case TK_noteq:
        case TK_eq:
            return OPERATOR(expr_binary, PREC_EQUALITY);
        case '<':
        case '>':
        case TK_ge:
        case TK_le:
            return OPERATOR(expr_binary, PREC_COMPARISON);
        case TK_and:
            return OPERATOR(expr_binary, PREC_AND);
        case TK_or:
            return OPERATOR(expr_binary, PREC_OR);
        /* Casts */
        case TK_bool:
        case TK_uint8:
        case TK_uint16:
        case TK_uint32:
        case TK_uint64:
        case TK_int8:
        case TK_int16:
        case TK_int32:
        case TK_int64:
        case TK_float:
        case TK_double:
        case TK_void:
            return PREFIX(expr_tycast);
        case TK_cast:
        case TK_intcast:
        case TK_floatcast:
        case TK_ptrcast:
        case TK_bitcast:
            return PREFIX(expr_cast);
        case TK_sizeof:
            return PREFIX(expr_sizeof);
        case TK_alignof:
            return PREFIX(expr_alignof);
        case TK_offsetof:
            return PREFIX(expr_offsetof);
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
    SrcLoc loc = lex_srcloc(ls);
    vp_lex_next(ls);
    ParsePrefixFn prefix = expr_rule(ls->prev).prefix;
    if(prefix == NULL)
    {
        vp_err_error(loc, "expected expression");
    }

    Expr* expr = prefix(ls, loc);
    while(prec <= expr_rule(ls->curr).prec)
    {
        loc = lex_srcloc(ls);
        vp_lex_next(ls);
        ParseInfixFn infix = expr_rule(ls->prev).infix;
        expr = infix(ls, expr, loc);
    }
    return expr;
}

/* Parse expression */
static Expr* expr(LexState* ls)
{
    return expr_prec(ls, PREC_ASSIGN);
}

static Type* tok2type(LexToken tok)
{
    Type* ty = NULL;
    switch(tok)
    {
    case TK_void: ty = tyvoid; break;
    case TK_bool: ty = tybool; break;
    case TK_uint8: ty = tyuint8; break;
    case TK_uint16: ty = tyuint16; break;
    case TK_uint32: ty = tyuint32; break;
    case TK_uint64: ty = tyuint64; break;
    case TK_int8: ty = tyint8; break;
    case TK_int16: ty = tyint16; break;
    case TK_int32: ty = tyint32; break;
    case TK_int64: ty = tyint64; break;
    case TK_float: ty = tyfloat; break;
    case TK_double: ty = tydouble; break;
    default: break;
    }
    return ty;
}

/* Parse typeof type */
static TypeSpec* parse_typeof(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    lex_consume(ls, '(');
    Expr* e = expr(ls);
    lex_consume(ls, ')');
    return vp_typespec_typeof(loc, e);
}

/* Forward declaration */
static TypeSpec* parse_type_fn(LexState* ls);

/* Parse an array of functions type */
static TypeSpec* parse_type_fnarr(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    vec_t(TypeSpec*) arrdims = NULL;
    while(lex_check(ls, '['))
    {
        lex_consume(ls, '[');
        Expr* size = NULL;
        if(!lex_check(ls, ']')) size = expr(ls);
        lex_consume(ls, ']');
        TypeSpec* arrspec = vp_typespec_arr(loc, NULL, size);
        vec_push(arrdims, arrspec);
    }
    TypeSpec* spec = parse_type_fn(ls);
    if(arrdims)
    {
        /* Apply array dimensions, innermost first */
        for(int32_t i = vec_len(arrdims) - 1; i >= 0; i--)
        {
            TypeSpec* arrspec = arrdims[i];
            arrspec->arr.base = spec;   /* Element type for this dimension */
            spec = arrspec; /* Dimension becomes the new type */
        }
    }
    return spec;
}

/* Parse a function type */
static TypeSpec* parse_type_fn(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    vec_t(TypeSpec*) args = NULL;
    lex_consume(ls, '(');
    if(!lex_check(ls, ')'))
    {
        do
        {
            TypeSpec* spec = parse_type(ls);
            if(lex_match(ls, ':'))
            {
                if(spec->kind != SPEC_NAME)
                {
                    vp_err_error(loc, "Expected function parameter name");
                }
                spec = parse_type(ls);
            }
            vec_push(args, spec);
        }
        while(lex_match(ls, ','));
    }
    lex_consume(ls, ')');
    lex_consume(ls, ':');
    TypeSpec* ret = parse_type(ls);
    return vp_typespec_fn(loc, ret, args);
}

/* Parse base type information (keyword or name) */
static TypeSpec* parse_type(LexState* ls)
{
    TypeSpec* spec = NULL;
    if(lex_match(ls, TK_typeof))
    {
        return parse_typeof(ls);
    }

    bool isconst = lex_match(ls, TK_const);
    if(lex_match(ls, TK_name))
    {
        SrcLoc loc = lex_srcloc(ls);
        Str* name = ls->val.name;
        spec = vp_typespec_name(loc, name);
    }
    else if(lex_match(ls, TK_fn))
    {
        if(lex_check(ls, '['))
        {
            spec = parse_type_fnarr(ls);
        }
        else
        {
            spec = parse_type_fn(ls);
        }
    }
    else
    {
        SrcLoc loc = lex_srcloc(ls);
        Type* type = tok2type(ls->curr);
        if(type == NULL)
        {
            vp_err_error(loc, "unexpected token type");
        } 
        vp_lex_next(ls);    /* Skip type */
        spec = vp_typespec_type(loc, type);
    }
    SrcLoc loc = lex_srcloc(ls);
    while(lex_check(ls, '*') || lex_check(ls, '['))
    {
        if(lex_match(ls, '['))
        {
            Expr* size = NULL;
            if(!lex_check(ls, ']')) size = expr(ls);
            lex_consume(ls, ']');
            spec = vp_typespec_arr(loc, spec, size);
        }
        else
        {
            lex_consume(ls, '*');
            spec = vp_typespec_ptr(loc, spec);
        }
    }
    if(isconst)
    {
        spec = vp_typespec_const(loc, spec);
    }
    return spec;
}

/* Forward declaration */
static Stmt* parse_stmt(LexState* ls);

/* Parse note argument */
static NoteArg parse_note_arg(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    Expr* e = expr(ls);
    return (NoteArg){.loc = loc, .e = e};
}

/* Parse a #note */
static Decl* parse_note(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    vp_lex_next(ls);    /* Skip #note */
    Str* name = ls->val.name;
    NoteArg* args = NULL;
    if(lex_match(ls, '('))
    {
        while(!lex_check(ls, ')'))
        {
            NoteArg arg = parse_note_arg(ls);
            vec_push(args, arg);
            if(!lex_match(ls, ','))
                break;
        }
        lex_consume(ls, ')');
    }
    lex_consume(ls, ';');
    Note note = (Note){.loc = loc, .name = name, .args = args};
    return vp_decl_note(loc, note);
}

/* Parse code block {} */
static Stmt* parse_block(LexState* ls)
{
    vec_t(Stmt*) stmts = NULL;
    lex_consume(ls, '{');
    SrcLoc loc = lex_srcloc(ls);
    while(!lex_check(ls, '}') && !lex_check(ls, TK_eof))
    {
        Stmt* st = NULL;
        Decl* d = parse_decl(ls);
        if(d)
        {
            st = vp_stmt_decl(d->loc, d);
        }
        else
        {
            st = parse_stmt(ls);
        }
        vec_push(stmts, st);
    }
    lex_consume(ls, '}');
    return vp_stmt_block(loc, stmts);
}

/* Parse 'if' statement */
static Stmt* parse_if(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'if' */
    SrcLoc loc = lex_srcloc(ls);
    Expr* cond = expr(ls);
    Stmt* tblock = parse_block(ls);
    Stmt* fblock = NULL;
    if(lex_match(ls, TK_else))
    {
        if(lex_check(ls, TK_if))
        {
            fblock = parse_stmt(ls);            
        }
        else
        {
            fblock = parse_block(ls);
        }
    }
    return vp_stmt_if(loc, cond, tblock, fblock);
}

static uint32_t loopcount = 0;

/* Parse 'while' statement */
static Stmt* parse_while(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'while' */
    SrcLoc loc = lex_srcloc(ls);
    Expr* cond = expr(ls);
    loopcount++;
    Stmt* body = parse_block(ls);
    loopcount--;
    return vp_stmt_while(loc, cond, body);
}

/* Parse 'break' or 'continue' statement */
static Stmt* parse_breakcontinue(LexState* ls, LexToken tok)
{
    StmtKind kind = tok == TK_break ? ST_BREAK : ST_CONTINUE;
    vp_lex_next(ls);    /* Skip 'break'/'continue' */
    SrcLoc loc = lex_srcloc(ls);
    Str* name = ls->val.name;
    lex_consume(ls, ';');
    if(loopcount == 0)
    {
        vp_err_error(loc, "'%.*s' cannot be used", name->len, str_data(name));
    }
    return vp_stmt_break(loc, kind);
}

/* Parse 'return' statement */
static Stmt* parse_return(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'return' */
    SrcLoc loc = lex_srcloc(ls);
    Expr* e = expr(ls);
    lex_consume(ls, ';');
    return vp_stmt_return(loc, e);
}

/* Parse fn parameters */
static Param* parse_params(LexState* ls)
{
    Param* params = NULL;
    lex_consume(ls, '(');
    if(!lex_check(ls, ')'))
    {
        do
        {
            SrcLoc loc = lex_srcloc(ls);
            Str* name = lex_name(ls);
            lex_consume(ls, ':');
            TypeSpec* spec = parse_type(ls);
            Param p = (Param){.loc = loc, .name = name, .spec = spec};
            vec_push(params, p);
        }
        while(lex_match(ls, ','));
    }
    lex_consume(ls, ')');
    return params;
}

static Decl* parse_typedef(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'type' */
    SrcLoc loc = lex_srcloc(ls);
    Str* name = lex_name(ls);
    lex_consume(ls, '=');
    TypeSpec* spec = parse_type(ls);
    lex_consume(ls, ';');
    return vp_decl_type(loc, name, spec);
}

static Aggregate* parse_aggr(LexState* ls);

static AggregateItem parse_aggr_item(LexState* ls)
{
    if(lex_match(ls, TK_struct))
    {
        return (AggregateItem) {
            .kind = AGR_ITEM_SUB,
            .sub = parse_aggr(ls)
        };
    }
    else
    {
        SrcLoc loc = lex_srcloc(ls);
        vec_t(Str*) names = NULL;
        do
        {
            Str* name = lex_name(ls);
            vec_push(names, name);
        }
        while(lex_match(ls, ','));
        lex_consume(ls, ':');
        TypeSpec* spec = parse_type(ls);
        lex_consume(ls, ';');
        return (AggregateItem) {
            .loc = loc,
            .kind = AGR_ITEM_FIELD,
            .names = names,
            .type = spec
        };
    }
}

/* Parse struct/union item */
static Aggregate* parse_aggr(LexState* ls)
{
    lex_consume(ls, '{');
    SrcLoc loc = lex_srcloc(ls);
    vec_t(AggregateItem) items = NULL;
    while(!lex_check(ls, '}') && !lex_check(ls, TK_eof))
    {
        AggregateItem item = parse_aggr_item(ls);
        vec_push(items, item);
    }
    lex_consume(ls, '}');
    return vp_aggr_new(loc, AGR_STRUCT, items);
}

/* Parse 'struct' declaration */
static Decl* parse_struct(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'struct' */
    SrcLoc loc = lex_srcloc(ls);
    Str* name = lex_name(ls);
    Aggregate* aggr = parse_aggr(ls);
    return vp_decl_aggr(loc, DECL_STRUCT, name, aggr);
}

/* Parse enum items */
static EnumItem parse_enum_item(LexState* ls)
{
    SrcLoc loc = lex_srcloc(ls);
    Str* name = lex_name(ls);
    Expr* init = lex_match(ls, '=') ? expr(ls) : NULL;
    return (EnumItem){.loc = loc, .name = name, .init = init};
}

/* Parse 'enum' declaration */
static Decl* parse_enum(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'enum' */
    SrcLoc loc = lex_srcloc(ls);
    Str* name = lex_name(ls);
    TypeSpec* spec = NULL;
    if(lex_match(ls, ':'))
    {
        spec = parse_type(ls);
    }
    lex_consume(ls, '{');
    vec_t(EnumItem) items = NULL;
    while(!lex_check(ls, '}') && !lex_check(ls, TK_eof))
    {
        EnumItem item = parse_enum_item(ls);
        vec_push(items, item);
        if(!lex_match(ls, ','))
        {
            break;
        }
    }
    lex_consume(ls, '}');
    return vp_decl_enum(loc, name, spec, items);
}

/* Parse 'fn' declaration */
static Decl* parse_fn(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'fn' */
    SrcLoc loc = lex_srcloc(ls);
    Str* name = lex_name(ls);

    Param* params = parse_params(ls);
    lex_consume(ls, ':');
    TypeSpec* ret = parse_type(ls);

    Stmt* body;
    if(lex_match(ls, ';'))
    {
        body = NULL;
    }
    else
    {
        body = parse_block(ls);
    }
    return vp_decl_fn(loc, ret, name, params, body);
}

/* Parse 'var' */
static Decl* parse_var(LexState* ls)
{
    vp_lex_next(ls);    /* Skip 'var' */
    SrcLoc loc = lex_srcloc(ls);
    Str* name = lex_name(ls);

    TypeSpec* spec = NULL;
    if(lex_match(ls, ':'))
        spec = parse_type(ls);

    Expr* e = lex_match(ls, '=') ? expr(ls) : NULL;
    lex_consume(ls, ';');

    return vp_decl_var(loc, name, spec, e);
}

/* Parse simple statement */
static Stmt* parse_stmt_simple(LexState* ls)
{
    Expr* e = expr(ls);
    Stmt* st;
    switch(ls->curr)
    {
        case '=':
            vp_lex_next(ls);
            st = vp_stmt_assign(e->loc, ST_ASSIGN, e, expr(ls));
            break;
        case TK_pluseq:
        case TK_mineq:
        case TK_muleq:
        case TK_diveq:
        case TK_modeq:
        case TK_bandeq:
        case TK_boreq:
        case TK_bxoreq:
        case TK_lshifteq:
        case TK_rshifteq:
        {
            StmtKind kind = (ls->curr - TK_pluseq) + ST_ADD_ASSIGN;
            vp_lex_next(ls);
            st = vp_stmt_assign(e->loc, kind, e, expr(ls));
            break;
        }
        default:
            st = vp_stmt_expr(e->loc, e);
            break;
    }
    lex_consume(ls, ';');
    return st;
}

/* Parse a statement */
static Stmt* parse_stmt(LexState* ls)
{
    Stmt* st;
    switch(ls->curr)
    {
        case TK_if:
            st = parse_if(ls);
            break;
        case TK_while:
            st = parse_while(ls);
            break;
        case TK_break:
        case TK_continue:
            st = parse_breakcontinue(ls, ls->curr);
            break;
        case TK_return:
            st = parse_return(ls);
            break;
        case '{':
            st = parse_block(ls);
            break;
        default:
            st = parse_stmt_simple(ls);
            break;
    }
    return st;
}

/* Parse a declaration */
static Decl* parse_decl(LexState* ls)
{
    Decl* d = NULL;
    switch(ls->curr)
    {
        case TK_note:
            d = parse_note(ls);
            break;
        case TK_fn:
            d = parse_fn(ls);
            break;
        case TK_var:
            d = parse_var(ls);
            break;
        case TK_type:
            d = parse_typedef(ls);
            break;
        case TK_struct:
            d = parse_struct(ls);
            break;
        case TK_enum:
            d = parse_enum(ls);
            break;
        default:
            break;
    }
    return d;
}

vec_t(Decl*) vp_parse(VpState* V, LexState* ls)
{
    UNUSED(V);
    vp_lex_next(ls);    /* Read the first token into ls->curr */

    vec_t(Decl*) decls = NULL;
    while(!lex_match(ls, TK_eof))
    {
        Decl* d = parse_decl(ls);
        vec_push(decls, d);
    }
    return decls;
}