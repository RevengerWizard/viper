/*
** vp_lex.c
** Lexical analyzer
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vp_lex.h"
#include "vp_buf.h"
#include "vp_char.h"

/* Viper lexer token names */
static const char* const lex_tokennames[] = {
    #define TKSTR(name, sym) #sym,
        TKDEF(TKSTR)
    #undef TKSTR
        NULL
    };

#define LEX_EOF (-1)

static LexChar lex_more(LexState* ls)
{
    size_t size;
    const char* p = ls->reader(ls->rdata, &size);
    if(p == NULL || size == 0)
        return LEX_EOF;
    ls->pe = p + size;
    ls->p = p + 1;
    return (LexChar)(uint8_t)p[0];
}

static LexChar lex_next(LexState* ls)
{
    return (ls->c = ls->p < ls->pe ? (LexChar)(uint8_t)*ls->p++ : lex_more(ls));
}

static void lex_save(LexState* ls, LexChar c)
{
    vp_buf_putb(&ls->sb, c);
}

static LexChar lex_savenext(LexState* ls)
{
    lex_save(ls, ls->c);
    return lex_next(ls);
}

static LexChar lex_number(LexState* ls, LexValue* v)
{
    while(vp_char_isdigit(ls->c))
    {
        lex_savenext(ls);
    }
    lex_save(ls, '\0');
    v->n = atoi(ls->sb.b);
    return TK_INTEGER;
}

typedef struct Keyword
{
    const char* name;
    int len;
    LexToken tt;
} Keyword;

static const Keyword keywords[] = {
    { "fn", 2, TK_FN },
    { "int", 3, TK_INT },
    { "return", 6, TK_RETURN },
    { NULL, 0, 0 }
};

static LexToken lex_name(const char* name, int len)
{
    const Keyword* k;
    for(k = keywords; k->name != NULL; k++)
    {
        if(k->len == len && memcmp(k->name, name, len) == 0)
            return k->tt;
    }
    return TK_NAME;
}

static LexToken lex_scan(LexState* ls, LexValue* v)
{
    vp_buf_reset(&ls->sb);
    while(true)
    {
        if(vp_char_isident(ls->c))
        {
            /* Numeric literal */
            if(vp_char_isdigit(ls->c))
            {
                return lex_number(ls, v);
            }
            do
            {
                lex_savenext(ls);
            }
            while(vp_char_isident(ls->c) || vp_char_isdigit(ls->c));
            const char* s = ls->sb.b;
            int len = sbuf_len(&ls->sb);
            lex_save(ls, '\0');
            v->name = s;
            return lex_name(s, len);
        }
        switch(ls->c)
        {
            case '\r':
            case '\n':
            case ' ':
            case '\t':
            case '\v':
            case '\f':
            {
                lex_next(ls);
                continue;
            }
            case LEX_EOF:
                return TK_EOF;
            default:
            {
                LexChar c = ls->c;
                lex_next(ls);
                return c;
            }
        }
    }
}

void vp_lex_setup(LexState* ls)
{
    ls->pe = ls->p = NULL;
    ls->prev = 0;
    lex_next(ls);   /* Read first char */
}

const char* vp_lex_tok2str(LexState* ls, LexToken t)
{
    static char buf[16];
    if(t > TK_OFS)
        return lex_tokennames[t - TK_OFS - 1];
    else if(!vp_char_iscntrl(t))
    {
        buf[0] = (char)t;
        buf[1] = '\0';
        return buf;
    }
    else
    {
        sprintf(buf, "char(%d)", t);
        return buf;
    }
}

void vp_lex_next(LexState* ls)
{
    ls->prev = lex_scan(ls, &ls->v);
}