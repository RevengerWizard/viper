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
#include "vp_str.h"
#include "vp_strscan.h"

/* Viper lexer token names */
static const char* const lex_tokennames[] = {
#define TKSTR1(name) #name,
#define TKSTR2(name, sym) #sym,
    TKDEF(TKSTR1, TKSTR2)
#undef TKSTR1
#undef TKSTR2
    NULL
};

#define LEX_EOF (-1)

/* Get more input from reader */
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

/* Get next character */
static LexChar lex_next(LexState* ls)
{
    return (ls->c = ls->p < ls->pe ? (LexChar)(uint8_t)*ls->p++ : lex_more(ls));
}

/* Save character */
static void lex_save(LexState* ls, LexChar c)
{
    vp_buf_putb(&ls->sb, c);
}

/* Save previous character and get next character */
static LexChar lex_savenext(LexState* ls)
{
    lex_save(ls, ls->c);
    return lex_next(ls);
}

/* Parse a number literal */
static LexChar lex_number(LexState* ls, LexValue* v)
{
    StrScanFmt fmt;
    LexChar c, xp = 'e';
    bool und = false;

    vp_assertX(vp_char_isdigit(ls->c), "bad usage");
    if((c = ls->c) == '0' && (lex_savenext(ls) | 0x20) == 'x')
        xp = 'p';
    while(vp_char_isident(ls->c) || ls->c == '.' ||
           ((ls->c == '-' || ls->c == '+') && (c | 0x20) == xp))
    {
        /* Ignore underscores */
        if(ls->c == '_')
        {
            if(und)
            {
                /* Do not allow double underscores */
                vp_lex_error("Malformed number literal");
            }
            und = true;
            lex_next(ls);
            continue;
        }
        else
        {
            und = false;
        }
        /* Do not allow leading '.' numbers */
        c = ls->c;
        lex_savenext(ls);
    }
    /* Do not allow leading '_' */
    if(und)
    {
        vp_lex_error("Malformed number literal");
    }
    lex_save(ls, '\0');

    fmt = vp_strscan_scan((const uint8_t*)ls->sb.b, sbuf_len(&ls->sb) - 1, v);

    if(fmt == STRSCAN_NUM)
    {
        return TK_number;
    }
    else if(fmt != STRSCAN_ERROR)
    {
        return TK_integer;
    }
    vp_assertX(fmt == STRSCAN_ERROR, "unexpected number format %d", fmt);
    vp_lex_error("Malformed number literal");
    return TK_eof;
}

/* Get next lexical token */
static LexToken lex_scan(LexState* ls, LexValue* val)
{
    vp_buf_reset(&ls->sb);
    while(true)
    {
        if(vp_char_isident(ls->c))
        {
            /* Numeric literal */
            if(vp_char_isdigit(ls->c))
            {
                return lex_number(ls, val);
            }
            do
            {
                lex_savenext(ls);
            }
            while(vp_char_isident(ls->c) || vp_char_isdigit(ls->c));
            Str* s = vp_str_new(ls->sb.b, sbuf_len(&ls->sb));
            val->name = s;
            if(s->reserved > 0)
            {
                return TK_OFS + s->reserved;
            }
            else
            {
                return TK_name;
            }
        }
        switch(ls->c)
        {
            case LEX_EOF:
                return TK_eof;
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
            case '<':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_le; }
                if(ls->c == '<')
                {
                    lex_next(ls);
                    if(ls->c == '=') { lex_next(ls); return TK_lshifteq; }
                    return TK_lshift;
                }
                return '<';
            }
            case '>':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_ge; };
                if(ls->c == '>')
                {
                    lex_next(ls);
                    if(ls->c == '=') { lex_next(ls); return TK_rshifteq; }
                    return TK_rshift;
                }
                return '>';
            }
            case '!':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_noteq; }
                return '!';
            }
            case '=':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_eq; };
                return '=';
            }
            case '+':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_pluseq; }
                return '+';
            }
            case '-':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_mineq; }
                return '-';
            }
            case '*':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_muleq; }
                return '*';
            }
            case '/':
            {
                lex_next(ls);
                if(ls->c == '/')
                {
                    lex_next(ls);
                    while(ls->c != '\n' && ls->c != LEX_EOF)
                        lex_next(ls);
                    continue;
                }
                if(ls->c == '=') { lex_next(ls); return TK_diveq; }
                if(ls->c == '*')
                {
                    lex_next(ls);
                    int nesting = 1;
                    while(nesting > 0)
                    {
                        if(ls->c == LEX_EOF)
                        {
                            vp_lex_error("Unterminated multi comment");
                        }
                        if(ls->c == '/' && lex_next(ls) == '*')
                        {
                            lex_next(ls);
                            nesting++;
                            continue;
                        }
                        if(ls->c == '*' && lex_next(ls) == '/')
                        {
                            lex_next(ls);
                            nesting--;
                            continue;
                        }
                        lex_next(ls);
                    }
                    continue;
                }
                return '/';
            }
            case '%':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_modeq; }
                return '%';
            }
            case '&':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_bandeq; }
                return '&';
            }
            case '|':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_boreq; }
                return '|';
            }
            case '^':
            {
                lex_next(ls);
                if(ls->c == '=') { lex_next(ls); return TK_xoreq; }
                return '^';
            }
            default:
            {
                LexChar c = ls->c;
                lex_next(ls);
                return c;
            }
        }
    }
}

/* Setup lexer */
void vp_lex_setup(LexState* ls)
{
    ls->pe = ls->p = NULL;
    ls->curr = 0;
    lex_next(ls);   /* Read first char */
}

/* Convert token to string */
const char* vp_lex_tok2str(LexState* ls, LexToken t)
{
    static char buf[32];
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
        sprintf(buf, "char(%d)", (unsigned char)t);
        return buf;
    }
}

void vp_lex_error(const char* msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    fputc('\n', stderr);
    va_end(args);
    exit(EXIT_FAILURE);
}

/* Return next lexical token */
void vp_lex_next(LexState* ls)
{
    ls->prev = ls->curr;
    ls->nextval = ls->val;
    if(ls->prev == TK_eof) return;
    ls->curr = lex_scan(ls, &ls->val);
}

/* Initialize strings for reserved words */
void vp_lex_init()
{
    for(uint32_t i = 0; i < TK_RESERVED; i++)
    {
        Str* s = vp_str_newlen(lex_tokennames[i]);
        s->reserved = (uint8_t)(i + 1);
    }
}