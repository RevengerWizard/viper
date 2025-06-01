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
#include "vp_err.h"
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
#define lex_iseol(ls)  (ls->c == '\n' || ls->c == '\r')

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
    ls->lineofst++;
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

/* Skip "\n", "\r", "\r\n" or "\n\r" line breaks  */
static void lex_newline(LexState* ls)
{
    LexChar old = ls->c;
    vp_assertX(lex_iseol(ls), "bad usage");
    lex_next(ls);   /* Skip "\n" or "\r" */
    if(lex_iseol(ls) && ls->c != old)
        lex_next(ls);  /* Skip "\n\r" or "\r\n" */
    if(++ls->linenumber >= VP_MAX_LINE)
        vp_lex_error(ls, "too many lines");
}

/* Parse literal suffix modifier */
static void lex_nummod(LexState* ls)
{
    if(ls->c == 'u' || ls->c == 'i')
    {
        LexChar sign = ls->c;
        lex_next(ls);
        if(ls->c == '8')
        {
            lex_next(ls);
            ls->mod = (sign == 'u') ? NUM_U8 : NUM_I8;
            return;
        }
        else if(ls->c == '1' || ls->c == '3' || ls->c == '6')
        {
            LexChar dig = ls->c;
            lex_next(ls);
            if(dig == '1' && ls->c == '6')
            {
                lex_next(ls);
                ls->mod = (sign == 'u') ? NUM_U16 : NUM_I16;
                return;
            }
            else if(dig == '3' && ls->c == '2')
            {
                lex_next(ls);
                ls->mod = (sign == 'u') ? NUM_U32: NUM_I32;
                return;
            }
            else if(dig == '6' && ls->c == '4')
            {
                lex_next(ls);
                ls->mod = (sign == 'u') ? NUM_U64 : NUM_I64;
                return;
            }
        }
    }
    else
    {
        return;
    }
    vp_lex_error(ls, "invalid suffix");
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
    while((vp_char_isident(ls->c) && (ls->c != 'u' && ls->c != 'i')) 
            || ls->c == '.' ||
           ((ls->c == '-' || ls->c == '+') && (c | 0x20) == xp))
    {
        /* Ignore underscores */
        if(ls->c == '_')
        {
            if(und)
            {
                /* Do not allow double underscores */
                vp_lex_error(ls, "malformed number literal");
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
        vp_lex_error(ls, "malformed number literal");
    }
    lex_save(ls, '\0');
    
    fmt = vp_strscan_scan((const uint8_t*)ls->sb.b, sbuf_len(&ls->sb) - 1, v);
    lex_nummod(ls);
    if(fmt == STRSCAN_NUM)
    {
        return TK_number;
    }
    else if(fmt != STRSCAN_ERROR)
    {
        return TK_integer;
    }
    vp_assertX(fmt == STRSCAN_ERROR, "unexpected number format %d", fmt);
    vp_lex_error(ls, "malformed number literal");
    return TK_eof;
}

/* Parse decimal escape '\ddd' */
static LexChar lex_dec_escape(LexState* ls, LexChar c)
{
    if(!vp_char_isdigit(c))
        goto err_xesc;
    c -= '0'; /* Decimal escape '\ddd' */
    if(vp_char_isdigit(lex_next(ls)))
    {
        c = c * 10 + (ls->c - '0');
        if(vp_char_isdigit(lex_next(ls)))
        {
            c = c * 10 + (ls->c - '0');
            if(c > 255)
            {
            err_xesc:
                return -1;
            }
            lex_next(ls);
        }
    }
    return c;
}

/* Parse hex escape '\xXX' */
static LexChar lex_hex_escape(LexState* ls)
{
    LexChar c = (lex_next(ls) & 15u) << 4;
    if(!vp_char_isdigit(ls->c))
    {
        if(!vp_char_isxdigit(ls->c))
            return -1;
        c += 9 << 4;
    }
    c += (lex_next(ls) & 15u);
    if(!vp_char_isdigit(ls->c))
    {
        if(!vp_char_isxdigit(ls->c))
            return -1;
        c += 9;
    }
    return c;
}

/* Parse unicode escapes '\uXXXX' or '\UXXXXXXXX' */
static int lex_unicode_escape(LexState* ls, int len)
{
    LexChar c = 0;
    for(int i = 0; i < len; i++)
    {
        c = (c << 4) | (ls->c & 15u);
        if(!vp_char_isdigit(ls->c))
        {
            if(!vp_char_isxdigit(ls->c))
                return -1;
            c += 9;
        }
        if(c >= 0x110000)
            return -1;  /* Out of Unicode range */
        lex_next(ls);
    }
    if(c < 0x800)
    {
        if(c < 0x80)
            return c;
        lex_save(ls, 0xc0 | (c >> 6));
    }
    else
    {
        if(c >= 0x10000)
        {
            lex_save(ls, 0xf0 | (c >> 18));
            lex_save(ls, 0x80 | ((c >> 12) & 0x3f));
        }
        else
        {
            if(c >= 0xd800 && c < 0xe000)
                return -1; /* No surrogates */
            lex_save(ls, 0xe0 | (c >> 12));
        }
        lex_save(ls, 0x80 | ((c >> 6) & 0x3f));
    }
    c = 0x80 | (c & 0x3f);
    return c;
}

/* Parse a string */
static LexChar lex_string(LexState* ls, LexValue* val)
{
    while(ls->c != '"')
    {
        switch(ls->c)
        {
            case LEX_EOF:
            case '\n':
            case '\r':
            {
                vp_lex_error(ls, "unterminated string");
            }
            case '\\':
            {
                LexChar c = lex_next(ls);  /* Skip the '\\' */
                switch(ls->c)
                {
                    case LEX_EOF: continue; /* Will raise an error next loop */
                    case '\"': c = '\"'; break;
                    case '\'': c = '\''; break;
                    case '\\': c = '\\'; break;
                    case 'a': c = '\a'; break;
                    case 'b': c = '\b'; break;
                    case 'e': c = '\033'; break;
                    case 'f': c = '\f'; break;
                    case 'n': c = '\n'; break;
                    case 'r': c = '\r'; break;
                    case 't': c = '\t'; break;
                    case 'v': c = '\v'; break;
                    case 'x':   /* Hexadecimal escape '\xXX' */
                    {
                        c = lex_hex_escape(ls);
                        if(c == -1)
                        {
                            vp_lex_error(ls, "incomplete hex escape sequence");
                        }
                        break;
                    }
                    case 'u':   /* Unicode escapes '\uXXXX' and '\uXXXXXXXX' */
                    case 'U':
                    {
                        int u = (ls->c == 'u') * 4 + (ls->c == 'U') * 8;
                        lex_next(ls);
                        c = lex_unicode_escape(ls, u);
                        if(c == -1)
                        {
                            vp_lex_error(ls, "incomplete unicode escape sequence");
                        }
                        lex_save(ls, c);
                        continue;
                    }
                    default:
                    {
                        c = lex_dec_escape(ls, c);
                        if(c == -1)
                        {
                            vp_lex_error(ls, "invalid decimal escape character");
                        }
                        lex_save(ls, c);
                        continue;
                    }
                }
                lex_save(ls, c);
                lex_next(ls);
                break;
            }
            default:
            {
                lex_savenext(ls);
                break;
            }
        }
    }
    lex_savenext(ls);   /* Skip and save " */
    Str* str = vp_str_new(ls->sb.b + 1, sbuf_len(&ls->sb) - 2);
    val->name = str;
    return TK_string;
}

/* Parse a char literal */
static LexChar lex_char(LexState* ls, LexValue* val)
{
    LexChar c = 0;
    switch(ls->c)
    {
        case LEX_EOF:
        case '\n':
        case '\r':
            vp_lex_error(ls, "unterminated char");
            break;
        case '\'':
            vp_lex_error(ls, "char literal cannot be empty");
            break;
        case '\\':
        {
            c = lex_next(ls);  /* Skip the '\\' */
            switch(ls->c)
            {
                case LEX_EOF: vp_lex_error(ls, "unterminated char"); break;
                case '\"': c = '\"'; break;
                case '\'': c = '\''; break;
                case '\\': c = '\\'; break;
                case 'a': c = '\a'; break;
                case 'b': c = '\b'; break;
                case 'e': c = '\033'; break;
                case 'f': c = '\f'; break;
                case 'n': c = '\n'; break;
                case 'r': c = '\r'; break;
                case 't': c = '\t'; break;
                case 'v': c = '\v'; break;
                case 'x':   /* Hexadecimal escape '\xXX' */
                {
                    c = lex_hex_escape(ls);
                    if(c == -1)
                    {
                        vp_lex_error(ls, "incomplete hex escape sequence");
                    }
                    break;
                }
                default:
                {
                    c = lex_dec_escape(ls, c);
                    if(c == -1)
                    {
                        vp_lex_error(ls, "invalid decimal escape character");
                    }
                    goto finish;
                }
            }
            lex_next(ls);
            break;
        }
        default:
            c = ls->c;
            lex_next(ls);
            break;
    }
finish:
    if(ls->c != '\'')
    {
        vp_lex_error(ls, "unterminated char");
    }
    lex_next(ls);   /* Skip ' */
    val->i = c;
    return TK_char;
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
                lex_newline(ls);
                ls->lineofst = 1;
                continue;
            case ' ':
            case '\t':
            case '\v':
            case '\f':
                lex_next(ls);
                continue;
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
                if(ls->c == '=') { lex_next(ls); return TK_diveq; }
                if(ls->c == '/')
                {
                    lex_next(ls);
                    while(ls->c != '\n' && ls->c != LEX_EOF)
                        lex_next(ls);
                    continue;
                }
                if(ls->c == '*')
                {
                    lex_next(ls);
                    uint32_t nesting = 1;
                    while(nesting > 0)
                    {
                        if(ls->c == LEX_EOF)
                        {
                            vp_lex_error(ls, "unterminated multi comment");
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
                        if(ls->c == '\n')
                            ls->linenumber++;
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
                if(ls->c == '=') { lex_next(ls); return TK_bxoreq; }
                return '^';
            }
            case '"':
            {
                lex_savenext(ls);
                return lex_string(ls, val);
            }
            case '\'':
            {
                lex_savenext(ls);
                return lex_char(ls, val);
            }
            case '#':
            {
                lex_next(ls);
                if(vp_char_isident(ls->c))
                {
                    do
                    {
                        lex_savenext(ls);
                    }
                    while(vp_char_isident(ls->c) || vp_char_isdigit(ls->c));
                    Str* s = vp_str_new(ls->sb.b, sbuf_len(&ls->sb));
                    val->name = s;
                    return TK_note;
                }
                return '#';
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
    ls->linenumber = 1;
    ls->lineofst = 1;
    ls->mod = NUM_NONE;
    lex_next(ls);   /* Read first char */
}

/* Convert token to string */
const char* vp_lex_tok2str(LexState* ls, LexToken t)
{
    UNUSED(ls);
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

/* Lexer error */
void vp_lex_error(LexState* ls, const char* msg, ...)
{
    va_list argp;
    va_start(argp, msg);
    vp_err_lex(ls->linenumber, ls->lineofst, ls->name, msg, argp);
    va_end(argp);
}

/* Return next lexical token */
void vp_lex_next(LexState* ls)
{
    ls->prev = ls->curr;
    ls->val = ls->nextval;
    if(ls->prev == TK_eof) return;
    ls->curr = lex_scan(ls, &ls->nextval);
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