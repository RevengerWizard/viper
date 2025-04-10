/*
** vp_lex.h
** Lexical analyzer
*/

#ifndef _VP_LEX_H
#define _VP_LEX_H

#include "vp_buf.h"
#include "vp_state.h"

/* Viper lexer tokens */
#define TKDEF(_) \
    _(FN, fn) _(RETURN, return) \
    _(INT, int) \
    _(NAME, <name>) _(INTEGER, <integer>) \
    _(EOF, <eof>)

enum
{
    TK_OFS = 256,
#define TKENUM(name, sym) TK_##name,
    TKDEF(TKENUM)
#undef TKENUM
    TK_RESERVED = TK_RETURN - TK_OFS
};

typedef int LexChar;
typedef int LexToken;

typedef union LexValue
{
    const char* name;
    int n;
} LexValue;

typedef struct LexState
{
    SBuf sb;
    const char* p;
    const char* pe;
    Reader reader;
    void* rdata;
    LexChar c;
    LexToken prev;
    LexValue v;
} LexState;

void vp_lex_setup(LexState* ls);
const char* vp_lex_tok2str(LexState* ls, LexToken t);
void vp_lex_next(LexState* ls);

#endif