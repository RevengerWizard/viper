/*
** vp_lex.h
** Lexical analyzer
*/

#ifndef _VP_LEX_H
#define _VP_LEX_H

#include "vp_buf.h"
#include "vp_state.h"
#include "vp_str.h"

/* Viper lexer tokens */
#define TKDEF(_, __) \
    /* Keywords */ \
    _(fn) \
    _(struct) _(union) _(enum) \
    _(var) _(const) \
    _(not) _(and) _(or) \
    _(type) _(void) _(cast) \
    _(bool) \
    _(int8) _(int16) _(int32) _(int64) \
    _(uint8) _(uint16) _(uint32) _(uint64) \
    _(usize) _(isize) \
    _(float) _(double) \
    _(true) _(false) _(nil) \
    _(return) \
    /* Symbols */ \
    __(ge, >=) __(le, <=) \
    __(lshift, <<) __(rshift, >>) \
    __(eq, ==) __(noteq, !=) \
    __(pluseq, +=) __(mineq, -=) \
    __(muleq, *=) __(diveq, /=) __(modeq, %=) \
    __(bandeq, &=) __(boreq, |=) __(bxoreq, ^=) \
    __(lshifteq, <<=) __(rshifteq, >>=) \
    __(name, <name>) __(string, <string>) \
    __(integer, <integer>) __(number, <number>) \
    __(eof, <eof>)

enum
{
    TK_OFS = 256,
#define TKENUM1(name) TK_##name,
#define TKENUM2(name, sym) TK_##name,
    TKDEF(TKENUM1, TKENUM2)
#undef TKENUM1
#undef TKENUM2
    TK_RESERVED = TK_return - TK_OFS
};

typedef int LexChar;    /* Lexical character */
typedef int LexToken;   /* Lexical token */

/* Lexical value */
typedef union LexValue
{
    Str* name;
    struct
    {
        int32_t i;
        uint32_t u;
    };
    uint64_t u64;
    double n;
} LexValue;

/* Viper lexer state */
typedef struct LexState
{
    SBuf sb;    /* String buffer for tokens */
    const char* p;  /* Current position in input buffer */
    const char* pe; /* End of input buffer */
    LexChar c;  /* Current character */
    VpReader reader;  /* VpReader callback */
    void* rdata;    /* VpReader callback data */
    LexToken prev;  /* Currently used token */
    LexToken curr;  /* Lookahead token */
    LexValue val;   /* Current token value */
    LexValue nextval;   /* Lookahead token value */
} LexState;

void vp_lex_setup(LexState* ls);
const char* vp_lex_tok2str(LexState* ls, LexToken t);
void vp_lex_error(const char* msg, ...);
void vp_lex_next(LexState* ls);
void vp_lex_init();

#endif