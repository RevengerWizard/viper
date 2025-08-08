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
    _(asm) \
    _(var) _(const) \
    _(not) _(and) _(or) \
    _(type) _(void) _(cast) \
    _(typeof) _(sizeof) _(alignof) _(offsetof) \
    _(bool) \
    _(int8) _(int16) _(int32) _(int64) \
    _(uint8) _(uint16) _(uint32) _(uint64) \
    _(usize) _(isize) \
    _(float) _(double) \
    _(true) _(false) _(nil) \
    _(if) _(else) \
    _(while) \
    _(break) _(continue) \
    _(return) \
    /* Symbols */ \
    __(ge, >=) __(le, <=) \
    __(lshift, <<) __(rshift, >>) \
    __(eq, ==) __(noteq, !=) \
    __(pluseq, +=) __(mineq, -=) \
    __(muleq, *=) __(diveq, /=) __(modeq, %=) \
    __(bandeq, &=) __(boreq, |=) __(bxoreq, ^=) \
    __(lshifteq, <<=) __(rshifteq, >>=) \
    __(name, <name>) __(note, <note>) \
    __(string, <string>) __(char, <char>) \
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

typedef uint32_t LexLine;   /* Lexical line */
typedef uint64_t LexOffset;  /* Lexical line offset */

typedef int LexChar;    /* Lexical character */
typedef int LexToken;   /* Lexical token */

/* Source line position */
typedef struct SrcLoc
{
    LexLine line;   /* Line number */
    LexOffset ofs;   /* Line number */
    const char* name;   /* File name */
} SrcLoc;

/* Lexical value */
typedef union LexValue
{
    Str* name;
    struct
    {
        int32_t i;
        uint32_t u;
    };
    int64_t i64;
    uint64_t u64;
    double n;
} LexValue;

typedef enum
{
    NUM_NONE = 0,
    NUM_U8 = TY_uint8,
    NUM_U16,
    NUM_U32,
    NUM_U64,
    NUM_I8,
    NUM_I16,
    NUM_I32,
    NUM_I64
} NumMod;

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
    NumMod mod;    /* Literal suffix modifier */
    const char* name;   /* File name */
    LexLine linenumber; /* Line counter */
    LexOffset lineofst; /* Line offset */
} LexState;

void vp_lex_setup(LexState* ls);
const char* vp_lex_tok2str(LexState* ls, LexToken t);
void vp_lex_error(LexState* ls, const char* msg, ...);
void vp_lex_next(LexState* ls);
void vp_lex_init();

#endif