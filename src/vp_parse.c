/*
** vp_parse.c
*/

#include <stdio.h>
#include <stdlib.h>

#include "vp_parse.h"
#include "vp_lex.h"
#include "vp_state.h"
#include "vp_asm.h"

static LexValue lex_consume(LexState* ls, LexToken t)
{
    if(ls->prev == t)
    {
        LexValue v = ls->v;
        vp_lex_next(ls);
        return v;
    }
    const char* tokstr = vp_lex_tok2str(ls, t);
    const char* prev = vp_lex_tok2str(ls, ls->prev);
    fprintf(stderr, "Expected %s, got %s\n", tokstr, prev);
    exit(0);
}

static bool lex_check(LexState* ls, LexToken t)
{
    return ls->prev == t;
}

static bool lex_match(LexState* ls, LexToken t)
{
    if(ls->prev != t)
        return false;
    vp_lex_next(ls);
    return true;
}

/*void vp_parse(LexState* ls)
{
    while(lex_match(ls, TK_EOF))
    {
        parse_chunk(ls);
    }
    int i = ls->i;
    printf("ret %d\n", i);
}*/

void vp_parse(VpState* V, LexState* ls)
{
    int x;
    while(ls->prev != TK_EOF)
    {
        vp_lex_next(ls);
        LexToken t = ls->prev;
        LexValue v = ls->v;
        if(lex_check(ls, TK_INTEGER))
        {
            printf("%d\n", v.n);
            x = v.n;
        }
        else if(lex_check(ls, TK_NAME))
        {
            printf("%s\n", v.name);
        }
        else
        {
            printf("%s\n", vp_lex_tok2str(ls, t));
        }
    }
    printf("ret %d\n", x);
    vp_asm_mov_rax_imm32(V, x);
    vp_asm_ret(V);
}