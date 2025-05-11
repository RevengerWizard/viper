/*
** vp_state.c
** State management
*/

#include <stdlib.h>
#include <string.h>

#include "vp_state.h"
#include "vp_lex.h"
#include "vp_mem.h"

VpState* V;

VpState* vp_state_open()
{
    V = (VpState*)vp_mem_alloc(sizeof(*V));
    memset(V, 0, sizeof(*V));
    vp_lex_init();
    return V;
}

void vp_state_close(VpState* V)
{
    free(V);
}