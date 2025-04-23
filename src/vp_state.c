/*
** vp_state.c
** State management
*/

#include <stdlib.h>
#include <string.h>

#include "vp_state.h"
#include "vp_lex.h"

VpState* vp_state_open()
{
    VpState* V = (VpState*)malloc(sizeof(*V));
    memset(V, 0, sizeof(*V));
    vp_lex_init();
    return V;
}

void vp_state_close(VpState* V)
{
    free(V);
}