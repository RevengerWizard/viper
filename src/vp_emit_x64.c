/*
** vp_emit_x64.c
** x64 Instruction emitter
*/

#include "vp_buf.h"
#include "vp_state.h"

void vp_emit_ret(VpState* V)
{
    char* p = vp_buf_more(&V->code, 1);
    p[0] = 0xc3;    /* ret */

    V->code.w = p + 1;
}

void vp_emit_mov_rax_imm32(VpState* V, int32_t imm)
{
    char* p = vp_buf_more(&V->code, 7);

    p[0] = 0x48;  /* REX.W prefix for 64-bit operand  */
    p[1] = 0xc7;  /* mov r/m64, imm32 */
    p[2] = 0xc0;  /* mod=11, reg=000 (rax), r/m=000 (rax) */
    
    /* Write value as little-endian */
    p[3] = (imm) & 0xFF;
    p[4] = (imm >> 8) & 0xFF;
    p[5] = (imm >> 16) & 0xFF;
    p[6] = (imm >> 24) & 0xFF;

    V->code.w = p + 7;
}