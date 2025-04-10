/*
** vp_asm.h
** Instruction encoding
*/

#ifndef _VP_ASM_H
#define _VP_ASM_H

#include "vp_state.h"

void vp_asm_ret(VpState* V);
void vp_asm_mov_rax_imm32(VpState* V, int imm);

#endif