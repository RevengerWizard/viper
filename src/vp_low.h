/*
** vp_low.h
** IR lowering interface
*/

#ifndef _VP_LOW_H
#define _VP_LOW_H

#include "vp_asm.h"
#include "vp_codegen.h"
#include "vp_regalloc.h"

void vp_lowX64_caller_push(Code* code, vec_t(struct RegSave) saves, uint32_t total);
void vp_lowX64_caller_pop(Code* code, vec_t(struct RegSave) saves, uint32_t ofs);

void vp_instX64(Inst* inst);

RegSet vp_raX64(RegAlloc* ra, IR* ir);

void vp_irX64(IR* ir);
void vp_irX64_tweak(Code* code);

void vp_lowX64(vec_t(Code*) codes);

#endif