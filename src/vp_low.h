/*
** vp_low.h
** IR lowering interface
*/

#ifndef _VP_LOW_H
#define _VP_LOW_H

#include "vp_asm.h"
#include "vp_codegen.h"
#include "vp_regalloc.h"

void push_caller_save(vec_t(struct RegSave) saves, uint32_t total);
void pop_caller_save(vec_t(struct RegSave) saves, uint32_t ofs);

void vp_inst_x64(Inst* inst);

RegSet vp_ir_x64_extra(RegAlloc* ra, IR* ir);
void vp_ir_x64(IR* ir);
void vp_ir_x64_tweak(Code* code);

void vp_low(vec_t(Code*) codes);

#endif