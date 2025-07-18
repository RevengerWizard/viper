/*
** vp_sel.h
** Instruction selection
*/

#ifndef _VP_SEL_H
#define _VP_SEL_H

#include "vp_codegen.h"

RegSet sel_extra(RegAlloc* ra, IR* ir);

void vp_sel_tweak(Code* c);
void vp_sel(vec_t(Code*) codes);

#endif