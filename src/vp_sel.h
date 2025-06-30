/*
** vp_sel.h
** Instruction selection
*/

#ifndef _VP_SEL_H
#define _VP_SEL_H

#include "vp_ast.h"

RegSet sel_extra(RegAlloc* ra, IR* ir);

void vp_sel_tweak(Decl* d);
void vp_sel(Decl** decls);

#endif