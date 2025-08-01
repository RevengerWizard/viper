/*
** vp_parse.h
** Syntax analyser
*/

#ifndef _VP_PARSE_H
#define _VP_PARSE_H

#include "vp_ast.h"
#include "vp_lex.h"
#include "vp_state.h"

vec_t(Decl*) vp_parse(VpState* V, LexState* ls);

#endif