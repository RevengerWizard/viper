/*
** vp_dump.h
** Data structure print
*/

#ifndef _VP_DUMP_H
#define _VP_DUMP_H

#include <stdio.h>

#include "vp_codegen.h"
#include "vp_ast.h"

void vp_dump_bbs(Code* code);
void vp_dump_code(vec_t(Code*) codes);
void vp_dump_dot(vec_t(Code*) codes, const char* filename);
void vp_dump_strintern(void);
void vp_dump_typecache(void);
void vp_dump_decl(Decl* d);

#endif