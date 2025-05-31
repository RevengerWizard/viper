/*
** vp_dump.h
** Data structure printing
*/

#ifndef _VP_DUMP_H
#define _VP_DUMP_H

#include <stdio.h>

#include "vp_ast.h"

void vp_dump_ir(void);
void vp_dump_strintern(void);
void vp_dump_type(Type* t);
void vp_dump_typecache(void);
void vp_dump_ast(Decl* d);

#endif