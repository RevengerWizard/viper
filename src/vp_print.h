/*
** vp_print.h
** Data structure printing
*/

#ifndef _VP_PRINT_H
#define _VP_PRINT_H

#include "vp_ast.h"

void vp_print_strintern(void);
void vp_print_type(Type* t);
void vp_print_typecache(void);
void vp_print_ast(Decl* d);

#endif