/*
** vp_err.h
** Error and warning handling
*/

#ifndef _VP_ERR_H
#define _VP_ERR_H

#include "vp_lex.h"

void vp_err_warn(SrcLoc loc, const char* msg, ...);
void vp_err_error(SrcLoc loc, const char* msg, ...);
void vp_err_lex(LexLine line, LexOffset ofs, const char* name, const char* msg, va_list argp);

#endif