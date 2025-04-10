/*
** vp_char.h
** Character types
*/

#ifndef _VP_CHAR_H
#define _VP_CHAR_H

#include "vp_def.h"

#define VP_CHAR_CNTRL  0x01
#define VP_CHAR_SPACE  0x02
#define VP_CHAR_PUNCT  0x04
#define VP_CHAR_DIGIT  0x08
#define VP_CHAR_XDIGIT 0x10
#define VP_CHAR_UPPER  0x20
#define VP_CHAR_LOWER  0x40
#define VP_CHAR_IDENT  0x80
#define VP_CHAR_ALPHA  (VP_CHAR_LOWER | VP_CHAR_UPPER)
#define VP_CHAR_ALNUM  (VP_CHAR_ALPHA | VP_CHAR_DIGIT)
#define VP_CHAR_GRAPH  (VP_CHAR_ALNUM | VP_CHAR_PUNCT)

/* Only pass -1 or 0..255 to these macros. Never pass a signed char! */
#define vp_char_isa(c, t)  ((vp_char_bits + 1)[(c)] & t)
#define vp_char_iscntrl(c) vp_char_isa((c), VP_CHAR_CNTRL)
#define vp_char_isspace(c) vp_char_isa((c), VP_CHAR_SPACE)
#define vp_char_ispunct(c) vp_char_isa((c), VP_CHAR_PUNCT)
#define vp_char_isdigit(c) vp_char_isa((c), VP_CHAR_DIGIT)
#define vp_char_isxdigit(c) vp_char_isa((c), VP_CHAR_XDIGIT)
#define vp_char_isupper(c) vp_char_isa((c), VP_CHAR_UPPER)
#define vp_char_islower(c) vp_char_isa((c), VP_CHAR_LOWER)
#define vp_char_isident(c) vp_char_isa((c), VP_CHAR_IDENT)
#define vp_char_isalpha(c) vp_char_isa((c), VP_CHAR_ALPHA)
#define vp_char_isalnum(c) vp_char_isa((c), VP_CHAR_ALNUM)
#define vp_char_isgraph(c) vp_char_isa((c), VP_CHAR_GRAPH)

#define vp_char_toupper(c) ((c) - (vp_char_islower(c) >> 1))
#define vp_char_tolower(c) ((c) + vp_char_isupper(c))

extern const uint8_t vp_char_bits[257];

#endif