/*
** vp_str.h
** String handling
*/

#ifndef _VP_STR_H
#define _VP_STR_H

#include "vp_def.h"

/* String header. String payload follows */
typedef struct Str
{
    uint8_t reserved;   /* Used by lexer for fast lookup of reserved words */
    uint32_t hash;  /* Hash of string */
    uint32_t len;   /* Size of string */
} Str;

#define str_data(s) ((const char*)((s) + 1))
#define str_datawr(s) ((char*)((s) + 1))

Str* vp_str_new(const char* chars, uint32_t len);

#define vp_str_newlit(s) (vp_str_new("" s, (sizeof(s) - 1)))
#define vp_str_newlen(s) (vp_str_new(s, strlen(s)))
#define vp_str_size(len) (sizeof(Str) + (len) + 1)

#endif