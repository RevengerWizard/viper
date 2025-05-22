/*
** vp_strscan.h
** String scanning
*/

#ifndef _VP_STRSCAN_H
#define _VP_STRSCAN_H

#include "vp_def.h"
#include "vp_lex.h"

/* Returned format */
typedef enum
{
    STRSCAN_ERROR,
    STRSCAN_NUM,
    STRSCAN_INT,
    STRSCAN_U32, STRSCAN_I64, STRSCAN_U64
} StrScanFmt;

StrScanFmt vp_strscan_scan(const uint8_t* p, size_t len, LexValue* o);

#endif