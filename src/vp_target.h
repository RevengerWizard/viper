/*
** vp_target.h
** Target machine configurations
*/

#ifndef _VP_TARGET_H
#define _VP_TARGET_H

#include "vp_regalloc.h"

/* x64 integer registers */
#define X64IREGDEF(_) \
    _(rax) _(rcx) _(rdx) _(rbx) \
    _(rsp) _(rbp) _(rsi) _(rdi) \
    _(r8) _(r9) _(r10) _(r11) \
    _(r12) _(r13) _(r14) _(r15) \

/* x64 float registers */
#define X64FREGDEF(_) \
    _(xmm0) _(xmm1) _(xmm2) _(xmm3) \
    _(xmm4) _(xmm5) _(xmm6) _(xmm7) \
    _(xmm8) _(xmm9) _(xmm10) _(xmm11) \
    _(xmm12) _(xmm13) _(xmm14) _(xmm15)

enum
{
#define IREGENUM(name) REG_##name,
#define FREGENUM(name) REG_##name,
    X64IREGDEF(IREGENUM)
    __ = -1,
    X64FREGDEF(FREGENUM)
#undef IREGENUM
#undef FREGENUM
};

extern const uint32_t winx64_icaller[];
extern const uint32_t winx64_fcaller[];

#define ICALLER_SIZE (7)
#define FCALLER_SIZE (4)

extern const RASettings sysvx64_ra;
extern const RASettings winx64_ra;

#endif