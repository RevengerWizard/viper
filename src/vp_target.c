/*
** vp_target.c
** Target machine configurations
*/

#include "vp_target.h"

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
#define X64IREGENUM(name) REG_##name,
#define X64FREGENUM(name) REG_##name,
    X64IREGDEF(X64IREGENUM)
    __ = -1,
    X64FREGDEF(X64FREGENUM)
#undef X64IREGENUM
#undef X64FREGENUM
};

/* Windows x64 int register parameters mapping */
static const uint32_t winx64_imap[] = {
    [0] = REG_rcx,
    [1] = REG_rdx,
    [2] = REG_r8,
    [3] = REG_r9
};

/* Windows x64 float register parameters mapping */
static const uint32_t winx64_fmap[] = {
    [0] = REG_xmm0,
    [1] = REG_xmm1,
    [2] = REG_xmm2,
    [3] = REG_xmm3
};

/* Windows x64 volatile int registers */
#define WINX64_ITEMP \
    ((1ULL << REG_rax) | (1ULL << REG_rcx) | (1ULL << REG_rdx) | \
    (1ULL << REG_r8) | (1ULL << REG_r9) | (1ULL << REG_r10) | (1ULL << REG_r11))

/* Windows x64 volatile float registers */
#define WINX64_FTEMP \
    ((1ULL << REG_xmm0) | (1ULL << REG_xmm1) | (1ULL << REG_xmm2) | \
    (1ULL << REG_xmm3) | (1ULL << REG_xmm4) | (1ULL << REG_xmm5))

/* Windows x64 regalloc settings */
const RASettings winx64_ra = {
    .imap = winx64_imap,
    .fmap = winx64_fmap,
    .iphysmax = 16,
    .fphysmax = 16,
    .itemp = WINX64_ITEMP,
    .ftemp = WINX64_FTEMP
};