/*
** vp_target.c
** Target machine configurations
*/

#include "vp_target.h"
#include "vp_sel.h"

/* Windows x64 int parameters */
static const uint32_t winx64_imap[] = {
    [0] = REG_rcx,
    [1] = REG_rdx,
    [2] = REG_r8,
    [3] = REG_r9
};

/* Windows x64 float parameters */
static const uint32_t winx64_fmap[] = {
    [0] = REG_xmm0,
    [1] = REG_xmm1,
    [2] = REG_xmm2,
    [3] = REG_xmm3
};

/* Windows x64 callee-saved int registers */
#define WINX64_ITEMP \
    ((1ULL << REG_rbx) | (1ULL << REG_rsi) | (1ULL << REG_rdi) | \
    (1ULL << REG_r12) | (1ULL << REG_r13) | (1ULL << REG_r14) | (1ULL << REG_r15))

/* Windows x64 callee-saved float registers */
#define WINX64_FTEMP \
    ((1ULL << REG_xmm0) | (1ULL << REG_xmm1) | (1ULL << REG_xmm2) | \
    (1ULL << REG_xmm3) | (1ULL << REG_xmm4) | (1ULL << REG_xmm5))

/* Windows x64 regalloc settings */
const RASettings winx64_ra = {
    .extra = sel_extra,
    .imap = winx64_imap,
    .fmap = winx64_fmap,
    .iphysmax = 16,
    .fphysmax = 16,
    .itemp = WINX64_ITEMP,
    .ftemp = WINX64_FTEMP
};

/* System-V x64 int parameters */
static const uint32_t sysv_x64_imap[] = {
    [0] = REG_rdi,
    [1] = REG_rsi,
    [2] = REG_rdx,
    [3] = REG_rcx,
    [4] = REG_r8,
    [5] = REG_r9
};

/* System-V x64 float parameters */
static const uint32_t sysv_x64_fmap[] = {
    [0] = REG_xmm0,
    [1] = REG_xmm1,
    [2] = REG_xmm2,
    [3] = REG_xmm3,
    [4] = REG_xmm4,
    [5] = REG_xmm5,
    [6] = REG_xmm6,
    [7] = REG_xmm7
};

/* System V x64 callee-saved int registers */
#define SYSV_X64_ITEMP \
    ((1ULL << REG_rbx) | (1ULL << REG_rbp) | (1ULL << REG_r12) | \
    (1ULL << REG_r13) | (1ULL << REG_r14) | (1ULL << REG_r15))

/* System V x64 callee-saved float registers */
#define SYSV_X64_FTEMP \
    ((1ULL << REG_xmm8) | (1ULL << REG_xmm9) | (1ULL << REG_xmm10) | \
    (1ULL << REG_xmm11) | (1ULL << REG_xmm12) | (1ULL << REG_xmm13) | \
    (1ULL << REG_xmm14) | (1ULL << REG_xmm15))

/* System V x64 regalloc settings */
const RASettings sysv_x64_ra = {
    .extra = sel_extra,
    .imap = sysv_x64_imap,
    .fmap = sysv_x64_fmap,
    .iphysmax = 16,
    .fphysmax = 16,
    .itemp = SYSV_X64_ITEMP,
    .ftemp = SYSV_X64_FTEMP
};