/*
** vp_target.c
** Target machine configurations
*/

#include "vp_target.h"

#include "vp_def.h"
#include "vp_regalloc.h"
#include "vp_target_x64.h"
#include "vp_low.h"

/* -- Windows x64 ABI ----------------------------------------------- */

/* Int parameters */
static const uint32_t winx64_imap[] = {
    RCX, RDX, R8, R9
};

/* Float parameters */
static const uint32_t winx64_fmap[] = {
    XMM0, XMM1, XMM2, XMM3
};

/* Caller-save registers */
const uint32_t winx64_icaller[] = {
    RAX, RCX, RDX, R8, R9, R10, R11
};

/* Caller-save registers */
const uint32_t winx64_fcaller[] = {
    XMM0, XMM1, XMM2, XMM3
};

/* Callee-save int registers */
#define WINX64_ICALLEE \
    ((1ULL << RBX) | (1ULL << RSI) | (1ULL << RDI) | \
    (1ULL << R12) | (1ULL << R13) | (1ULL << R14) | (1ULL << R15))

/* Callee-save float registers */
#define WINX64_FCALLEE \
    ((1ULL << XMM0) | (1ULL << XMM1) | (1ULL << XMM2) | \
    (1ULL << XMM3) | (1ULL << XMM4) | (1ULL << XMM5))

/* -- System V x64 ABI ---------------------------------------------- */

/* Int parameters */
static const uint32_t sysvx64_imap[] = {
    RDI, RSI, RDX, RCX, R8, R9
};

/* Float parameters */
static const uint32_t sysvx64_fmap[] = {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7
};

/* Caller-save registers */
static const uint32_t sysvx64_icaller[] = {
    RAX, RCX, RDX, RSI,
    RDI, R8, R9, R10, R11
};

/* Caller-save registers */
static const uint32_t sysvx64_fcaller[] = {
    XMM0, XMM1, XMM2, XMM3,
    XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11,
    XMM12, XMM13, XMM14, XMM15
};

/* Callee-save int registers */
#define SYSV_X64_ICALLEE \
    ((1ULL << RBX) | (1ULL << RBP) | (1ULL << R12) | \
    (1ULL << R13) | (1ULL << R14) | (1ULL << R15))

/* Callee-save float registers */
#define SYSV_X64_FCALLEE \
    ((1ULL << XMM8) | (1ULL << XMM9) | (1ULL << XMM10) | \
    (1ULL << XMM11) | (1ULL << XMM12) | (1ULL << XMM13) | \
    (1ULL << XMM14) | (1ULL << XMM15))

/* Archs */
static const ArchInfo archs[] = {
    [ARCH_X64] = {
        .ptrsize = 8,
        .iregnum = X64_IREG,
        .fregnum = X64_FREG,
        .name = "x64"
    }
};

/* ABIs */
static const ABIInfo abis[] = {
    [ABI_WIN_X64] = {
        .imap = winx64_imap,
        .fmap = winx64_fmap,
        .icaller = winx64_icaller,
        .fcaller = winx64_fcaller,
        .icallersize = ARRSIZE(winx64_icaller),
        .fcallersize = ARRSIZE(winx64_fcaller),
        .icallee = WINX64_ICALLEE,
        .fcallee = WINX64_FCALLEE
    },
    [ABI_SYSV_X64] = {
        .imap = sysvx64_imap,
        .fmap = sysvx64_fmap,
        .icaller = sysvx64_icaller,
        .fcaller = sysvx64_fcaller,
        .icallersize = ARRSIZE(sysvx64_icaller),
        .fcallersize = ARRSIZE(sysvx64_fcaller),
        .icallee = SYSV_X64_ICALLEE,
        .fcallee = SYSV_X64_FCALLEE
    }
};

/* RegAlloc settings */
static const RASettings ras[] = {
    [ABI_WIN_X64] = {
        .extra = vp_ir_x64_extra,
        .imap = abis[ABI_WIN_X64].imap,
        .fmap = abis[ABI_WIN_X64].fmap,
        .iphysmax = archs[ARCH_X64].iregnum,
        .fphysmax = archs[ARCH_X64].fregnum,
        .itemp = abis[ABI_WIN_X64].icallee,
        .ftemp = abis[ABI_WIN_X64].fcallee,
    },
    [ABI_SYSV_X64] = {
        .extra = vp_ir_x64_extra,
        .imap = abis[ABI_SYSV_X64].imap,
        .fmap = abis[ABI_SYSV_X64].fmap,
        .iphysmax = archs[ARCH_X64].iregnum,
        .fphysmax = archs[ARCH_X64].fregnum,
        .itemp = abis[ABI_SYSV_X64].icallee,
        .ftemp = abis[ABI_SYSV_X64].fcallee,
    }
};

/* Target configuration table */
static const TargetInfo targets[] = {
    {
        TARGET_X64_WINDOWS,
        OS_WINDOWS,
        ARCH_X64,
        ABI_WIN_X64,
        "x64-windows",
        &abis[ABI_WIN_X64],
        &archs[ARCH_X64],
        &ras[ABI_WIN_X64]
    }
};

#define TARGET_NUM (ARRSIZE(targets))

/* Initialize target configuration */
const TargetInfo* vp_target_init(TargetID id)
{
    if(id >= TARGET_NUM)
        return NULL;
    return &targets[id];
}