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
    RN_CX, RN_DX, RN_8, RN_9
};

/* Float parameters */
static const uint32_t winx64_fmap[] = {
    XN_0, XN_1, XN_2, XN_3
};

/* Caller-save registers */
const uint32_t winx64_icaller[] = {
    RN_AX, RN_CX, RN_DX, RN_8, RN_9, RN_10, RN_11
};

/* Caller-save registers */
const uint32_t winx64_fcaller[] = {
    XN_0, XN_1, XN_2, XN_3
};

/* Callee-save int registers */
#define WINX64_ICALLEE \
    ((1ULL << RN_BX) | (1ULL << RN_SI) | (1ULL << RN_DI) | \
    (1ULL << RN_12) | (1ULL << RN_13) | (1ULL << RN_14) | (1ULL << RN_15))

/* Callee-save float registers */
#define WINX64_FCALLEE \
    ((1ULL << XN_0) | (1ULL << XN_1) | (1ULL << XN_2) | \
    (1ULL << XN_3) | (1ULL << XN_4) | (1ULL << XN_5))

/* -- System V x64 ABI ---------------------------------------------- */

/* Int parameters */
static const uint32_t sysvx64_imap[] = {
    RN_DI, RN_SI, RN_DX, RN_CX, RN_8, RN_9
};

/* Float parameters */
static const uint32_t sysvx64_fmap[] = {
    XN_0, XN_1, XN_2, XN_3, XN_4, XN_5, XN_6, XN_7
};

/* Caller-save registers */
static const uint32_t sysvx64_icaller[] = {
    RN_AX, RN_CX, RN_DX, RN_SI,
    RN_DI, RN_8, RN_9, RN_10, RN_11
};

/* Caller-save registers */
static const uint32_t sysvx64_fcaller[] = {
    XN_0, XN_1, XN_2, XN_3,
    XN_4, XN_5, XN_6, XN_7,
    XN_8, XN_9, XN_10, XN_11,
    XN_12, XN_13, XN_14, XN_15
};

/* Callee-save int registers */
#define SYSV_X64_ICALLEE \
    ((1ULL << RN_BX) | (1ULL << RN_BP) | (1ULL << RN_12) | \
    (1ULL << RN_13) | (1ULL << RN_14) | (1ULL << RN_15))

/* Callee-save float registers */
#define SYSV_X64_FCALLEE \
    ((1ULL << XN_8) | (1ULL << XN_9) | (1ULL << XN_10) | \
    (1ULL << XN_11) | (1ULL << XN_12) | (1ULL << XN_13) | \
    (1ULL << XN_14) | (1ULL << XN_15))

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