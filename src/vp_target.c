/*
** vp_target.c
** Target machine configurations
*/

#include "vp_target.h"

#include "vp_def.h"
#include "vp_target_x64.h"

const X64Reg x64R8[X64_IREG] =  {AL,  CL,  DL,  BL,  SPL, BPL, SIL, DIL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B};
const X64Reg x64R16[X64_IREG] = {AX,  CX,  DX,  BX,  SP,  BP,  SI,  DI,  R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W};
const X64Reg x64R32[X64_IREG] = {EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D};
const X64Reg x64R64[X64_IREG] = {RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8,  R9,  R10,  R11,  R12,  R13,  R14,  R15};
const X64Reg* x64R[] = {x64R8, x64R16, x64R32, x64R64};

const X64Reg x64X[X64_FREG] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15};

/* -- Windows x64 ABI ----------------------------------------------- */

/* Int parameters */
/* rcx, rdx, r8, r9 */
static const uint32_t winx64_imap[] = {
    RN_CX, RN_DX, RN_8, RN_9
};

/* Float parameters */
/* xmm0-xmm3 */
static const uint32_t winx64_fmap[] = {
    XN_0, XN_1, XN_2, XN_3
};

/* Caller-save int registers */
/* rax, rcx, rdx, r8-r11 volatile */
#define WINX64_ICALLER \
    ((1ULL << RN_AX) | (1ULL << RN_CX) | (1ULL << RN_DX) | \
    (1ULL << RN_8) | (1ULL << RN_9) | (1ULL << RN_10) | (1ULL << RN_11))

/* Caller-save float registers */
/* xmm0-xmm5 volatile */
#define WINX64_FCALLER \
    ((1ULL << XN_0) | (1ULL << XN_1) | (1ULL << XN_2) | \
    (1ULL << XN_3) | (1ULL << XN_4) | (1ULL << XN_5))

/* Callee-save int registers */
/* rbx, rsi, rdi, rbp, r12-r15 nonvolatile */
#define WINX64_ICALLEE \
    ((1ULL << RN_BX) | (1ULL << RN_SI) | (1ULL << RN_DI) | (1ULL << RN_BP) | \
    (1ULL << RN_12) | (1ULL << RN_13) | (1ULL << RN_14) | (1ULL << RN_15))

/* Callee-save float registers */
/* xmm6-xmm15 nonvolatile */
#define WINX64_FCALLEE \
    ((1ULL << XN_6) | (1ULL << XN_7) | (1ULL << XN_8) | \
    (1ULL << XN_9) | (1ULL << XN_10) | (1ULL << XN_11) | \
    (1ULL << XN_12) | (1ULL << XN_13) | (1ULL << XN_14) | \
    (1ULL << XN_15))

/* -- System V x64 ABI ---------------------------------------------- */

/* Int parameters */
/* rdi, rsi, rdx, rcx, r8, r9 */
static const uint32_t sysvx64_imap[] = {
    RN_DI, RN_SI, RN_DX, RN_CX, RN_8, RN_9
};

/* Float parameters */
/* xmm0-xmm7 */
static const uint32_t sysvx64_fmap[] = {
    XN_0, XN_1, XN_2, XN_3, XN_4, XN_5, XN_6, XN_7
};

/* Caller-save int registers */
/* rax, rcx, rdx, rsi, rdi, r8-r11 volatile */
#define SYSV_X64_ICALLER \
    ((1ULL << RN_AX) | (1ULL << RN_CX) | (1ULL << RN_DX) | (1ULL << RN_SI) | \
    (1ULL << RN_DI) | (1ULL << RN_8) | (1ULL << RN_9) | (1ULL << RN_10) | (1ULL << RN_11))

/* Caller-save float registers */
/* xmm0-xmm15 volatile */
#define SYSV_X64_FCALLER \
    ((1ULL << XN_0) | (1ULL << XN_1) | (1ULL << XN_2) | (1ULL << XN_3) | \
    (1ULL << XN_4) | (1ULL << XN_5) | (1ULL << XN_6) | (1ULL << XN_7) | \
    (1ULL << XN_8) | (1ULL << XN_9) | (1ULL << XN_10) | (1ULL << XN_11) | \
    (1ULL << XN_12) | (1ULL << XN_13) | (1ULL << XN_14) | (1ULL << XN_15))

/* Callee-save int registers */
/* rbx, rbp, r12-r15 nonvolatile */
#define SYSV_X64_ICALLEE \
    ((1ULL << RN_BX) | (1ULL << RN_BP) | (1ULL << RN_12) | \
    (1ULL << RN_13) | (1ULL << RN_14) | (1ULL << RN_15))

/* Callee-save float registers */
/* r8-r15 nonvolatile */
#define SYSV_X64_FCALLEE \
    ((1ULL << XN_8) | (1ULL << XN_9) | (1ULL << XN_10) | \
    (1ULL << XN_11) | (1ULL << XN_12) | (1ULL << XN_13) | \
    (1ULL << XN_14) | (1ULL << XN_15))

/* Archs */
static const ArchInfo archs[] = {
    [ARCH_X64] = {
        .ptrsize = 8,
        .imax = X64_IREG,
        .fmax = X64_FREG,
        .name = "x64"
    }
};

/* ABIs */
static const ABIInfo abis[] = {
    [ABI_WIN_X64] = {
        .flags = ABI_POS | ABI_SHADOW,
        .imap = winx64_imap,
        .fmap = winx64_fmap,
        .imax = ARRSIZE(winx64_imap),
        .fmax = ARRSIZE(winx64_fmap),
        .icaller = WINX64_ICALLER,
        .fcaller = WINX64_FCALLER,
        .icallee = WINX64_ICALLEE,
        .fcallee = WINX64_FCALLEE
    },
    [ABI_SYSV_X64] = {
        .flags = 0,
        .imap = sysvx64_imap,
        .fmap = sysvx64_fmap,
        .imax = ARRSIZE(sysvx64_imap),
        .fmax = ARRSIZE(sysvx64_fmap),
        .icaller = SYSV_X64_ICALLER,
        .fcaller = SYSV_X64_FCALLER,
        .icallee = SYSV_X64_ICALLEE,
        .fcallee = SYSV_X64_FCALLEE
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
    },
    {
        TARGET_X64_LINUX,
        OS_LINUX,
        ARCH_X64,
        ABI_SYSV_X64,
        "x64-linux",
        &abis[ABI_SYSV_X64],
        &archs[ARCH_X64]
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