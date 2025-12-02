/*
** vp_target.h
** Target machine configurations
*/

#ifndef _VP_TARGET_H
#define _VP_TARGET_H

#include "vp_regalloc.h"

/* Object target format */
typedef enum
{
    TARGET_PE,
    TARGET_ELF,
    TARGET_COFF
} TargetFormat;

/* Operating systems */
typedef enum
{
    OS_WINDOWS
} OSType;

/* Architectures */
typedef enum
{
    ARCH_X64
} ArchType;

/* Architecture features */
typedef struct
{
    uint8_t ptrsize;    /* Size of pointer */
    uint32_t iregnum;   /* Number of int registers */
    uint32_t fregnum;   /* Number of float registers */
    const char* name;
} ArchInfo;

/* ABIs */
typedef enum
{
    ABI_WIN_X64,    /* Windows x64 ABI */
    ABI_SYSV_X64,   /* System V x64 */
} ABIType;

/* ABI configurations */
typedef struct
{
    /* Parameter registers */
    const uint32_t* imap;
    const uint32_t* fmap;

    /* Caller-save registers */
    const uint32_t* icaller;
    const uint32_t* fcaller;
    uint32_t icallersize, fcallersize;

    /* Callee-save registers */
    RegSet icallee;
    RegSet fcallee;
} ABIInfo;

/* Predefined targets */
typedef enum
{
    TARGET_X64_WINDOWS  /* x64-windows */
} TargetID;

/* Target configuration */
typedef struct TargetInfo
{
    TargetID id;
    OSType os;
    ArchType arch;
    ABIType abi;
    const char* name;
    const ABIInfo* abiinfo;
    const ArchInfo* archinfo;
    const RASettings* raset;
} TargetInfo;

const TargetInfo* vp_target_init(TargetID id);

#endif