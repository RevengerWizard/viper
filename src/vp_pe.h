/*
** vp_pe.h
** Windows PE emitter
*/

#ifndef _VP_PE_H
#define _VP_PE_H

#include "vp_state.h"

/* Subsystems */
#define IMAGE_SUBSYSTEM_WINDOWS_GUI 2
#define IMAGE_SUBSYSTEM_CONSOLE 3

/* Machine types */
#define IMAGE_FILE_MACHINE_AMD64    0x8664

/* Characteristics */
#define IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define IMAGE_FILE_LARGE_ADDRESS_AWARE 0x0020
#define IMAGE_FILE_DEBUG_STRIPPED   0x0200

/*  Section flags */
#define IMAGE_SCN_CNT_INITIALIZED_DATA  0x00000040
#define IMAGE_SCN_CNT_CODE              0x00000020
#define IMAGE_SCN_MEM_EXECUTE           0x20000000
#define IMAGE_SCN_MEM_READ              0x40000000
#define IMAGE_SCN_MEM_WRITE             0x80000000

/* Symbol storage class */
#define IMAGE_SYM_CLASS_EXTERNAL 2
#define IMAGE_SYM_CLASS_STATIC 3

/* Symbol types */
#define IMAGE_SYM_DTYPE_FUNCTION 0x20

/* x64 Relocation types */
#define IMAGE_REL_AMD64_ABSOLUTE 0x0000
#define IMAGE_REL_AMD64_ADDR64   0x0001
#define IMAGE_REL_AMD64_ADDR32   0x0002
#define IMAGE_REL_AMD64_ADDR32NB 0x0003
#define IMAGE_REL_AMD64_REL32    0x0004

void vp_emit_coff(VpState* V, SBuf* sb);
void vp_emit_exe(VpState* V, SBuf* sb);

#endif