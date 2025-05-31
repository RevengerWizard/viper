/*
** vp_regalloc.h
** Register allocation
*/

#ifndef _VP_REGALLOC_H
#define _VP_REGALLOC_H

#include "vp_ir.h"
#include "vp_type.h"

/* Register allocator state */
typedef struct RegAlloc
{
    VReg** vregs;
    VReg** vconsts;
} RegAlloc;

/* Register allocation */
RegAlloc* vp_regalloc_new();
VReg* vp_regalloc_spawn(VRegSize vsize, uint8_t vflag);

/* Virtual registers */
VReg* vp_vreg_ku(uint64_t u64, VRegSize vsize);
VReg* vp_vreg_ki(int64_t i64, VRegSize vsize);
VReg* vp_vreg_kf(double n, VRegSize vsize);

/* Convert type size to virtual register size */
static inline VRegSize vp_vsize(Type* ty)
{
    uint32_t size = vp_type_sizeof(ty);
    return vp_msb(size);
}

#endif