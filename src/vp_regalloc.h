/*
** vp_regalloc.h
** Register allocation
*/

#ifndef _VP_REGALLOC_H
#define _VP_REGALLOC_H

#include "vp_ir.h"
#include "vp_type.h"

/* Liveness intervals */
typedef struct LiveInterval
{
    uint32_t start; /* First use position */
    uint32_t end;   /* Last use position */
} LiveInterval;

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

static inline uint8_t vp_vflag(Type* ty)
{
    if(type_isflo(ty))
        return VRF_NUM;
    return 0;
}

/* Convert type size to virtual register size */
static inline VRegSize vp_vsize(Type* ty)
{
    vp_assertX(type_isscalar(ty), "not a primitive type");
    uint32_t size = vp_type_sizeof(ty);
    return vp_msb(size);
}

static inline VReg* vp_vreg_new(Type* ty)
{
    return vp_regalloc_spawn(vp_vsize(ty), vp_vflag(ty));
}

#endif