/*
** vp_regalloc.h
** Register allocation
*/

#ifndef _VP_REGALLOC_H
#define _VP_REGALLOC_H

#include "vp_ir.h"
#include "vp_type.h"

enum
{
    LI_NORMAL,
    LI_SPILL
};

/* Liveness intervals */
typedef struct LiveInterval
{
    uint8_t state;
    uint32_t start; /* First use position */
    uint32_t end;   /* Last use position */
    uint32_t virt;  /* Virtual register number */
    uint32_t phys;  /* Mapped physical register number */
    uint64_t regbits;   /* Occupied registers */
} LiveInterval;

typedef struct RegAlloc RegAlloc;

/* Register allocator settings */
typedef struct RASettings
{
    uint32_t (*extra)(RegAlloc* ra, IR* ir);    /* Detect extra register constraints */
    const uint32_t* imap;    /* Mapping of integer params -> registers */
    const uint32_t* fmap;    /* Mapping of float params -> registers */
    uint32_t iphysmax;  /* Max physical integer registers */
    uint32_t fphysmax;  /* Max physical float registers */
    uint32_t itemp; /* Temporary integer registers (bitmask) */
    uint32_t ftemp; /* Temporary float registers (bitmask) */
} RASettings;

#define RAF_STACK_FRAME (1 << 0)    /* Require stack frame */

/* Register allocator state */
typedef struct RegAlloc
{
    const RASettings* set;  /* Regalloc settings */
    VReg** vregs;   /* Virtual registers */
    VReg** vconsts; /* Constant registers */
    LiveInterval* intervals;    /* Live intervals */
    LiveInterval** sorted;      /* Sorted live intervals */
    uint64_t iregbits;  /* Integer register bits in use */
    uint64_t fregbits;  /* Floating regitser bits in use */
    uint8_t flag;   /* RegAlloc flags */
} RegAlloc;

/* Virtual registers */
VReg* vp_vreg_ku(uint64_t u64, VRegSize vsize);
VReg* vp_vreg_ki(int64_t i64, VRegSize vsize);
VReg* vp_vreg_kf(double n, VRegSize vsize);

/* Register allocation */
RegAlloc* vp_ra_new(const RASettings* set);
VReg* vp_ra_spawn(VRegSize vsize, uint8_t vflag);
void vp_ra_alloc(RegAlloc *ra, BB** bbs);

/* Spill a virtual register */
static inline void vreg_spill(VReg* vr)
{
    vr->phys = 0;
    vr->flag |= VRF_SPILL;
}

/* Convert type to virtual register flag */
static inline uint8_t vp_vflag(Type* ty)
{
    uint8_t flag = 0;
    if(type_isflo(ty))
    {
        flag |= VRF_FLO;
    }
    if(type_isunsigned(ty))
    {
        flag |= VRF_UNSIGNED;
    }
    return flag;
}

/* Convert type size to virtual register size */
static inline VRegSize vp_vsize(Type* ty)
{
    vp_assertX(type_isscalar(ty), "not a primitive type");
    uint32_t size = vp_type_sizeof(ty);
    return vp_msb(size);
}

/* Spawn a new vreg using type information */
static inline VReg* vp_vreg_new(Type* ty)
{
    return vp_ra_spawn(vp_vsize(ty), vp_vflag(ty));
}

#endif