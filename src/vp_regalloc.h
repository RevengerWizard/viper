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

typedef uint64_t RegSet;

/* Liveness intervals */
typedef struct LiveInterval
{
    uint8_t state;
    uint32_t start; /* First use position */
    uint32_t end;   /* Last use position */
    uint32_t virt;  /* Virtual register number */
    uint32_t phys;  /* Mapped physical register number */
    RegSet regbits;   /* Occupied registers */
} LiveInterval;

typedef struct RegAlloc RegAlloc;

/* Register allocator settings */
typedef struct RASettings
{
    RegSet (*extra)(RegAlloc* ra, IR* ir);    /* Detect extra register constraints */
    const uint32_t* imap;    /* Mapping of integer params -> registers */
    const uint32_t* fmap;    /* Mapping of float params -> registers */
    uint32_t iphysmax;  /* Max physical integer registers */
    uint32_t fphysmax;  /* Max physical float registers */
    RegSet itemp; /* Temporary integer registers (bitmask) */
    RegSet ftemp; /* Temporary float registers (bitmask) */
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
    RegSet iregbits;  /* Integer register bits in use */
    RegSet fregbits;  /* Floating regitser bits in use */
    uint8_t flag;   /* RegAlloc flags */
} RegAlloc;

/* Virtual registers */
VReg* vp_vreg_ki(int64_t i64, VRSize vsize);
VReg* vp_vreg_kf(double n, VRSize vsize);

/* Register allocation */
RegAlloc* vp_ra_new(const RASettings* set);
VReg* vp_ra_spawn(VRSize vsize, uint8_t vflag);
void vp_ra_alloc(RegAlloc *ra, BB** bbs);

/* Spill a virtual register */
static VP_AINLINE void vreg_spill(VReg* vr)
{
    vr->phys = 0;
    vr->flag |= VRF_SPILL;
}

/* Convert type to virtual register flag */
static VP_AINLINE uint8_t vp_vflag(Type* ty)
{
    uint8_t flag = 0;
    if(ty_isflo(ty))
    {
        flag |= VRF_FLO;
    }
    return flag;
}

/* Convert type size to virtual register size */
static VP_AINLINE VRSize vp_vsize(Type* ty)
{
    vp_assertX(ty_isscalar(ty), "not a primitive type");
    uint32_t size = vp_type_sizeof(ty);
    return vp_msb(size);
}

/* Spawn a new vreg using type information */
static VP_AINLINE VReg* vp_vreg_new(Type* ty)
{
    return vp_ra_spawn(vp_vsize(ty), vp_vflag(ty));
}

#define vp_assertVSize(v, min, max) \
    vp_assertX((v) >= (min) && (v) <= (max), "invalid VRSize")

#endif