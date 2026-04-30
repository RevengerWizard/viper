/*
** vp_abi.h
** ABI interface
*/

#ifndef _VP_ABI_H
#define _VP_ABI_H

#include "vp_def.h"
#include "vp_type.h"
#include "vp_target.h"
#include "vp_var.h"

/* ABI parameter classification */
typedef enum
{
    PC_IREG,    /* Integer register */
    PC_FREG,    /* Float register */
    PC_STACK,   /* Stack slot */
    PC_MEM,     /* Memory  */
    PC_SMALL,   /* Small struct passed in GPR */
} ParamClass;

/* Parameter location mapping */
typedef struct ParamLoc
{
    ParamClass cls;    /* Classifier */
    uint32_t idx;   /* Register index or stack offset */
    VarInfo* vi;
} ParamLoc;

/* Determine if type parameter needs to be allocated on the stack */
static VP_AINLINE bool abi_isstack(Type* ty)
{
    if(ty_isaggr(ty))
    {
        uint32_t size = vp_type_sizeof(ty);
        return size > 8;
    }
    return false;
}

/* Determine if type parameter aggregate is a small struct (passed in GPR) */
static VP_AINLINE bool abi_issmall(Type* ty)
{
    return ty_isaggr(ty) && vp_type_sizeof(ty) <= 8;
}

/* Classify parameter for ABI */
static VP_AINLINE ParamClass abi_classify(Type* ty, const ABIInfo* abi, uint32_t* iidx, uint32_t* fidx)
{
    if(abi_isstack(ty))
        return PC_MEM;

    if(abi_issmall(ty))
    {
        /* Small aggregate (size <= 8): passed in GPR on Windows x64 */
        uint32_t slot = (abi->flags & ABI_POS) ? MAX(*iidx, *fidx) : *iidx;
        if(slot >= abi->imax)
            return PC_STACK;
        if(abi->flags & ABI_POS)
            *iidx = *fidx = slot + 1;
        else
            (*iidx)++;
        return PC_SMALL;
    }

    uint32_t slot = (abi->flags & ABI_POS) ? MAX(*iidx, *fidx) : (ty_isflo(ty) ? *fidx : *iidx);
    uint32_t limit = ty_isflo(ty) ? abi->fmax : abi->imax;

    if(slot >= limit)
        return PC_STACK;

    if(abi->flags & ABI_POS)
    {
        *iidx = *fidx = slot + 1;
    }
    else
    {
        if(ty_isflo(ty))
            (*fidx)++;
        else
            (*iidx)++;
    }

    return ty_isflo(ty) ? PC_FREG : PC_IREG;
}

#endif