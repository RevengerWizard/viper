/*
** vp_abi.h
** ABI interface
*/

#ifndef _VP_ABI_H
#define _VP_ABI_H

#include "vp_def.h"
#include "vp_type.h"
#include "vp_target.h"

/* ABI parameter classification */
typedef enum
{
    PC_IREG,    /* Integer register */
    PC_FREG,    /* Float register */
    PC_STACK,   /* Stack slot */
    PC_MEM      /* Memory  */
} ParamClass;

/* Parameter location mapping */
typedef struct ParamLoc
{
    ParamClass cls;    /* Classifier */
    uint32_t idx;   /* Register index or stack offset */
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

/* Classify parameter for ABI */
static VP_AINLINE ParamClass abi_classify(Type* ty, const ABIInfo* abi, uint32_t* iidx, uint32_t* fidx)
{
    if(abi_isstack(ty))
        return PC_MEM;

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