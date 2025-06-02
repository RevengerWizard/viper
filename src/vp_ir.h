/*
** vp_ir.h
** Intermediate representation
*/

#ifndef _VP_IR_H
#define _VP_IR_H

#include "vp_str.h"

#define VRF_INT (1 << 0)    /* Signed integer */
#define VRF_UINT (1 << 1)   /* Unsigned integer */
#define VRF_NUM (1 << 2)    /* Floating point */
#define VRF_REF (1 << 3)    /* Reference & */
#define VRF_PARAM (1 << 4)  /* Function parameter */

#define VRF_CONST (VRF_INT | VRF_UINT | VRF_NUM)

/* Size of a virtual register */
typedef enum VRegSize
{
    VRegSize1,
    VRegSize2,
    VRegSize4,
    VRegSize8,
} VRegSize;

/* Virtual register */
typedef struct VReg
{
    VRegSize vsize;
    uint8_t flag;
    union
    {
        /* Non-const */
        struct
        {
            struct VReg* vreg;
            uint32_t virt;  /* Virtual reg number */
            uint32_t phys;  /* Physical reg number */
        };
        /* Constant */
        int64_t i64;
        uint64_t u64;
        double n;
    };
} VReg;

typedef enum IrKind
{
    IR_BOFS,    /* dst = [rbp + offset] */
    IR_IOFS,    /* dst = [rip + label] */
    IR_MOV,     /* dst = src1 */
    IR_STORE,   /* [src2] = src1 */
    IR_LOAD,    /* dst = [src1] */
    IR_RET,
    IR_COND,
    IR_JMP,
    /* Binary operators */
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_MOD,
    IR_BAND,
    IR_BOR,
    IR_BXOR,
    IR_LSHIFT,
    IR_RSHIFT,
    /* Unary operators */
    IR_NEG,
    IR_NOT,
    IR_BNOT,
} IrKind;

/* Condition flags (lower bits) */
typedef enum CondKind
{
    COND_NONE,
    COND_ANY,
    COND_EQ,
    COND_NEQ,
    COND_LT,
    COND_LE,
    COND_GT,
    COND_GE
} CondKind;

/* Condition masks */
enum
{
    COND_MASK = 0x07,
    COND_UNSIGNED = 1 << 3,
    COND_NUM = 1 << 4
};

typedef struct IR
{
    IrKind kind;
    VReg* dst;
    VReg* src1;
    VReg* src2;
    union
    {
        CondKind cond;
        Str* label;
        struct
        {
            struct BB* bb;
            CondKind cond; 
        } jmp;
    };
} IR;

/* Basic blocks */
typedef struct BB
{
    struct BB* next;
    Str* label;
    IR** irs;
} BB;

/* IR instructions */
IR* vp_ir_bofs();
IR* vp_ir_iofs(Str* label);
IR* vp_ir_mov(VReg* dst, VReg* src);
IR* vp_ir_store(VReg* dst, VReg* src);
IR* vp_ir_load(VReg* src, VRegSize vsize);
IR* vp_ir_ret(VReg* src);
IR* vp_ir_cond(VReg* src1, VReg* src2, CondKind cond);
IR* vp_ir_jmp(BB* bb);
void vp_ir_cjmp(VReg* src1, VReg* src2, CondKind cond, BB* bb);
VReg* vp_ir_binop(IrKind kind, VReg* src1, VReg* src2, VRegSize vsize);
VReg* vp_ir_unary(IrKind kind, VReg* src, VRegSize vsize);

/* Conditions */
CondKind vp_cond_swap(CondKind cond);
CondKind vp_cond_invert(CondKind cond);

/* Basic blocks */
BB* vp_bb_new();
void vp_bb_setcurr(BB* bb);

Str* vp_label_new();

#endif