/*
** vp_ir.h
** Intermediate representation
*/

#ifndef _VP_IR_H
#define _VP_IR_H

#include "vp_def.h"

typedef enum IrKind
{
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
    IR_BNOT,
} IrKind;

typedef struct IR
{
    IrKind kind;
} IR;

#endif