/*
** vp_emit_x64.c
** x64 Instruction emitter
*/

#include "vp_emit.h"
#include "vp_state.h"
#include "vp_target_x64.h"

/* -- MOV instructions ---------------------------------------------- */

/* MOV reg64, reg64 */
static void emit_mov64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x89);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* MOV reg32, reg32 */
static void emit_mov32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x89);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* MOV reg16, reg16 */
static void emit_mov16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x89);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* MOV reg8, reg8 */
static void emit_mov8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(src & 8) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(rex || (dst >= RSP && dst <= RDI) || (src >= RSP && src <= RDI))
    {
        if(!(rex & (REX_W | REX_R | REX_X | REX_B)))
        {

        }
        if(rex) emit_u8(V, rex);
    }
    emit_u8(V, 0x88);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* MOV reg64, imm64 */
static void emit_mov64_ri(VpState* V, X64Reg reg, int64_t imm)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    if(vp_isimm32(imm))
    {
        /* imm32 */
        emit_u8(V, 0xC7);
        emit_u8(V, MODRM(3, 0, reg & 7));
        emit_im32(V, (int32_t)imm);
    }
    else
    {
        /* imm64 */
        emit_u8(V, 0xB8 + (reg & 7));
        emit_im64(V, imm);
    }
}

/* MOV reg32, imm32 */
static void emit_mov32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xB8 + (reg & 7));
    emit_im32(V, imm);
}

/* MOV reg16, imm16 */
static void emit_mov16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xB8 + (reg & 7));
    emit_im16(V, imm);
}

/* MOV reg8, imm8 */
static void emit_mov8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI))
    {
        if(regext(reg)) emit_u8(V, REX_B);
        emit_u8(V, 0xC6);
        emit_u8(V, MODRM(3, 0, reg & 7));
    }
    else    /* AL, CL, DL, BL (or AX/EAX/RAX if reg is 0) */
    {
        /* These can use the 0xB0 + reg opcode */
        emit_u8(V, 0xB0 + (reg & 7));
    }
    emit_u8(V, (uint8_t)imm);
}

static void mem_op(X64Mem mem, X64Reg* base, X64Reg* idx, uint8_t* scale, int32_t* off)
{
    mem &= ~((int64_t)1 << 63);

    *base = (X64Reg)((mem >> 32) & 0xfff);
    *idx = (X64Reg)((mem >> 44) & 0xfff);
    *scale = (uint8_t)((mem >> 56) & 0xf);
    *off = (int32_t)(mem & 0xffffffff);
}

/* Determine REX for memory instructions */
static VP_AINLINE uint8_t rex_mem(X64Reg dst, X64Reg base, X64Reg idx, bool is64)
{
    uint8_t rex = 0;
    if(is64) rex |= REX_W;
    if(regext(dst)) rex |= REX_R;
    if(base != RIP && regext(base)) rex |= REX_B;
    if(idx != NOREG && regext(idx)) rex |= REX_X;
    return rex;
}

static void emit_modrm_sib_disp(VpState* V, X64Reg src, X64Reg base, X64Reg idx, uint8_t scale, int32_t disp)
{
    uint8_t mod = 0;
    uint8_t rm = 0;
    bool needs_sib = false;

    if(base == RIP)
    {
        mod = 0;
        rm = 5;
        /* No SIB byte for RIP-relative addressing */
    }
    else
    {
        if(disp == 0 && (base != RBP && base != R13))
        {
            mod = 0;
        }
        else if(vp_isimm8(disp))
        {
            mod = 1;
        }
        else
        {
            mod = 2;
        }

        if(idx != NOREG || base == RSP || base == R12)
        {
            needs_sib = true;
            rm = RM_SIB;
        }
        else
        {
            rm = base & 7;
        }
    }

    emit_u8(V, MODRM(mod, src & 7, rm));

    /* Emit SIB byte if needed */
    if(needs_sib)
    {
        uint8_t sib_scale = 0;
        if(scale == 2) sib_scale = 1;
        else if(scale == 4) sib_scale = 2;
        else if(scale == 8) sib_scale = 3;

        uint8_t sib_idx = (idx == NOREG) ? RSP & 7 : idx & 7;
        uint8_t sib_base = base & 7;

        emit_u8(V, SIB(sib_scale, sib_idx, sib_base));
    }

    if(mod == 1)
    {
        emit_u8(V, (int8_t)disp);
    }
    else if (mod == 2 || (base == RIP && mod == 0))
    {
        emit_im32(V, disp);
    }
}

/* Emit common MOV instructions with memory operand (load/store) */
static void emit_mov_mem(VpState* V, X64Reg src, X64Mem mem, int size, bool is_load)
{
    X64Reg base, idx;
    uint8_t scale;
    int32_t disp;
    mem_op(mem, &base, &idx, &scale, &disp);

    if(idx != NOREG)
    {
        vp_assertX(IS_POW2(scale) && (scale >= 1 && scale <= 8), "bad scale (1/2/4/8)");
    }

    uint8_t rex = rex_mem(src, base, idx, size == 64);
    if(size == 8 && (src & 7) >= 4)
    {
        if(rex == 0) rex = 0x40;
    }
    if(rex) emit_u8(V, rex);

    if(size == 16) emit_u8(V, 0x66);

    uint8_t op = is_load ? 0x8B : 0x89; /* MOV reg, r/m vs MOV r/m, reg */
    if(size == 8) op = is_load ? 0x8A : 0x88; /* MOV reg8, r/m8 vs MOV r/m8, reg8 */

    emit_u8(V, op);
    emit_modrm_sib_disp(V, src, base, idx, scale, disp);
}

/* MOV reg, [base + index*scale + disp32] */
static void emit_mov8_rm(VpState* V, X64Reg dst, X64Mem mem) { emit_mov_mem(V, dst, mem, 8, true); }
static void emit_mov16_rm(VpState* V, X64Reg dst, X64Mem mem) { emit_mov_mem(V, dst, mem, 16, true); }
static void emit_mov32_rm(VpState* V, X64Reg dst, X64Mem mem) { emit_mov_mem(V, dst, mem, 32, true); }
static void emit_mov64_rm(VpState* V, X64Reg dst, X64Mem mem) { emit_mov_mem(V, dst, mem, 64, true); }

/* MOV [base + index*scale + disp32], reg */
static void emit_mov8_mr(VpState* V, X64Mem mem, X64Reg src) { emit_mov_mem(V, src, mem, 8, false); }
static void emit_mov16_mr(VpState* V, X64Mem mem, X64Reg src) { emit_mov_mem(V, src, mem, 16, false); }
static void emit_mov32_mr(VpState* V, X64Mem mem, X64Reg src) { emit_mov_mem(V, src, mem, 32, false); }
static void emit_mov64_mr(VpState* V, X64Mem mem, X64Reg src) { emit_mov_mem(V, src, mem, 64, false); }

/* Helper to emit MOV [reg], imm instructions */
static void emit_mov_mi_common(VpState* V, X64Reg dst, int size, int64_t imm)
{
    uint8_t rex = 0;
    if(size == 64) rex |= REX_W;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);

    if(size == 16) emit_u8(V, 0x66);

    uint8_t op;
    if(size == 8) op = 0xC6; /* MOV r/m8, imm8 */
    else op = 0xC7; /* MOV r/m16/32/64, imm16/32 */
    emit_u8(V, op);

    uint8_t modrm_rm = dst & 7;
    if(dst == RSP || ((dst & 7) == (R12 & 7) && regext(dst)))
    {
        modrm_rm = RM_SIB;
    }
    emit_u8(V, MODRM(0, 0, modrm_rm));

    if(modrm_rm == RM_SIB)
    {
        emit_u8(V, SIB(0, RSP & 7, dst & 7));
    }

    if(size == 8)
    {
        emit_u8(V, (uint8_t)imm);
    }
    else if(size == 16)
    {
        emit_im16(V, (int16_t)imm);
    }
    else if(size == 32 || size == 64)
    {
        emit_im32(V, (int32_t)imm);
    }
}

/* MOV [reg8], imm8 */
static void emit_mov8_mi(VpState* V, X64Reg reg, int8_t imm)
{
    emit_mov_mi_common(V, reg, 8, imm);
}

/* MOV [reg16], imm16 */
static void emit_mov16_mi(VpState* V, X64Reg reg, int16_t imm)
{
    emit_mov_mi_common(V, reg, 16, imm);
}

/* MOV [reg32], imm32 */
static void emit_mov32_mi(VpState* V, X64Reg reg, int32_t imm)
{
    emit_mov_mi_common(V, reg, 32, imm);
}

/* MOV [reg64], imm32 (sign-extended to 64-bit) */
static void emit_mov64_mi(VpState* V, X64Reg reg, int32_t imm)
{
    emit_mov_mi_common(V, reg, 64, imm);
}

/* -- MOVSX instructions -------------------------------------------- */

/* MOVSX reg16, reg8 */
static void emit_movsx_r16r8(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xBE);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVSX reg32, reg8 */
static void emit_movsx_r32r8(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xBE);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVSX reg32, reg16 */
static void emit_movsx_r32r16(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xBF);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVSX reg64, reg8 */
static void emit_movsx_r64r8(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xBE);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVSX reg64, reg16 */
static void emit_movsx_r64r16(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xBF);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVSX reg64, reg32 */
static void emit_movsx_r64r32(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x8B);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* -- MOVZX instructions -------------------------------------------- */

/* MOVZX reg16, reg8 */
static void emit_movzx_r16r8(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xB6);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVZX reg32, reg8 */
static void emit_movzx_r32r8(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xB6);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVZX reg32, reg16 */
static void emit_movzx_r32r16(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xB7);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVZX reg64, reg8 */
static void emit_movzx_r64r8(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xB6);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVZX reg64, reg16 */
static void emit_movzx_r64r16(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0xB7);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* -- MOVQ/MOVD instructions ---------------------------------------- */

/* MOVQ xmm, reg64 */
static void emit_movq_xr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = REX_W;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x6E); /* MOVQ opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVD xmm, reg32 */
static void emit_movd_xr(VpState* V, X64Reg dst_xmm, X64Reg src_gpr32)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(dst_xmm)) rex |= REX_R;
    if(regext(src_gpr32)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x6E);
    emit_u8(V, MODRM(3, dst_xmm & 7, src_gpr32 & 7));
}

/* -- LEA instructions ---------------------------------------------- */

/* LEA reg64, [base + idx * scale + disp32] */
static void emit_lea64_rm(VpState* V, X64Reg dst, X64Mem mem)
{
    X64Reg base, idx;
    uint8_t scale;
    int32_t disp;
    mem_op(mem, &base, &idx, &scale, &disp);

    if(idx != NOREG)
    {
        vp_assertX(IS_POW2(scale) && scale <= 8, "bad scale (1/2/4/8)");
    }

    uint8_t rex = rex_mem(dst, base, idx, true);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x8D);   /* LEA opcode */

    emit_modrm_sib_disp(V, dst, base, idx, scale, disp);
}

/* -- ADD instructions ---------------------------------------------- */

/* ADD reg64, reg64 */
static void emit_add64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x01);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* ADD reg32, reg32 */
static void emit_add32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x01);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* ADD reg16, reg16 */
static void emit_add16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x01);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* ADD reg8, reg8 */
static void emit_add8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x00);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* ADD reg64, imm32 */
static void emit_add64_ri(VpState* V, X64Reg reg, int32_t imm)
{
    uint8_t rex = REX_W; /* REX.W is required for 64-bit operand size */
    if(regext(reg)) rex |= REX_B; /* Set REX.B if reg is R8-R15 */
    emit_u8(V, rex);
    if(reg == RAX)
    {
        /* Special opcode for ADD RAX, imm32 (0x05) */
        emit_u8(V, 0x05);
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        /* Use opcode 0x83 for sign-extended 8-bit immediate */
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 0, reg & 7)); /* ModR/M byte with /0 extension for ADD */
        emit_u8(V, (uint8_t)imm); /* 8-bit immediate */
    }
    else
    {
        /* Use opcode 0x81 for 32-bit immediate */
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 0, reg & 7)); /* ModR/M byte with /0 extension for ADD */
        emit_im32(V, imm); /* 32-bit immediate */
    }
}

/* ADD reg32, imm32 */
static void emit_add32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        /* RAX maps to EAX if REX.W is not set */
        emit_u8(V, 0x05);
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 0, reg & 7));   /* /0 extension for ADD */
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 0, reg & 7));   /* /0 extension for ADD */
        emit_im32(V, imm);
    }
}

/* ADD reg16, imm16 */
static void emit_add16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        /* RAX maps to AX if REX.W is not set */
        emit_u8(V, 0x05);
        emit_im16(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83); /* Use sign-extended 8-bit immediate for 16-bit */
        emit_u8(V, MODRM(3, 0, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 0, reg & 7));
        emit_im16(V, imm);
    }
}

/* ADD reg8, imm8 */
static void emit_add8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x04);
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x80);
        emit_u8(V, MODRM(3, 0, reg & 7));   /* /0 extension for ADD */
        emit_u8(V, (uint8_t)imm);
    }
}

/* -- SUB instructions ---------------------------------------------- */

/* SUB reg64, imm32 (sign-extended) */
static void emit_sub_r64i32(VpState* V, X64Reg reg, int32_t imm)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_im32(V, imm);
    }
}

/* SUB reg32, imm32 */
static void emit_sub32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x2D);
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_im32(V, imm);
    }
}

/* SUB reg16, imm16 */
static void emit_sub16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x2D);
        emit_im16(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_im16(V, imm);
    }
}

/* SUB reg8, imm8 */
static void emit_sub8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x2C);
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x80);
        emit_u8(V, MODRM(3, 5, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
}

/* SUB reg64, reg64 */
static void emit_sub64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x29);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* SUB reg32, reg32 */
static void emit_sub32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x29);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* SUB reg16, reg16 */
static void emit_sub16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x29);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* SUB reg8, reg8 */
static void emit_sub8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x28);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* -- CALL instructions --------------------------------------------- */

/* CALL reg64 */
static void emit_call64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* CALL rel32 */
static void emit_call_rel32(VpState* V, int32_t rel)
{
    emit_u8(V, 0xE8);
    emit_im32(V, rel);
}

/* -- INC instructions ---------------------------------------------- */

/* INC reg64 */
static void emit_inc64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 0, reg & 7));
}

/* INC reg32 */
static void emit_inc32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 0, reg & 7));
}

/* INC reg16 */
static void emit_inc16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 0, reg & 7));
}

/* INC reg8 */
static void emit_inc8_r(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xFE);
    emit_u8(V, MODRM(3, 0, reg & 7));
}

/* -- DEC instructions ---------------------------------------------- */

/* DEC reg64 */
static void emit_dec64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 1, reg & 7));
}

/* DEC reg32 */
static void emit_dec32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 1, reg & 7));
}

/* DEC reg16 */
static void emit_dec16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xFF);
    emit_u8(V, MODRM(3, 1, reg & 7));
}

/* DEC reg8 */
static void emit_dec8_r(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xFE);
    emit_u8(V, MODRM(3, 1, reg & 7));
}

/* -- NEG instructions ---------------------------------------------- */

/* NEG reg64 */
static void emit_neg64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* NEG reg32 */
static void emit_neg32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* NEG reg16 */
static void emit_neg16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* NEG reg8 */
static void emit_neg8_r(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xF6);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* -- NOT instructions ---------------------------------------------- */

/* NOT reg64 */
static void emit_not64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* NOT reg32 */
static void emit_not32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* NOT reg16 */
static void emit_not16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* NOT reg8 */
static void emit_not8_r(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xF6);
    emit_u8(V, MODRM(3, 2, reg & 7));
}

/* RET */
static void emit_ret(VpState* V)
{
    emit_u8(V, 0xC3);
}

/* -- PUSH instructions --------------------------------------------- */

/* PUSH reg64 */
static void emit_push64_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0x50 + (reg & 7));
}

/* -- POP instructions ---------------------------------------------- */

/* POP reg64 */
static void emit_pop64_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_W | REX_B);
    emit_u8(V, 0x58 + (reg & 7));
}

/* -- JMP instructions ---------------------------------------------- */

/* JMP rel32 */
static void emit_jmp_rel32(VpState* V, int32_t rel)
{
    emit_u8(V, 0xE9);
    emit_im32(V, rel);
}

/* Jcc rel32 */
static void emit_jcc_rel32(VpState* V, X64CC cc, int32_t rel)
{
    emit_u8(V, 0x0F);
    emit_u8(V, 0x80 + cc);
    emit_im32(V, rel);
}

/* -- SETcc instructions -------------------------------------------- */

/* SETcc reg8 */
static void emit_setcc(VpState* V, X64CC cc, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x90 | (uint8_t)cc);
    emit_u8(V, MODRM(3, 0, reg & 7));
}

/* -- Sign-extension instructions ----------------------------------- */

/* CWDE EAX, AX */
static void emit_cwde(VpState* V)
{
    emit_u8(V, 0x98);
}

/* CDQ EDX:EAX, EAX */
static void emit_cdq(VpState* V)
{
    emit_u8(V, 0x99);
}

/* CQO RDX:RAX, RAX */
static void emit_cqo(VpState* V)
{
    emit_u8(V, REX_W);
    emit_u8(V, 0x99);
}

/* -- MUL instructions ---------------------------------------------- */

/* MUL reg64 */
static void emit_mul64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* MUL reg32 */
static void emit_mul32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* MUL reg16 */
static void emit_mul16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* MUL reg8 */
static void emit_mul8_r(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xF6);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* -- DIV instructions ---------------------------------------------- */

/* DIV reg64 */
static void emit_div64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 6, reg & 7));
}

/* DIV reg32 */
static void emit_div32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 6, reg & 7));
}

/* DIV reg16 */
static void emit_div16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 6, reg & 7));
}

/* DIV reg8 */
static void emit_div8_r(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xF6);
    emit_u8(V, MODRM(3, 6, reg & 7));
}

/* -- IDIV instructions --------------------------------------------- */

/* IDIV reg64 */
static void emit_idiv64_r(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* IDIV reg32 */
static void emit_idiv32_r(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* IDIV reg16 */
static void emit_idiv16_r(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xF7);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* IDIV r/m8 */
static void emit_idiv8_r(VpState* V, X64Reg reg)
{
    /* For 8-bit, if reg is SPL, BPL, SIL, DIL or R8B-R15B, REX.B is needed */
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xF6);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* -- AND instructions ---------------------------------------------- */

/* AND reg32, imm32 */
static void emit_and32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x25); /* AND EAX, imm32 */
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_im32(V, imm);
    }
}

/* AND reg16, imm16 */
static void emit_and16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x25); /* AND AX, imm16 */
        emit_im16(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_im16(V, imm);
    }
}

/* AND reg8, imm8 */
static void emit_and8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x24);
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x80);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
}

/* AND reg64, reg64 */
static void emit_and64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x21);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* AND reg32, reg32 */
static void emit_and32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if (rex) emit_u8(V, rex);
    emit_u8(V, 0x21);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* AND reg16, reg16 */
static void emit_and16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x21);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* AND reg8, reg8 */
static void emit_and8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x20);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* -- OR instructions ----------------------------------------------- */

/* OR reg32, imm32 */
static void emit_or32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x0D);
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 1, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 1, reg & 7));
        emit_im32(V, imm);
    }
}

/* OR reg16, imm16 */
static void emit_or16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x0D);
        emit_im16(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 1, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 1, reg & 7));
        emit_im16(V, imm);
    }
}

/* OR reg8, imm8 */
static void emit_or8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x0C);
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x80);
        emit_u8(V, MODRM(3, 1, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
}

/* OR reg64, reg64 */
static void emit_or64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x09);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* OR reg32, reg32 */
static void emit_or32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x09);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* OR reg16, reg16 */
static void emit_or16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x09);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* OR reg8, reg8 */
static void emit_or8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x08);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* -- XOR instructions ---------------------------------------------- */

/* XOR reg64, reg64 */
static void emit_xor64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x31);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* XOR reg32, reg32 */
static void emit_xor32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x31);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* XOR reg16, reg16 */
static void emit_xor16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x31);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* XOR reg8, reg8 */
static void emit_xor8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x30);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* XOR reg32, imm32 */
static void emit_xor32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x35); /* XOR EAX, imm32 */
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83); /* XOR Ev, Ib (sign-extended 8-bit immediate) */
        emit_u8(V, MODRM(3, 6, reg & 7));   /* /6 extension for XOR */
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81); /* XOR Ev, Iz */
        emit_u8(V, MODRM(3, 6, reg & 7));   /* /6 extension for XOR */
        emit_im32(V, imm);
    }
}

/* XOR reg16, imm16 */
static void emit_xor16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66); /* Operand size prefix for 16-bit */
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        /* RAX maps to AX if REX.W is not set */
        emit_u8(V, 0x35); /* XOR AX, imm16 */
        emit_im16(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83); /* XOR Gw, Ib (sign-extended 8-bit immediate) */
        emit_u8(V, MODRM(3, 6, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81); /* XOR Gw, Iw */
        emit_u8(V, MODRM(3, 6, reg & 7));
        emit_im16(V, imm);
    }
}

/* XOR reg8, imm8 */
static void emit_xor8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x34); /* XOR AL, imm8 */
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x80); /* XOR Gb, Ib */
        emit_u8(V, MODRM(3, 6, reg & 7));   /* /6 extension for XOR */
        emit_u8(V, (uint8_t)imm);
    }
}

/* -- SHL instructions ---------------------------------------------- */

/* SHL reg64, imm8 */
static void emit_shl_r64i8(VpState* V, X64Reg reg, uint8_t imm)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    if(imm == 1)
    {
        emit_u8(V, 0xD1);
        emit_u8(V, MODRM(3, 4, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC1);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, imm);
    }
}

/* SHL reg32, imm8 */
static void emit_shl_r32i8(VpState* V, X64Reg reg, uint8_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(imm == 1)
    {
        emit_u8(V, 0xD1);
        emit_u8(V, MODRM(3, 4, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC1);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, imm);
    }
}

/* SHL reg16, imm8 */
static void emit_shl_r16i8(VpState* V, X64Reg reg, uint8_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(imm == 1)
    {
        emit_u8(V, 0xD1);
        emit_u8(V, MODRM(3, 4, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC1);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, imm);
    }
}

/* SHL reg8, imm8 */
static void emit_shl_r8i8(VpState* V, X64Reg reg, uint8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(imm == 1)
    {
        emit_u8(V, 0xD0);
        emit_u8(V, MODRM(3, 4, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC0);
        emit_u8(V, MODRM(3, 4, reg & 7));
        emit_u8(V, imm);
    }
}

/* SHL reg64, CL */
static void emit_shl_r64cl(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* SHL reg32, CL */
static void emit_shl_r32cl(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* SHL reg16, CL */
static void emit_shl_r16cl(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* SHL reg8, CL */
static void emit_shl_r8cl(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xD2);
    emit_u8(V, MODRM(3, 4, reg & 7));
}

/* -- SHR instructions ---------------------------------------------- */

/* SHR reg64, imm8 */
static void emit_shr_r64i8(VpState* V, X64Reg reg, uint8_t imm)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xC1);
    emit_u8(V, MODRM(3, 5, reg & 7));
    emit_u8(V, imm);
}

/* SHR reg32, imm8 */
static void emit_shr_r32i8(VpState* V, X64Reg reg, uint8_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xC1);
    emit_u8(V, MODRM(3, 5, reg & 7));
    emit_u8(V, imm);
}

/* SHR reg16, imm8 */
static void emit_shr_r16i8(VpState* V, X64Reg reg, uint8_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xC1);
    emit_u8(V, MODRM(3, 5, reg & 7));
    emit_u8(V, imm);
}

/* SHR reg8, imm8 */
static void emit_shr8_ri(VpState* V, X64Reg reg, uint8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xC0);
    emit_u8(V, MODRM(3, 5, reg & 7));
    emit_u8(V, imm);
}

/* SHR reg64, CL */
static void emit_shr_r64cl(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 5, reg & 7));
}

/* SHR reg32, CL */
static void emit_shr_r32cl(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 5, reg & 7));
}

/* SHR reg16, CL */
static void emit_shr_r16cl(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 5, reg & 7));
}

/* SHR reg8, CL */
static void emit_shr_r8cl(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xD2);
    emit_u8(V, MODRM(3, 5, reg & 7));
}

/* -- SAR instructions ---------------------------------------------- */

/* SAR reg64, imm8 */
static void emit_sar_r64i8(VpState* V, X64Reg reg, uint8_t imm)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    if(imm == 1)
    {
        emit_u8(V, 0xD1);
        emit_u8(V, MODRM(3, 7, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC1);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
}

/* SAR reg32, imm8 */
static void emit_sar_r32i8(VpState* V, X64Reg reg, uint8_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(imm == 1)
    {
        emit_u8(V, 0xD1);
        emit_u8(V, MODRM(3, 7, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC1);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, imm);
    }
}

/* SAR reg16, imm8 */
static void emit_sar_r16i8(VpState* V, X64Reg reg, uint8_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(imm == 1)
    {
        emit_u8(V, 0xD1);
        emit_u8(V, MODRM(3, 7, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC1);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, imm);
    }
}

/* SAR reg8, imm8 */
static void emit_sar_r8i8(VpState* V, X64Reg reg, uint8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(imm == 1)
    {
        emit_u8(V, 0xD0);
        emit_u8(V, MODRM(3, 7, reg & 7));
    }
    else
    {
        emit_u8(V, 0xC0);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, imm);
    }
}

/* SAR reg64, CL */
static void emit_sar_r64cl(VpState* V, X64Reg reg)
{
    uint8_t rex = REX_W;
    if(regext(reg)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* SAR reg32, CL */
static void emit_sar_r32cl(VpState* V, X64Reg reg)
{
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* SAR reg16, CL */
static void emit_sar_r16cl(VpState* V, X64Reg reg)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    emit_u8(V, 0xD3);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* SAR reg8, CL */
static void emit_sar_r8cl(VpState* V, X64Reg reg)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    emit_u8(V, 0xD2);
    emit_u8(V, MODRM(3, 7, reg & 7));
}

/* -- TEST instructions --------------------------------------------- */

/* TEST reg64, reg64 */
static void emit_test64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x85);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* TEST reg32, reg32 */
static void emit_test32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if (rex) emit_u8(V, rex);
    emit_u8(V, 0x85);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* TEST reg16, reg16 */
static void emit_test16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x85);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* TEST reg8, reg8 */
static void emit_test8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x84);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* -- CMP instructions ---------------------------------------------- */

/* CMP reg32, imm32 */
static void emit_cmp32_ri(VpState* V, X64Reg reg, int32_t imm)
{
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x3D);
        emit_im32(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_im32(V, imm);
    }
}

/* CMP reg16, imm16 */
static void emit_cmp16_ri(VpState* V, X64Reg reg, int16_t imm)
{
    emit_u8(V, 0x66);
    if(regext(reg)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x3D);
        emit_im16(V, imm);
    }
    else if(vp_isimm8(imm))
    {
        emit_u8(V, 0x83);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x81);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_im16(V, imm);
    }
}

/* CMP reg8, imm8 */
static void emit_cmp8_ri(VpState* V, X64Reg reg, int8_t imm)
{
    if(regext(reg) || (reg >= RSP && reg <= RDI)) emit_u8(V, REX_B);
    if(reg == RAX)
    {
        emit_u8(V, 0x3C);
        emit_u8(V, (uint8_t)imm);
    }
    else
    {
        emit_u8(V, 0x80);
        emit_u8(V, MODRM(3, 7, reg & 7));
        emit_u8(V, (uint8_t)imm);
    }
}

/* CMP reg64, reg64 */
static void emit_cmp64_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    emit_u8(V, rex);
    emit_u8(V, 0x39);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* CMP reg32, reg32 */
static void emit_cmp32_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if (rex) emit_u8(V, rex);
    emit_u8(V, 0x39);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* CMP reg16, reg16 */
static void emit_cmp16_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_u8(V, 0x66);
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x39);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* CMP reg8, reg8 */
static void emit_cmp8_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src) || (src >= RSP && src <= RDI)) rex |= REX_R;
    if(regext(dst) || (dst >= RSP && dst <= RDI)) rex |= REX_B;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x38);
    emit_u8(V, MODRM(3, src & 7, dst & 7));
}

/* Emit common SSE/SSE2 rr instructions */
static VP_AINLINE void emit_sse_rr(VpState* V, uint8_t prefix, uint8_t op, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_R;
    if(regext(dst)) rex |= REX_B;
    if(prefix) emit_u8(V, prefix);
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, op);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* MOVSD xmm, xmm */
static void emit_movsd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF2, 0x10, dst, src);
}

/* ADDSD xmm, xmm */
static void emit_addsd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF2, 0x58, dst, src);
}

/* SUBSD xmm, xmm */
static void emit_subsd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF2, 0x5C, dst, src);
}

/* MULSD xmm, xmm */
static void emit_mulsd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF2, 0x59, dst, src);
}

/* DIVSD xmm, xmm */
static void emit_divsd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF2, 0x5E, dst, src);
}

/* XORPD xmm, xmm */
static void emit_xorpd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0x66, 0x57, dst, src);
}

/* COMISD xmm, xmm */
static void emit_comisd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0x66, 0x2F, dst, src);
}

/* UCOMISD xmm, xmm */
static void emit_ucomisd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0x66, 0x2E, dst, src);
}

/* MOVSS xmm, xmm */
static void emit_movss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF3, 0x10, dst, src);
}

/* ADDSS xmm, xmm */
static void emit_addss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF3, 0x58, dst, src);
}

/* SUBSS xmm, xmm */
static void emit_subss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF3, 0x5C, dst, src);
}

/* MULSS xmm, xmm */
static void emit_mulss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF3, 0x59, dst, src);
}

/* DIVSS xmm, xmm */
static void emit_divss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0xF3, 0x5E, dst, src);
}

/* XORPS xmm, xmm */
static void emit_xorps_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0x00, 0x57, dst, src);
}

/* COMISS xmm, xmm */
static void emit_comiss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0x00, 0x2F, dst, src);
}

/* UCOMISS xmm, xmm */
static void emit_ucomiss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    emit_sse_rr(V, 0x00, 0x2E, dst, src);
}

/* Emit common SSE/SSE2 instructions with memory operand (load/store) */
static void emit_sse_mem(VpState* V, uint8_t prefix, uint8_t op, X64Reg reg, X64Mem mem)
{
    X64Reg base, idx;
    uint8_t scale;
    int32_t disp;
    mem_op(mem, &base, &idx, &scale, &disp);

    if(idx != NOREG)
    {
        vp_assertX(IS_POW2(scale) && scale <= 8, "bad scale (1/2/4/8)");
    }

    if(prefix) emit_u8(V, prefix);

    uint8_t rex = rex_mem(reg, base, idx, false);
    if(rex) emit_u8(V, rex);
    emit_u8(V, op);

    emit_modrm_sib_disp(V, reg, base, idx, scale, disp);
}

/* MOVSS xmm, [base + index*scale + disp32] */
static void emit_movss_rm(VpState* V, X64Reg dst, X64Mem mem)
{
    emit_sse_mem(V, 0xF3, 0x10, dst, mem);
}

/* MOVSS [base + index*scale + disp32], xmm */
static void emit_movss_mr(VpState* V, X64Mem mem, X64Reg src)
{
    emit_sse_mem(V, 0xF3, 0x11, src, mem);
}

/* MOVSD xmm, [base + index*scale + disp32] */
static void emit_movsd_rm(VpState* V, X64Reg dst, X64Mem mem)
{
    emit_sse_mem(V, 0xF2, 0x10, dst, mem);
}

/* MOVSD [base + index*scale + disp32], xmm */
static void emit_movsd_mr(VpState* V, X64Mem mem, X64Reg src)
{
    emit_sse_mem(V, 0xF2, 0x11, src, mem);
}

/* CVTSD2SS xmm, xmm */
static void emit_cvtsd2ss_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;

    emit_u8(V, 0xF2); /* CVTSD2SS prefix */
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x5A);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTSS2SD xmm, xmm */
static void emit_cvtss2sd_rr(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;

    emit_u8(V, 0xF3); /* CVTSS2SD prefix */
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x5A);
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTTSS2SI r32, xmm */
static void emit_cvttss2si_r32x(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;

    emit_u8(V, 0xF3); /* F3 prefix for SSE */
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2C); /* CVTTSS2SI opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTTSS2SI r64, xmm */
static void emit_cvttss2si_r64x(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;

    emit_u8(V, 0xF3); /* F3 prefix for SSE */
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2C); /* CVTTSS2SI opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTTSD2SI r32, xmm */
static void emit_cvttsd2si_r32x(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;

    emit_u8(V, 0xF2); /* F2 prefix for SSE */
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2C); /* CVTTSD2SI opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTTSD2SI r64, xmm */
static void emit_cvttsd2si_r64x(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(dst)) rex |= REX_R;
    if(regext(src)) rex |= REX_B;

    emit_u8(V, 0xF2); /* F2 prefix for SSE */
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2C); /* CVTTSD2SI opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTSI2SS xmm, reg32 */
static void emit_cvtsi2ss_xr32(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_B;
    if(regext(dst)) rex |= REX_R;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0xF3); /* F3 prefix */
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2A); /* CVTSI2SS opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTSI2SS xmm, reg64 */
static void emit_cvtsi2ss_xr64(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_B;
    if(regext(dst)) rex |= REX_R;
    emit_u8(V, rex);
    emit_u8(V, 0xF3); /* F3 prefix */
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2A); /* CVTSI2SS opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTSI2SD xmm, reg32 */
static void emit_cvtsi2sd_xr32(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = 0;
    if(regext(src)) rex |= REX_B;
    if(regext(dst)) rex |= REX_R;
    if(rex) emit_u8(V, rex);
    emit_u8(V, 0xF2); /* F2 prefix */
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2A); /* CVTSI2SD opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* CVTSI2SD xmm, reg64 */
static void emit_cvtsi2sd_xr64(VpState* V, X64Reg dst, X64Reg src)
{
    uint8_t rex = REX_W;
    if(regext(src)) rex |= REX_B;
    if(regext(dst)) rex |= REX_R;
    emit_u8(V, rex);
    emit_u8(V, 0xF2); /* F2 prefix */
    emit_u8(V, 0x0F);
    emit_u8(V, 0x2A); /* CVTSI2SD opcode */
    emit_u8(V, MODRM(3, dst & 7, src & 7));
}

/* RDTSC */
static void emit_rdtsc(VpState* V)
{
    emit_u8(V, 0x0f);
    emit_u8(V, 0x31);
}

/* CPUID */
static void emit_cpuid(VpState* V)
{
    emit_u8(V, 0x0f);
    emit_u8(V, 0xa2);
}

static void emit_mov_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_mov8_rr(V, r1, r2); break;
    case VRSize2: emit_mov16_rr(V, r1, r2); break;
    case VRSize4: emit_mov32_rr(V, r1, r2); break;
    case VRSize8: emit_mov64_rr(V, r1, r2); break;
    }
}

static void emit_mov_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_mov8_ri(V, r, i); break;
    case VRSize2: emit_mov16_ri(V, r, i); break;
    case VRSize4: emit_mov32_ri(V, r, i); break;
    case VRSize8: emit_mov64_ri(V, r, i); break;
    }
}

static void emit_add_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_add8_rr(V, r1, r2); break;
    case VRSize2: emit_add16_rr(V, r1, r2); break;
    case VRSize4: emit_add32_rr(V, r1, r2); break;
    case VRSize8: emit_add64_rr(V, r1, r2); break;
    }
}

static void emit_add_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_add8_ri(V, r, i); break;
    case VRSize2: emit_add16_ri(V, r, i); break;
    case VRSize4: emit_add32_ri(V, r, i); break;
    case VRSize8: emit_add64_ri(V, r, i); break;
    }
}

static void emit_sub_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_sub8_rr(V, r1, r2); break;
    case VRSize2: emit_sub16_rr(V, r1, r2); break;
    case VRSize4: emit_sub32_rr(V, r1, r2); break;
    case VRSize8: emit_sub64_rr(V, r1, r2); break;
    }
}

static void emit_sub_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_sub8_ri(V, r, i); break;
    case VRSize2: emit_sub16_ri(V, r, i); break;
    case VRSize4: emit_sub32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_mul_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_mul8_r(V, r); break;
    case VRSize2: emit_mul16_r(V, r); break;
    case VRSize4: emit_mul32_r(V, r); break;
    case VRSize8: emit_mul64_r(V, r); break;
    }
}

static void emit_div_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_div8_r(V, r); break;
    case VRSize2: emit_div16_r(V, r); break;
    case VRSize4: emit_div32_r(V, r); break;
    case VRSize8: emit_div64_r(V, r); break;
    }
}

static void emit_idiv_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_idiv8_r(V, r); break;
    case VRSize2: emit_idiv16_r(V, r); break;
    case VRSize4: emit_idiv32_r(V, r); break;
    case VRSize8: emit_idiv64_r(V, r); break;
    }
}

static void emit_and_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_and8_rr(V, r1, r2); break;
    case VRSize2: emit_and16_rr(V, r1, r2); break;
    case VRSize4: emit_and32_rr(V, r1, r2); break;
    case VRSize8: emit_and64_rr(V, r1, r2); break;
    }
}

static void emit_and_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_and8_ri(V, r, i); break;
    case VRSize2: emit_and16_ri(V, r, i); break;
    case VRSize4: emit_and32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_or_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_or8_rr(V, r1, r2); break;
    case VRSize2: emit_or16_rr(V, r1, r2); break;
    case VRSize4: emit_or32_rr(V, r1, r2); break;
    case VRSize8: emit_or64_rr(V, r1, r2); break;
    }
}

static void emit_or_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_or8_ri(V, r, i); break;
    case VRSize2: emit_or16_ri(V, r, i); break;
    case VRSize4: emit_or32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_xor_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_xor8_rr(V, r1, r2); break;
    case VRSize2: emit_xor16_rr(V, r1, r2); break;
    case VRSize4: emit_xor32_rr(V, r1, r2); break;
    case VRSize8: emit_xor64_rr(V, r1, r2); break;
    }
}

static void emit_xor_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_xor8_ri(V, r, i); break;
    case VRSize2: emit_xor16_ri(V, r, i); break;
    case VRSize4: emit_xor32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_shl_ri(VRSize p, uint32_t r, uint8_t i)
{
    switch(p)
    {
    case VRSize1: emit_shl_r8i8(V, r, i); break;
    case VRSize2: emit_shl_r16i8(V, r, i); break;
    case VRSize4: emit_shl_r32i8(V, r, i); break;
    case VRSize8: emit_shl_r64i8(V, r, i); break;
    }
}

static void emit_shl_rcl(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_shl_r8cl(V, r); break;
    case VRSize2: emit_shl_r16cl(V, r); break;
    case VRSize4: emit_shl_r32cl(V, r); break;
    case VRSize8: emit_shl_r64cl(V, r); break;
    }
}

static void emit_shr_ri(VRSize p, uint32_t r, uint8_t i)
{
    switch(p)
    {
    case VRSize1: emit_shr8_ri(V, r, i); break;
    case VRSize2: emit_shr_r16i8(V, r, i); break;
    case VRSize4: emit_shr_r32i8(V, r, i); break;
    case VRSize8: emit_shr_r64i8(V, r, i); break;
    }
}

static void emit_shr_rcl(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_shr_r8cl(V, r); break;
    case VRSize2: emit_shr_r16cl(V, r); break;
    case VRSize4: emit_shr_r32cl(V, r); break;
    case VRSize8: emit_shr_r64cl(V, r); break;
    }
}

static void emit_sar_ri(VRSize p, uint32_t r, uint8_t i)
{
    switch(p)
    {
    case VRSize1: emit_sar_r8i8(V, r, i); break;
    case VRSize2: emit_sar_r16i8(V, r, i); break;
    case VRSize4: emit_sar_r32i8(V, r, i); break;
    case VRSize8: emit_sar_r64i8(V, r, i); break;
    }
}

static void emit_sar_rcl(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_sar_r8cl(V, r); break;
    case VRSize2: emit_sar_r16cl(V, r); break;
    case VRSize4: emit_sar_r32cl(V, r); break;
    case VRSize8: emit_sar_r64cl(V, r); break;
    }
}

static void emit_inc_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_inc8_r(V, r); break;
    case VRSize2: emit_inc16_r(V, r); break;
    case VRSize4: emit_inc32_r(V, r); break;
    case VRSize8: emit_inc64_r(V, r); break;
    }
}

static void emit_dec_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_dec8_r(V, r); break;
    case VRSize2: emit_dec16_r(V, r); break;
    case VRSize4: emit_dec32_r(V, r); break;
    case VRSize8: emit_dec64_r(V, r); break;
    }
}

static void emit_neg_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_neg8_r(V, r); break;
    case VRSize2: emit_neg16_r(V, r); break;
    case VRSize4: emit_neg32_r(V, r); break;
    case VRSize8: emit_neg64_r(V, r); break;
    }
}

static void emit_not_r(VRSize p, uint32_t r)
{
    switch(p)
    {
    case VRSize1: emit_not8_r(V, r); break;
    case VRSize2: emit_not16_r(V, r); break;
    case VRSize4: emit_not32_r(V, r); break;
    case VRSize8: emit_not64_r(V, r); break;
    }
}

static void emit_test_r(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_test8_rr(V, r1, r2); break;
    case VRSize2: emit_test16_rr(V, r1, r2); break;
    case VRSize4: emit_test32_rr(V, r1, r2); break;
    case VRSize8: emit_test64_rr(V, r1, r2); break;
    }
}

static void emit_cmp_ri(VRSize p, uint32_t r, int64_t i)
{
    switch(p)
    {
    case VRSize1: emit_cmp8_ri(V, r, i); break;
    case VRSize2: emit_cmp16_ri(V, r, i); break;
    case VRSize4: emit_cmp32_ri(V, r, i); break;
    case VRSize8: vp_assertX(0, "?"); break;
    }
}

static void emit_cmp_rr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize1: emit_cmp8_rr(V, r1, r2); break;
    case VRSize2: emit_cmp16_rr(V, r1, r2); break;
    case VRSize4: emit_cmp32_rr(V, r1, r2); break;
    case VRSize8: emit_cmp64_rr(V, r1, r2); break;
    }
}

static void emit_movsx_rr(VRSize pd, VRSize ps, X64Reg dst, X64Reg src)
{
    switch(pd)
    {
        case VRSize8:
            switch(ps)
            {
                case VRSize4: emit_movsx_r64r32(V, dst, src); break;
                case VRSize2: emit_movsx_r64r16(V, dst, src); break;
                case VRSize1: emit_movsx_r64r8(V, dst, src); break;
                default: vp_assertX(0, "?"); break;
            }
            break;
        case VRSize4:
            switch(ps)
            {
                case VRSize2: emit_movsx_r32r16(V, dst, src); break;
                case VRSize1: emit_movsx_r32r8(V, dst, src); break;
                default: vp_assertX(0, "?"); break;
            }
            break;
        case VRSize2:
            switch(ps)
            {
                case VRSize1: emit_movsx_r16r8(V, dst, src); break;
                default: vp_assertX(0, "?"); break;
            }
            break;
        default:
            vp_assertX(0, "?");
            break;
    }
}

static void emit_movzx_rr(VRSize pd, VRSize ps, uint32_t r1, uint32_t r2)
{
    switch(pd)
    {
    case VRSize2:
        vp_assertX(ps == VRSize1, "invalid size combination");
        emit_movzx_r16r8(V, r1, r2);
        break;
    case VRSize4:
        switch(ps)
        {
        case VRSize1: emit_movzx_r32r8(V, r1, r2); break;
        case VRSize2: emit_movzx_r32r16(V, r1, r2); break;
        default: vp_assertX(0, "?"); break;
        }
        break;
    case VRSize8:
        switch(ps)
        {
        case VRSize1: emit_movzx_r64r8(V, r1, r2); break;
        case VRSize2: emit_movzx_r64r16(V, r1, r2); break;
        default: vp_assertX(0, "?"); break;
        }
        break;
    default:
        vp_assertX(0, "?");
        break;
    }
}

static void emit_cvttss2si_rx(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize4: emit_cvttss2si_r32x(V, r1, r2); break;
    case VRSize8: emit_cvttss2si_r64x(V, r1, r2); break;
    default: vp_assertX(0, "?"); break;
    }
}

static void emit_cvttsd2si_rx(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
    case VRSize4: emit_cvttsd2si_r32x(V, r1, r2); break;
    case VRSize8: emit_cvttsd2si_r64x(V, r1, r2); break;
    default: vp_assertX(0, "?"); break;
    }
}

static void emit_cvtsi2ss_xr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
        case VRSize4: emit_cvtsi2ss_xr64(V, r1, r2);
        case VRSize8: emit_cvtsi2ss_xr32(V, r1, r2);
        default: vp_assertX(0, "?"); break;
    }
}

static void emit_cvtsi2sd_xr(VRSize p, uint32_t r1, uint32_t r2)
{
    switch(p)
    {
        case VRSize4: emit_cvtsi2sd_xr64(V, r1, r2);
        case VRSize8: emit_cvtsi2sd_xr32(V, r1, r2);
        default: vp_assertX(0, "?"); break;
    }
}