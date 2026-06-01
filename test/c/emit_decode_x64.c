/*
** emit_decode_x64.c
** Tests x64 emitter bytes by decoding and dumping them through vp_dump
*/

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "vp_buf.h"
#include "vp_dump.h"
#include "vp_emit_x64.h"
#include "vp_state.h"
#include "vp_target_x64.h"

static int failures;
static uint32_t tests_run;

static void put_hex(SBuf* sb, const uint8_t* p, uint32_t size)
{
    for(uint32_t i = 0; i < size; i++)
    {
        vp_buf_fmt(sb, "%s%02x", i ? " " : "", p[i]);
    }
}

static void check_emit(const char* emit, const char* expect, uint32_t start, uint32_t size, uint32_t line)
{
    SBuf got;
    vp_buf_init(&got);
    vp_dump_rawX64(&got, V->code.b + start, size);

    size_t got_len = sbuf_len(&got);
    size_t expect_len = strlen(expect);
    SBuf out;
    vp_buf_init(&out);

    if(got_len != expect_len || memcmp(got.b, expect, expect_len) != 0)
    {
        failures++;
        vp_buf_fmt(&out, "fail line %u: %s\nbytes: ", line, emit);
        put_hex(&out, (const uint8_t*)V->code.b + start, size);
        vp_buf_fmt(&out, "\nexpected:\n%sactual:\n%.*s\n", expect, (int)got_len, got.b);
        fwrite(out.b, 1, sbuf_len(&out), stdout);
        return;
    }

    vp_buf_fmt(&out, "ok   %.*s\n", (int)(expect_len - 1), expect);
    fwrite(out.b, 1, sbuf_len(&out), stdout);
}

#define TEST_EMIT(emit, expect) \
    do { \
        uint32_t start_ = (uint32_t)sbuf_len(&V->code); \
        emit; \
        uint32_t size_ = (uint32_t)sbuf_len(&V->code) - start_; \
        tests_run++; \
        check_emit(#emit, expect, start_, size_, __LINE__); \
    } while(0)

int main(void)
{
    VpState* state = vp_state_open();

    /* -- TEST -- */
    TEST_EMIT(vp_emitX64_testRR(SIL, SIL),
        "test      sil, sil\n");
    TEST_EMIT(vp_emitX64_testRR(BX, BX),
        "test      bx, bx\n");
    TEST_EMIT(vp_emitX64_testRR(DL, DL),
        "test      dl, dl\n");
    TEST_EMIT(vp_emitX64_testRR(R12D, R12D),
        "test      r12d, r12d\n");
    TEST_EMIT(vp_emitX64_testRR(R15, R15),
        "test      r15, r15\n");
    TEST_EMIT(vp_emitX64_testRR(SPL, SPL),
        "test      spl, spl\n");
    TEST_EMIT(vp_emitX64_testRR(BPL, BPL),
        "test      bpl, bpl\n");
    /* -- XOR -- */
    TEST_EMIT(vp_emitX64_xorRI(AL, 6),
        "xor       al, 0x6\n");
    TEST_EMIT(vp_emitX64_xorRR(RDX, RDX),
        "xor       rdx, rdx\n");
    TEST_EMIT(vp_emitX64_xorRR(EDX, EDX),
        "xor       edx, edx\n");
    TEST_EMIT(vp_emitX64_xorRR(DX, DX),
        "xor       dx, dx\n");
    TEST_EMIT(vp_emitX64_xorRR(DL, DL),
        "xor       dl, dl\n");
    TEST_EMIT(vp_emitX64_xorRR(R8, R8),
        "xor       r8, r8\n");
    TEST_EMIT(vp_emitX64_xorRR(R12D, R12D),
        "xor       r12d, r12d\n");
    /* -- OR -- */
    TEST_EMIT(vp_emitX64_orRI(AL, 11),
        "or        al, 0xb\n");
    TEST_EMIT(vp_emitX64_orRR(R13, R14),
        "or        r13, r14\n");
    TEST_EMIT(vp_emitX64_orRI(RAX, 0xFF),
        "or        rax, 0xff\n");
    TEST_EMIT(vp_emitX64_orRI(EBX, 0x80000000),
        "or        ebx, 0x80000000\n");
    /* -- AND -- */
    TEST_EMIT(vp_emitX64_andRI(AX, 897),
        "and       ax, 0x381\n");
    TEST_EMIT(vp_emitX64_andRI(RDI, 5),
        "and       rdi, 0x5\n");
    TEST_EMIT(vp_emitX64_andRI(RDI, 5),
        "and       rdi, 0x5\n");
    TEST_EMIT(vp_emitX64_andRR(RAX, RCX),
        "and       rax, rcx\n");
    TEST_EMIT(vp_emitX64_andRR(R8D, R9D),
        "and       r8d, r9d\n");
    TEST_EMIT(vp_emitX64_andRI(EAX, 0xFF),
        "and       eax, 0xff\n");
    TEST_EMIT(vp_emitX64_andRI(RAX, INT32_MAX),
        "and       rax, 0x7fffffff\n");
    /* -- LEA -- */
    TEST_EMIT(vp_emitX64_leaRM(RDI, X64MEM(RN_BP, NOREG, 2, 8, 8)),
        "lea       rdi, qword ptr [rbp+8]\n");
    TEST_EMIT(vp_emitX64_leaRM(RAX, X64MEM(RN_SP, NOREG, 1, 0, 8)),
        "lea       rax, qword ptr [rsp]\n");
    TEST_EMIT(vp_emitX64_leaRM(R11, X64MEM(RN_12, RN_14, 8, -32, 8)),
        "lea       r11, qword ptr [r12+r14*8-32]\n");
    TEST_EMIT(vp_emitX64_leaRM(RDX, X64MEM_RIP(-4, 8)),
        "lea       rdx, qword ptr [rip-4]\n");
    /* -- ADD -- */
    TEST_EMIT(vp_emitX64_addRI(AL, 127),
        "add       al, 0x7f\n");
    TEST_EMIT(vp_emitX64_addRI(AX, 4567),
        "add       ax, 0x11d7\n");
    TEST_EMIT(vp_emitX64_addRI(EAX, 16),
        "add       eax, 0x10\n");
    TEST_EMIT(vp_emitX64_addRI(RAX, INT32_MAX),
        "add       rax, 0x7fffffff\n");
    TEST_EMIT(vp_emitX64_addRI(BH, 8),
        "add       bh, 0x8\n");
    TEST_EMIT(vp_emitX64_addRR(DH, CH),
        "add       dh, ch\n");
    TEST_EMIT(vp_emitX64_addRR(RSP, RBP),
        "add       rsp, rbp\n");
    TEST_EMIT(vp_emitX64_addRR(R12, R13),
        "add       r12, r13\n");
    TEST_EMIT(vp_emitX64_addRI(RSP, -8),
        "add       rsp, 0xfffffffffffffff8\n");
    TEST_EMIT(vp_emitX64_addRI(RCX, INT32_MIN),
        "add       rcx, 0xffffffff80000000\n");
    /* -- DIV -- */
    TEST_EMIT(vp_emitX64_divR(AL),
        "div       al\n");
    TEST_EMIT(vp_emitX64_divR(SI),
        "div       si\n");
    TEST_EMIT(vp_emitX64_divR(EBX),
        "div       ebx\n");
    TEST_EMIT(vp_emitX64_divR(R10),
        "div       r10\n");
    TEST_EMIT(vp_emitX64_divR(R9W),
        "div       r9w\n");
    TEST_EMIT(vp_emitX64_divR(RCX),
        "div       rcx\n");
    /* -- PUSH -- */
    TEST_EMIT(vp_emitX64_pushR(RAX),
        "push      rax\n");
    TEST_EMIT(vp_emitX64_pushR(R12),
        "push      r12\n");
    TEST_EMIT(vp_emitX64_pushR(RSP),
        "push      rsp\n");
    TEST_EMIT(vp_emitX64_pushR(RBP),
        "push      rbp\n");
    /* -- POP -- */
    TEST_EMIT(vp_emitX64_popR(R13),
        "pop       r13\n");
    TEST_EMIT(vp_emitX64_popR(RAX),
        "pop       rax\n");
    TEST_EMIT(vp_emitX64_popR(RSP),
        "pop       rsp\n");
    TEST_EMIT(vp_emitX64_popR(R15),
        "pop       r15\n");
    /* -- NEG -- */
    TEST_EMIT(vp_emitX64_negR(AL),
        "neg       al\n");
    TEST_EMIT(vp_emitX64_negR(R8W),
        "neg       r8w\n");
    TEST_EMIT(vp_emitX64_negR(RSP),
        "neg       rsp\n");
    TEST_EMIT(vp_emitX64_negR(R12D),
        "neg       r12d\n");
    /* -- INC -- */
    TEST_EMIT(vp_emitX64_incR(R13D),
        "inc       r13d\n");
    TEST_EMIT(vp_emitX64_incR(RAX),
        "inc       rax\n");
    TEST_EMIT(vp_emitX64_incR(R8),
        "inc       r8\n");
    TEST_EMIT(vp_emitX64_incR(SPL),
        "inc       spl\n");
    /* -- DEC -- */
    TEST_EMIT(vp_emitX64_decR(ECX),
        "dec       ecx\n");
    TEST_EMIT(vp_emitX64_decR(R15),
        "dec       r15\n");
    TEST_EMIT(vp_emitX64_decR(AX),
        "dec       ax\n");
    /* -- NOT -- */
    TEST_EMIT(vp_emitX64_notR(R11),
        "not       r11\n");
    TEST_EMIT(vp_emitX64_notR(EAX),
        "not       eax\n");
    TEST_EMIT(vp_emitX64_notR(R8B),
        "not       r8b\n");
    /* -- MUL -- */
    TEST_EMIT(vp_emitX64_mulR(RBX),
        "mul       rbx\n");
    TEST_EMIT(vp_emitX64_mulR(AL),
        "mul       al\n");
    TEST_EMIT(vp_emitX64_mulR(R15D),
        "mul       r15d\n");
    /* -- IDIV -- */
    TEST_EMIT(vp_emitX64_idivR(DL),
        "idiv      dl\n");
    TEST_EMIT(vp_emitX64_idivR(R15W),
        "idiv      r15w\n");
    TEST_EMIT(vp_emitX64_idivR(ECX),
        "idiv      ecx\n");
    TEST_EMIT(vp_emitX64_idivR(RAX),
        "idiv      rax\n");
    TEST_EMIT(vp_emitX64_idivR(R12),
        "idiv      r12\n");
    /* -- SUB -- */
    TEST_EMIT(vp_emitX64_subRI(AL, INT8_MAX),
        "sub       al, 0x7f\n");
    TEST_EMIT(vp_emitX64_subRI(AX, INT16_MAX),
        "sub       ax, 0x7fff\n");
    TEST_EMIT(vp_emitX64_subRI(EAX, INT32_MAX),
        "sub       eax, 0x7fffffff\n");
    TEST_EMIT(vp_emitX64_subRI(RSP, 32),
        "sub       rsp, 0x20\n");
    TEST_EMIT(vp_emitX64_subRR(RSP, RBP),
        "sub       rsp, rbp\n");
    TEST_EMIT(vp_emitX64_subRR(R8, R9),
        "sub       r8, r9\n");
    TEST_EMIT(vp_emitX64_subRI(RBP, 128),
        "sub       rbp, 0x80\n");
    /* -- SAR -- */
    TEST_EMIT(vp_emitX64_sarRI(EAX, 17),
        "sar       eax, 0x11\n");
    TEST_EMIT(vp_emitX64_sarRCL(EAX),
        "sar       eax, cl\n");
    TEST_EMIT(vp_emitX64_sarRI(R15, 1),
        "sar       r15, 0x1\n");
    TEST_EMIT(vp_emitX64_sarRCL(R8),
        "sar       r8, cl\n");
    /* -- SHL -- */
    TEST_EMIT(vp_emitX64_shlRI(RAX, 3),
        "shl       rax, 0x3\n");
    TEST_EMIT(vp_emitX64_shlRCL(RCX),
        "shl       rcx, cl\n");
    TEST_EMIT(vp_emitX64_shlRI(AL, 7),
        "shl       al, 0x7\n");
    TEST_EMIT(vp_emitX64_shlRI(R11D, 1),
        "shl       r11d, 0x1\n");
    TEST_EMIT(vp_emitX64_shlRCL(AL),
        "shl       al, cl\n");
    /* -- SHR -- */
    TEST_EMIT(vp_emitX64_shrRI(AL, 1),
        "shr       al, 0x1\n");
    TEST_EMIT(vp_emitX64_shrRCL(RDX),
        "shr       rdx, cl\n");
    TEST_EMIT(vp_emitX64_shrRI(R9, 63),
        "shr       r9, 0x3f\n");
    TEST_EMIT(vp_emitX64_shrRCL(R12),
        "shr       r12, cl\n");
    /* -- CMP -- */
    TEST_EMIT(vp_emitX64_cmpRR(RSI, RDI),
        "cmp       rsi, rdi\n");
    TEST_EMIT(vp_emitX64_cmpRI(AL, 76),
        "cmp       al, 0x4c\n");
    TEST_EMIT(vp_emitX64_cmpRI(R9D, 32),
        "cmp       r9d, 0x20\n");
    TEST_EMIT(vp_emitX64_cmpRR(R12, R13),
        "cmp       r12, r13\n");
    TEST_EMIT(vp_emitX64_cmpRR(EAX, EBX),
        "cmp       eax, ebx\n");
    TEST_EMIT(vp_emitX64_cmpRI(RAX, -1),
        "cmp       rax, 0xffffffffffffffff\n");
    TEST_EMIT(vp_emitX64_cmpRI(RDX, INT32_MIN),
        "cmp       rdx, 0xffffffff80000000\n");
    TEST_EMIT(vp_emitX64_cmpRI(AX, 0),
        "cmp       ax, 0x0\n");
    /* -- MOVDQA -- */
    TEST_EMIT(vp_emitX64_movdqaXM(XMM0, X64MEM(RN_AX, NOREG, 1, 0, 8)),
        "movdqa    xmm0, xmmword ptr [rax]\n");
    TEST_EMIT(vp_emitX64_movdqaMX(X64MEM(RN_SP, NOREG, 1, 16, 8), XMM0),
        "movdqa    xmmword ptr [rsp+16], xmm0\n");
    /* -- MOVUPS -- */
    TEST_EMIT(vp_emitX64_movupsXM(XMM3, X64MEM(RN_BX, RN_CX, 2, 0, 8)),
        "movups    xmm3, xmmword ptr [rbx+rcx*2]\n");
    TEST_EMIT(vp_emitX64_movupsMX(X64MEM(RN_DI, NOREG, 1, 0, 8), XMM5),
        "movups    xmmword ptr [rdi], xmm5\n");
    /* -- MOVD -- */
    TEST_EMIT(vp_emitX64_movdXR(XMM11, EAX),
        "movd      xmm11, eax\n");
    TEST_EMIT(vp_emitX64_movdXR(XMM0, EAX),
        "movd      xmm0, eax\n");
    TEST_EMIT(vp_emitX64_movdXR(XMM8, R12D),
        "movd      xmm8, r12d\n");
    /* -- MOVQ -- */
    TEST_EMIT(vp_emitX64_movqXR(XMM5, RSI),
        "movq      xmm5, rsi\n");
    TEST_EMIT(vp_emitX64_movqXR(XMM0, RAX),
        "movq      xmm0, rax\n");
    TEST_EMIT(vp_emitX64_movqXR(XMM15, R15),
        "movq      xmm15, r15\n");
    /* -- MOV -- */
    TEST_EMIT(vp_emitX64_movRR(AH, BH),
        "mov       ah, bh\n");
    TEST_EMIT(vp_emitX64_movRR(RBP, RSP),
        "mov       rbp, rsp\n");
    TEST_EMIT(vp_emitX64_movRI(EAX, 42),
        "mov       eax, 0x2a\n");
    TEST_EMIT(vp_emitX64_movRM(RCX, X64MEM(RN_SP, NOREG, 1, 8, 8)),
        "mov       rcx, qword ptr [rsp+8]\n");
    TEST_EMIT(vp_emitX64_movMR(X64MEM(RN_DX, RN_SI, 1, 8, 8), R8),
        "mov       qword ptr [rdx+rsi+8], r8\n");
    TEST_EMIT(vp_emitX64_movMI(X64MEM(RN_DX, RN_SI, 1, 8, 1), 45),
        "mov       byte ptr [rdx+rsi+8], 0x2d\n");
    /*TEST_EMIT(vp_emitX64_movRM(RAX, X64MEM_SEG(RN_SI, NOREG, 1, 0, 8, SEG_GS)),
        "mov       rax, qword ptr gs:[rsi]\n");*/
    TEST_EMIT(vp_emitX64_movRM(RBX, X64MEM(RN_12, RN_13, 4, 64, 8)),
        "mov       rbx, qword ptr [r12+r13*4+64]\n");
    TEST_EMIT(vp_emitX64_movRR(AL, DL),
        "mov       al, dl\n");
    TEST_EMIT(vp_emitX64_movRR(R8, R9),
        "mov       r8, r9\n");
    TEST_EMIT(vp_emitX64_movRM(RAX, X64MEM_RIP(16, 8)),
        "mov       rax, qword ptr [rip+16]\n");
    TEST_EMIT(vp_emitX64_movRR(R8, RAX),
        "mov       r8, rax\n");
    TEST_EMIT(vp_emitX64_movRR(RAX, R9),
        "mov       rax, r9\n");
    TEST_EMIT(vp_emitX64_movRR(R11, R12),
        "mov       r11, r12\n");
    TEST_EMIT(vp_emitX64_movRR(SPL, AL),
        "mov       spl, al\n");
    TEST_EMIT(vp_emitX64_movRR(BPL, DL),
        "mov       bpl, dl\n");
    TEST_EMIT(vp_emitX64_movRI(RAX, INT64_MIN),
        "mov       rax, 0x8000000000000000\n");
    TEST_EMIT(vp_emitX64_movRI(R15, -1),
        "mov       r15, 0xffffffffffffffff\n");
    TEST_EMIT(vp_emitX64_movRI(R8D, 0),
        "mov       r8d, 0x0\n");
    TEST_EMIT(vp_emitX64_movRR(RSP, RBP),
        "mov       rsp, rbp\n");
    TEST_EMIT(vp_emitX64_movRR(R12, R13),
        "mov       r12, r13\n");
    TEST_EMIT(vp_emitX64_movRR(R13, RSP),
        "mov       r13, rsp\n");
    TEST_EMIT(vp_emitX64_movRM(RAX, X64MEM(RN_12, NOREG, 1, 0, 8)),
        "mov       rax, qword ptr [r12]\n");
    TEST_EMIT(vp_emitX64_movRM(RCX, X64MEM(RN_13, NOREG, 1, 0, 8)),
        "mov       rcx, qword ptr [r13]\n");
    /*TEST_EMIT(vp_emitX64_movRM(RDX, X64MEM(NOREG, RN_SI, 4, 0, 8)),
        "mov       rdx, qword ptr [rsi*4]\n");*/
    /* -- MOVSX -- */
    TEST_EMIT(vp_emitX64_movsxRR(R10, SP),
        "movsx     r10, sp\n");
    TEST_EMIT(vp_emitX64_movsxRR(EDI, R15W),
        "movsx     edi, r15w\n");
    TEST_EMIT(vp_emitX64_movsxRR(RCX, R8D),
        "movsx     rcx, r8d\n");
    TEST_EMIT(vp_emitX64_movsxRR(RAX, AL),
        "movsx     rax, al\n");
    TEST_EMIT(vp_emitX64_movsxRR(ESI, SIL),
        "movsx     esi, sil\n");
    TEST_EMIT(vp_emitX64_movsxRR(SI, R13B),
        "movsx     si, r13b\n");
    TEST_EMIT(vp_emitX64_movsxRR(DI, BL),
        "movsx     di, bl\n");
    TEST_EMIT(vp_emitX64_movsxRR(RSP, R15D),
        "movsx     rsp, r15d\n");
    TEST_EMIT(vp_emitX64_movsxRR(R14, R8B),
        "movsx     r14, r8b\n");
    TEST_EMIT(vp_emitX64_movsxRR(R8D, R9W),
        "movsx     r8d, r9w\n");
    /*TEST_EMIT(vp_emitX64_movsxRR(RAX, RSI),
        "movsx     rax, rsi\n");*/
    /* -- MOVZX -- */
    TEST_EMIT(vp_emitX64_movzxRR(ESI, R14W),
        "movzx     esi, r14w\n");
    TEST_EMIT(vp_emitX64_movzxRR(R9D, AX),
        "movzx     r9d, ax\n");
    TEST_EMIT(vp_emitX64_movzxRR(BX, SPL),
        "movzx     bx, spl\n");
    TEST_EMIT(vp_emitX64_movzxRR(EDX, BPL),
        "movzx     edx, bpl\n");
    TEST_EMIT(vp_emitX64_movzxRR(RCX, SIL),
        "movzx     rcx, sil\n");
    TEST_EMIT(vp_emitX64_movzxRR(EBX, R13W),
        "movzx     ebx, r13w\n");
    TEST_EMIT(vp_emitX64_movzxRR(R10, SP),
        "movzx     r10, sp\n");
    TEST_EMIT(vp_emitX64_movzxRR(R8D, AL),
        "movzx     r8d, al\n");
    TEST_EMIT(vp_emitX64_movzxRR(R9, BL),
        "movzx     r9, bl\n");
    TEST_EMIT(vp_emitX64_movzxRR(RAX, AL),
        "movzx     rax, al\n");
    TEST_EMIT(vp_emitX64_movzxRR(R15D, R14B),
        "movzx     r15d, r14b\n");
    TEST_EMIT(vp_emitX64_movzxRR(R11, R12W),
        "movzx     r11, r12w\n");
    TEST_EMIT(vp_emitX64_movzxRR(EAX, CL),
        "movzx     eax, cl\n");
    /* -- MOVSS -- */
    TEST_EMIT(vp_emitX64_movssXM(XMM3, X64MEM(RN_BX, NOREG, 1, 0, 4)),
        "movss     xmm3, dword ptr [rbx]\n");
    TEST_EMIT(vp_emitX64_movssMX(X64MEM(RN_BP, NOREG, 1, -4, 4), XMM4),
        "movss     dword ptr [rbp-4], xmm4\n");
    TEST_EMIT(vp_emitX64_movssXX(XMM0, XMM5),
        "movss     xmm0, xmm5\n");
    TEST_EMIT(vp_emitX64_movssXM(XMM0, X64MEM(RN_SP, NOREG, 1, 0, 4)),
        "movss     xmm0, dword ptr [rsp]\n");
    TEST_EMIT(vp_emitX64_movssXM(XMM8, X64MEM(RN_BP, NOREG, 1, -8, 4)),
        "movss     xmm8, dword ptr [rbp-8]\n");
    TEST_EMIT(vp_emitX64_movssMX(X64MEM(RN_AX, RN_CX, 4, 0, 4), XMM15),
        "movss     dword ptr [rax+rcx*4], xmm15\n");
    TEST_EMIT(vp_emitX64_movssXX(XMM8, XMM15),
        "movss     xmm8, xmm15\n");
    /* -- MULSS -- */
    TEST_EMIT(vp_emitX64_mulssXX(XMM1, XMM2),
        "mulss     xmm1, xmm2\n");
    TEST_EMIT(vp_emitX64_mulssXX(XMM8, XMM9),
        "mulss     xmm8, xmm9\n");
    /* -- DIVSS -- */
    TEST_EMIT(vp_emitX64_divssXX(XMM6, XMM7),
        "divss     xmm6, xmm7\n");
    TEST_EMIT(vp_emitX64_divssXX(XMM0, XMM0),
        "divss     xmm0, xmm0\n");
    /* -- COMISS -- */
    TEST_EMIT(vp_emitX64_comissXX(XMM2, XMM3),
        "comiss    xmm2, xmm3\n");
    TEST_EMIT(vp_emitX64_comissXX(XMM8, XMM0),
        "comiss    xmm8, xmm0\n");
    /* -- UCOMISS -- */
    TEST_EMIT(vp_emitX64_ucomissXX(XMM4, XMM5),
        "ucomiss   xmm4, xmm5\n");
    TEST_EMIT(vp_emitX64_ucomissXX(XMM15, XMM8),
        "ucomiss   xmm15, xmm8\n");
    /* -- XORPS -- */
    TEST_EMIT(vp_emitX64_xorpsXX(XMM10, XMM11),
        "xorps     xmm10, xmm11\n");
    TEST_EMIT(vp_emitX64_xorpsXX(XMM0, XMM0),
        "xorps     xmm0, xmm0\n");
    /* -- MOVSD -- */
    TEST_EMIT(vp_emitX64_movsdXX(XMM0, XMM11),
        "movsd     xmm0, xmm11\n");
    TEST_EMIT(vp_emitX64_movsdXM(XMM0, X64MEM(RN_AX, NOREG, 1, 0, 8)),
        "movsd     xmm0, qword ptr [rax]\n");
    TEST_EMIT(vp_emitX64_movsdMX(X64MEM(RN_SP, NOREG, 1, 16, 8), XMM1),
        "movsd     qword ptr [rsp+16], xmm1\n");
    TEST_EMIT(vp_emitX64_movsdXM(XMM8, X64MEM(RN_SP, NOREG, 1, 0, 8)),
        "movsd     xmm8, qword ptr [rsp]\n");
    TEST_EMIT(vp_emitX64_movsdMX(X64MEM(RN_BP, NOREG, 1, -8, 8), XMM0),
        "movsd     qword ptr [rbp-8], xmm0\n");
    TEST_EMIT(vp_emitX64_movsdXX(XMM15, XMM0),
        "movsd     xmm15, xmm0\n");
    /* -- ADDSS -- */
    TEST_EMIT(vp_emitX64_addssXX(XMM0, XMM1),
        "addss     xmm0, xmm1\n");
    TEST_EMIT(vp_emitX64_addssXX(XMM15, XMM0),
        "addss     xmm15, xmm0\n");
    /* -- ADDSD -- */
    TEST_EMIT(vp_emitX64_addsdXX(XMM8, XMM15),
        "addsd     xmm8, xmm15\n");
    /* -- SUBSS -- */
    TEST_EMIT(vp_emitX64_subssXX(XMM0, XMM15),
        "subss     xmm0, xmm15\n");
    /* -- SUBSD -- */
    TEST_EMIT(vp_emitX64_subsdXX(XMM14, XMM9),
        "subsd     xmm14, xmm9\n");
    TEST_EMIT(vp_emitX64_subsdXX(XMM0, XMM8),
        "subsd     xmm0, xmm8\n");
    /* -- MULSD -- */
    TEST_EMIT(vp_emitX64_mulsdXX(XMM2, XMM3),
        "mulsd     xmm2, xmm3\n");
    TEST_EMIT(vp_emitX64_mulsdXX(XMM15, XMM15),
        "mulsd     xmm15, xmm15\n");
    /* -- DIVSD -- */
    TEST_EMIT(vp_emitX64_divsdXX(XMM4, XMM5),
        "divsd     xmm4, xmm5\n");
    TEST_EMIT(vp_emitX64_divsdXX(XMM1, XMM0),
        "divsd     xmm1, xmm0\n");
    /* -- COMISD -- */
    TEST_EMIT(vp_emitX64_comisdXX(XMM6, XMM7),
        "comisd    xmm6, xmm7\n");
    TEST_EMIT(vp_emitX64_comisdXX(XMM15, XMM8),
        "comisd    xmm15, xmm8\n");
    /* -- UCOMISD -- */
    TEST_EMIT(vp_emitX64_ucomisdXX(XMM0, XMM1),
        "ucomisd   xmm0, xmm1\n");
    TEST_EMIT(vp_emitX64_ucomisdXX(XMM8, XMM15),
        "ucomisd   xmm8, xmm15\n");
    /* -- XORPD -- */
    TEST_EMIT(vp_emitX64_xorpdXX(XMM8, XMM9),
        "xorpd     xmm8, xmm9\n");
    TEST_EMIT(vp_emitX64_xorpdXX(XMM0, XMM0),
        "xorpd     xmm0, xmm0\n");
    /* -- CVT* -- */
    TEST_EMIT(vp_emitX64_cvtsd2ssXX(XMM0, XMM1),
        "cvtsd2ss  xmm0, xmm1\n");
    TEST_EMIT(vp_emitX64_cvttss2siRX(RAX, XMM6),
        "cvttss2si rax, xmm6\n");
    TEST_EMIT(vp_emitX64_cvtsi2sdXR(XMM9, EAX),
        "cvtsi2sd  xmm9, eax\n");
    TEST_EMIT(vp_emitX64_cvtsi2ssXR(XMM12, RAX),
        "cvtsi2ss  xmm12, rax\n");
    TEST_EMIT(vp_emitX64_cvtss2sdXX(XMM15, XMM7),
        "cvtss2sd  xmm15, xmm7\n");
    TEST_EMIT(vp_emitX64_cvttss2siRX(EAX, XMM0),
        "cvttss2si eax, xmm0\n");
    TEST_EMIT(vp_emitX64_cvttss2siRX(R8, XMM15),
        "cvttss2si r8, xmm15\n");
    TEST_EMIT(vp_emitX64_cvttsd2siRX(EAX, XMM0),
        "cvttsd2si eax, xmm0\n");
    TEST_EMIT(vp_emitX64_cvttsd2siRX(R9, XMM8),
        "cvttsd2si r9, xmm8\n");
    TEST_EMIT(vp_emitX64_cvtsi2ssXR(XMM0, EAX),
        "cvtsi2ss  xmm0, eax\n");
    TEST_EMIT(vp_emitX64_cvtsi2ssXR(XMM8, R15),
        "cvtsi2ss  xmm8, r15\n");
    TEST_EMIT(vp_emitX64_cvtsi2sdXR(XMM0, RAX),
        "cvtsi2sd  xmm0, rax\n");
    TEST_EMIT(vp_emitX64_cvtsi2sdXR(XMM15, R8D),
        "cvtsi2sd  xmm15, r8d\n");
    TEST_EMIT(vp_emitX64_cvtsd2ssXX(XMM8, XMM15),
        "cvtsd2ss  xmm8, xmm15\n");
    TEST_EMIT(vp_emitX64_cvtss2sdXX(XMM0, XMM0),
        "cvtss2sd  xmm0, xmm0\n");
    /* -- CWDE/CDQ/CQO -- */
    TEST_EMIT(vp_emitX64_cwde(),
        "cwde\n");
    TEST_EMIT(vp_emitX64_cdq(),
        "cdq\n");
    TEST_EMIT(vp_emitX64_cqo(),
        "cqo\n");
    /* -- CALL -- */
    TEST_EMIT(vp_emitX64_callREL32(5),
        "call      .+5\n");
    TEST_EMIT(vp_emitX64_callR(RAX),
        "call      rax\n");
    TEST_EMIT(vp_emitX64_callR(R11),
        "call      r11\n");
    TEST_EMIT(vp_emitX64_callREL32(0),
        "call      .+0\n");
    TEST_EMIT(vp_emitX64_callREL32(-1),
        "call      .-1\n");
    TEST_EMIT(vp_emitX64_callRIP(0),
        "call      qword ptr [rip]\n");
    /* -- JMP/Jcc -- */
    TEST_EMIT(vp_emitX64_jmpM(X64MEM(RN_BX, NOREG, 1, 0, 8)),
        "jmp       qword ptr [rbx]\n");
    TEST_EMIT(vp_emitX64_jmpREL32(12),
        "jmp       .+12\n");
    TEST_EMIT(vp_emitX64_jmpM(X64MEM(RN_AX, RN_BX, 8, 0, 8)),
        "jmp       qword ptr [rax+rbx*8]\n");
    TEST_EMIT(vp_emitX64_jmpREL32(0),
        "jmp       .+0\n");
    TEST_EMIT(vp_emitX64_jmpREL32(-1),
        "jmp       .-1\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_E, -4),
        "je        .-4\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_NE, 20),
        "jne       .+20\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_B, -100),
        "jb        .-100\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_AE, 0),
        "jae       .+0\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_LE, INT32_MIN),
        "jle       .-2147483648\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_G, INT32_MAX),
        "jg        .+2147483647\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_L, -1),
        "jl        .-1\n");
    TEST_EMIT(vp_emitX64_jccREL32(CC_GE, 1),
        "jge       .+1\n");
    /* -- SETcc -- */
    TEST_EMIT(vp_emitX64_setcc(CC_L, AL),
        "setl      al\n");
    TEST_EMIT(vp_emitX64_setcc(CC_GE, CL),
        "setge     cl\n");
    TEST_EMIT(vp_emitX64_setcc(CC_NE, DL),
        "setne     dl\n");
    TEST_EMIT(vp_emitX64_setcc(CC_O, AL),
        "seto      al\n");
    TEST_EMIT(vp_emitX64_setcc(CC_B, R8B),
        "setb      r8b\n");
    TEST_EMIT(vp_emitX64_setcc(CC_E, R12B),
        "sete      r12b\n");
    TEST_EMIT(vp_emitX64_setcc(CC_A, DL),
        "seta      dl\n");
    TEST_EMIT(vp_emitX64_setcc(CC_G, BPL),
        "setg      bpl\n");
    TEST_EMIT(vp_emitX64_setcc(CC_LE, SIL),
        "setle     sil\n");
    /* -- ... -- */
    TEST_EMIT(vp_emitX64_ret(),
        "ret\n");
    TEST_EMIT(vp_emitX64_cpuid(),
        "cpuid\n");
    TEST_EMIT(vp_emitX64_rdtsc(),
        "rdtsc\n");
    TEST_EMIT(vp_emitX64_syscall(),
        "syscall\n");

    vp_state_close(state);

    if(failures)
    {
        SBuf out;
        vp_buf_init(&out);
        vp_buf_fmt(&out, "%d emit/decode tests failed\n", failures);
        fwrite(out.b, 1, sbuf_len(&out), stdout);
        return 1;
    }

    SBuf out;
    vp_buf_init(&out);
    vp_buf_fmt(&out, "%u emit/decode tests passed\n", tests_run);
    fwrite(out.b, 1, sbuf_len(&out), stdout);
    return 0;
}
