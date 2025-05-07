/*
** vp_def.h
** Viper common internal definitions
*/

#ifndef _VP_DEF_H
#define _VP_DEF_H

#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>
#include <limits.h>

#define VP_MAX_MEM32   0x7fffff00  /* Max. 32 bit memory allocation */
#define VP_MAX_MEM64 ((uint64_t)1 << 47) /* Max. 64 bit memory allocation */

#define VP_MAX_LINE VP_MAX_MEM32

#define VP_BUFFER_SIZE 1024

#define VP_MIN_SBUF 32

#define MIN(x, y) ((x) <= (y) ? (x) : (y))
#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define CLAMP_MAX(x, max) MIN(x, max)
#define CLAMP_MIN(x, min) MAX(x, min)
#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a) - 1, (a))
#define ALIGN_DOWN_PTR(p, a) ((void*)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void*)ALIGN_UP((uintptr_t)(p), (a)))

#define U64x(hi, lo)    (((uint64_t)0x##hi << 32) + (uint64_t)0x##lo)

#if defined(__GNUC__) || defined(__clang) || defined(__clang__)

#define VP_NORET __attribute__((noreturn))
#define VP_INLINE inline
#define VP_AINLINE inline __attribute__((always_inline))
#define VP_NOINLINE __attribute__((noinline))

#define VP_LIKELY(x)   __builtin_expect(!!(x), 1)
#define VP_UNLIKELY(x) __builtin_expect(!!(x), 0)

#define vp_ffs(x)  ((uint32_t)__builtin_ctz(x))

#if defined(__INTEL_COMPILER) && (defined(__i386__) || defined(__x86_64__))
static VP_AINLINE uint32_t vp_fls(uint32_t x)
{
    uint32_t r; __asm__("bsrl %1, %0" : "=r" (r) : "rm" (x) : "cc"); return r;
}
#else
#define vp_fls(x) ((uint32_t)(__builtin_clz(x)^31))
#endif

#else
#error "missing defines for your compiler"
#endif

#ifndef VP_NORET
#define VP_NORET
#endif

#ifndef VP_LIKELY
#define VP_LIKELY(x) (x)
#define VP_UNLIKELY(x) (x)
#endif

#if defined(VIPER_USE_ASSERT)
void vp_assert_fail(const char* file, int line, const char* func, const char* fmt, ...);
#endif

/* Internal assertions */
#if defined(VIPER_USE_ASSERT)
#define vp_assert_check(c, ...) \
    ((c) ? (void)0 : \
    (vp_assert_fail(__FILE__, __LINE__, __func__, __VA_ARGS__), 0))
#endif

#ifdef VIPER_USE_ASSERT
#define vp_assertX(c, ...) vp_assert_check((c), __VA_ARGS__)
#else
#define vp_assertX(c, ...) ((void)0)
#endif

/* Static assertions */
#define VP_ASSERT_NAME2(name, line) name ## line
#define VP_ASSERT_NAME(line) VP_ASSERT_NAME2(vp_assert_, line)
#ifdef __COUNTER__
#define VP_STATIC_ASSERT(cond) \
    extern void VP_ASSERT_NAME(__COUNTER__)(int STATIC_ASSERTION_FAILED[(cond) ? 1 : -1])
#else
#define VP_STATIC_ASSERT(cond) \
    extern void VP_ASSERT_NAME(__LINE__)(int STATIC_ASSERTION_FAILED[(cond) ? 1 : -1])
#endif

#endif