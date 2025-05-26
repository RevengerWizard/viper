/*
** vp_strscan.c
** String scanning
**
** Major portions taken verbatim or adapted from LuaJIT by Mike Pall
*/

#include <math.h>

#include "vp_char.h"
#include "vp_lex.h"
#include "vp_strscan.h"

/* Definitions for circular decimal digit buffer (base 100 = 2 digits/byte) */
#define STRSCAN_DIG 1024
#define STRSCAN_MAXDIG 800 /* 772 + extra are sufficient */
#define STRSCAN_DDIG (STRSCAN_DIG / 2)
#define STRSCAN_DMASK (STRSCAN_DDIG - 1)
#define STRSCAN_MAXEXP (1 << 20)

/* Helpers for circular buffer */
#define DNEXT(a) (((a) + 1) & STRSCAN_DMASK)
#define DPREV(a) (((a) - 1) & STRSCAN_DMASK)
#define DLEN(lo, hi) ((int32_t)(((lo) - (hi)) & STRSCAN_DMASK))

#define casecmp(c, k) (((c) | 0x20) == k)

/* Final conversion to double */
static void strscan_double(uint64_t x, LexValue* o, int32_t ex2)
{
    double n;

    /* Avoid double rounding for denormals */
    if(VP_UNLIKELY(ex2 <= -1075 && x != 0))
    {
        int32_t b = (x >> 32) ? 32 + (int32_t)vp_fls((uint32_t)(x >> 32)) : (int32_t)vp_fls((uint32_t)x);
        if((int32_t)b + ex2 <= -1023 && (int32_t)b + ex2 >= -1075)
        {
            uint64_t rb = (uint64_t)1 << (-1075 - ex2);
            if((x & rb) && ((x & (rb + rb + rb - 1))))
                x += rb + rb;
            x = (x & ~(rb + rb - 1));
        }
    }

    /* Convert to double using a signed int64_t conversion, then rescale */
    vp_assertX((int64_t)x >= 0, "bad double conversion");
    n = (double)(int64_t)x;
    if(ex2) n = ldexp(n, ex2);
    o->n = n;
}

/* Parse hexadecimal number */
static StrScanFmt strscan_hex(const uint8_t* p, LexValue* o, StrScanFmt fmt, int32_t ex2, uint32_t dig)
{
    uint64_t x = 0;
    uint32_t i;

    /* Scan hex digits */
    for(i = dig > 16 ? 16 : dig; i; i--, p++)
    {
        uint32_t d = (*p != '.' ? *p : *++p);
        if(d > '9')
            d += 9;
        x = (x << 4) + (d & 15);
    }

    /* Summarize rounding-effect of excess digits */
    for(i = 16; i < dig; i++, p++)
        x |= ((*p != '.' ? *p : *++p) != '0'), ex2 += 4;

    /* Format-specific handling */
    switch(fmt)
    {
    case STRSCAN_INT:
        if(x < 0x80000000u)
        {
            o->u64 = (int32_t)x;
            return STRSCAN_INT;  /* Fast path for 32 bit integers. */
        }
    /* fallthrough */
    case STRSCAN_I64:
    case STRSCAN_U64:
        o->u64 = x;
        return fmt;
    case STRSCAN_U32:
        if(dig > 8) return STRSCAN_ERROR;
        o->u64 = (int32_t)x;
        return STRSCAN_U32;
    default:
        break;
    }

    /* Reduce range, then convert to double */
    if((x & U64x(c0000000, 0000000)))
    {
        x = (x >> 2) | (x & 3);
        ex2 += 2;
    }
    strscan_double(x, o, ex2);
    return fmt;
}

/* Parse octal number */
static StrScanFmt strscan_oct(const uint8_t* p, LexValue* o, StrScanFmt fmt, int32_t ex2, uint32_t dig)
{
    uint64_t x = 0;

    if(dig > 22 || (dig == 22 && *p > '1'))
        return STRSCAN_ERROR;

    /* Scan octal digits */
    while(dig-- > 0)
    {
        if(!(*p >= '0' && *p <= '7')) return STRSCAN_ERROR;
        x = (x << 3) + (*p++ & 7);
    }

    /* Format-specific handling */
    switch(fmt)
    {
    case STRSCAN_INT:
        if(x >= 0x80000000u) fmt = STRSCAN_INT;
        /* fallthrough */
    default:
    case STRSCAN_I64:
    case STRSCAN_U64:
        o->u64 = x;
        break;
    case STRSCAN_U32:
        if ((x >> 32)) return STRSCAN_ERROR;
        o->u64 = (int32_t)x;
        break;
    }
    return fmt;
}

/* Parse binary number */
static StrScanFmt strscan_bin(const uint8_t* p, LexValue* o, StrScanFmt fmt, int32_t ex2, uint32_t dig)
{
    uint64_t x = 0;
    uint32_t i;

    if(ex2 || dig > 64) return STRSCAN_ERROR;

    /* Scan binary digits */
    for(i = dig; i; i--, p++)
    {
        if((*p & ~1) != '0')
            return STRSCAN_ERROR;
        x = (x << 1) | (*p & 1);
    }

    /* Format-specific handling */
    switch(fmt)
    {
    case STRSCAN_INT:
        if(x < 0x80000000u)
        {
            o->u64 = (int32_t)x;
            return STRSCAN_INT;  /* Fast path for 32 bit integers. */
        }
    /* fallthrough */
    case STRSCAN_I64:
    case STRSCAN_U64:
        o->u64 = x;
        return fmt;
    case STRSCAN_U32:
        if(dig > 8) return STRSCAN_ERROR;
        o->u64 = (int32_t)x;
        return STRSCAN_U32;
    default:
        break;
    }
    return fmt;
}

/* Parse decimal number */
static StrScanFmt strscan_dec(const uint8_t* p, LexValue* o, StrScanFmt fmt, int32_t ex10, uint32_t dig)
{
    uint8_t xi[STRSCAN_DDIG], *xip = xi;

    if(dig)
    {
        uint32_t i = dig;
        if(i > STRSCAN_MAXDIG)
        {
            ex10 += (int32_t)(i - STRSCAN_MAXDIG);
            i = STRSCAN_MAXDIG;
        }
        /* Scan unaligned leading digit */
        if(((ex10 ^ i) & 1))
            *xip++ = ((*p != '.' ? *p : *++p) & 15), i--, p++;
        /* Scan aligned double-digits */
        for(; i > 1; i -= 2)
        {
            uint32_t d = 10 * ((*p != '.' ? *p : *++p) & 15);
            p++;
            *xip++ = d + ((*p != '.' ? *p : *++p) & 15);
            p++;
        }
        /* Scan and realign trailing digit */
        if(i) *xip++ = 10 * ((*p != '.' ? *p : *++p) & 15), ex10--, dig++, p++;

        /* Summarize rounding-effect of excess digits */
        if(dig > STRSCAN_MAXDIG)
        {
            do
            {
                if((*p != '.' ? *p : *++p) != '0')
                {
                    xip[-1] |= 1;
                    break;
                }
                p++;
            }
            while(--dig > STRSCAN_MAXDIG);
            dig = STRSCAN_MAXDIG;
        }
        else
        { 
            /* Simplify exponent */
            while(ex10 > 0 && dig <= 18)
                *xip++ = 0, ex10 -= 2, dig += 2;
        }
    }
    else
    { 
        /* Only got zeros */
        ex10 = 0;
        xi[0] = 0;
    }

    /* Fast path for numbers in integer format (but handles e.g. 1e6, too) */
    if(dig <= 20 && ex10 == 0)
    {
        uint8_t* xis;
        uint64_t x = xi[0];
        double n;
        for(xis = xi + 1; xis < xip; xis++)
            x = x * 100 + *xis;
        /* No overflow? */
        if(!(dig == 20 && (xi[0] > 18 || (int64_t)x >= 0)))
        {
            /* Format-specific handling */
            switch(fmt)
            {
            case STRSCAN_INT:
                if(x < 0x80000000u)
                {
                    o->u64 = (int32_t)x;
                    return STRSCAN_INT;  /* Fast path for 32 bit integers */
                }
                /* fallthrough */
            case STRSCAN_I64:
            case STRSCAN_U64:
                o->u64 = x;
                return fmt;
            case STRSCAN_U32:
                if((x >> 32) != 0) return STRSCAN_ERROR;
                o->u64 = (int32_t)x;
                return STRSCAN_U32;
            default:
                /* Fast path for plain numbers < 2^63 */
                if((int64_t)x < 0) break;
                n = (double)(int64_t)x;
                o->n = n;
                return fmt;
            }
        }
    }

    /* Slow non-integer path */
    if(fmt == STRSCAN_INT)
    {
        return STRSCAN_ERROR;
    }
    else if(fmt > STRSCAN_INT)
    {
        return STRSCAN_ERROR;
    }
    {
        uint32_t hi = 0, lo = (uint32_t)(xip - xi);
        int32_t ex2 = 0, idig = (int32_t)lo + (ex10 >> 1);

        vp_assertX(lo > 0 && (ex10 & 1) == 0, "bad lo %d ex10 %d", lo, ex10);

        /* Handle simple overflow/underflow */
        if(idig > 310 / 2)
        {
            o->n = INFINITY;
            return fmt;
        }
        else if(idig < -326 / 2)
        {
            o->n = 0.0;
            return fmt;
        }

        /* Scale up until we have at least 17 or 18 integer part digits */
        while(idig < 9 && idig < DLEN(lo, hi))
        {
            uint32_t i, cy = 0;
            ex2 -= 6;
            for(i = DPREV(lo);; i = DPREV(i))
            {
                uint32_t d = (xi[i] << 6) + cy;
                cy = (((d >> 2) * 5243) >> 17);
                d = d - cy * 100; /* Div/mod 100 */
                xi[i] = (uint8_t)d;
                if(i == hi)
                    break;
                if(d == 0 && i == DPREV(lo))
                    lo = i;
            }
            if(cy)
            {
                hi = DPREV(hi);
                if(xi[DPREV(lo)] == 0)
                    lo = DPREV(lo);
                else if(hi == lo)
                {
                    lo = DPREV(lo);
                    xi[DPREV(lo)] |= xi[lo];
                }
                xi[hi] = (uint8_t)cy;
                idig++;
            }
        }

        /* Scale down until no more than 17 or 18 integer part digits remain */
        while(idig > 9)
        {
            uint32_t i = hi, cy = 0;
            ex2 += 6;
            do
            {
                cy += xi[i];
                xi[i] = (cy >> 6);
                cy = 100 * (cy & 0x3f);
                if(xi[i] == 0 && i == hi)
                    hi = DNEXT(hi), idig--;
                i = DNEXT(i);
            } 
            while(i != lo);

            while(cy)
            {
                if(hi == lo)
                {
                    xi[DPREV(lo)] |= 1;
                    break;
                }
                xi[lo] = (cy >> 6);
                lo = DNEXT(lo);
                cy = 100 * (cy & 0x3f);
            }
        }

        /* Collect integer part digits and convert to rescaled double */
        {
            uint64_t x = xi[hi];
            uint32_t i;
            for(i = DNEXT(hi); --idig > 0 && i != lo; i = DNEXT(i))
                x = x * 100 + xi[i];
            if(i == lo)
            {
                while(--idig >= 0)
                    x = x * 100;
            }
            else
            { 
                /* Gather round bit from remaining digits */
                x <<= 1;
                ex2--;
                do
                {
                    if(xi[i])
                    {
                        x |= 1;
                        break;
                    }
                    i = DNEXT(i);
                } 
                while (i != lo);
            }
            strscan_double(x, o, ex2);
        }
    }
    return fmt;
}

/* Scan string containing a number. Returns format. Returns value in o */
StrScanFmt vp_strscan_scan(const uint8_t* p, size_t len, LexValue* o)
{
    const uint8_t* pe = p + len;

    /* Parse regular number */
    {
        StrScanFmt fmt = STRSCAN_INT;
        int cmask = VP_CHAR_DIGIT;
        int base = 10;
        const uint8_t *sp, *dp = NULL;
        uint32_t dig = 0, hasdig = 0, x = 0;
        int32_t ex = 0;

        /* Determine base and skip leading zeros */
        if(VP_UNLIKELY(*p <= '0'))
        {
            if(*p == '0')
            {
                if(casecmp(p[1], 'x'))
                    base = 16, cmask = VP_CHAR_XDIGIT, p += 2;
                else if(casecmp(p[1], 'o'))
                    base = 8, cmask = VP_CHAR_DIGIT, p += 2;
                else if(casecmp(p[1], 'b'))
                    base = 2, cmask = VP_CHAR_DIGIT, p += 2;
            }
            for(;; p++)
            {
                if(*p == '0')
                {
                    hasdig = 1;
                }
                else if(*p == '.')
                {
                    if(dp) return STRSCAN_ERROR;
                    dp = p;
                }
                else
                {
                    break;
                }
            }
        }

        /* Preliminary digit and decimal point scan */
        for(sp = p;; p++)
        {
            if(VP_LIKELY(vp_char_isa(*p, cmask)))
            {
                x = x * 10 + (*p & 15); /* For fast path below */
                dig++;
            }
            else if(*p == '.')
            {
                if(dp) return STRSCAN_ERROR;
                dp = p;
            }
            else
            {
                break;
            }
        }
        if(!(hasdig | dig)) return STRSCAN_ERROR;

        /* Handle decimal point */
        if(dp)
        {
            if(base == 2 || base == 8) return STRSCAN_ERROR;
            /*if(dp == p-1) return STRSCAN_ERROR;*/
            fmt = STRSCAN_NUM;
            if(dig)
            {
                ex = (int32_t)(dp - (p - 1));
                dp = p - 1;
                while(ex < 0 && *dp-- == '0') ex++, dig--; /* Skip trailing zeros */
                if(ex <= -STRSCAN_MAXEXP) return STRSCAN_ERROR;
                if(base == 16) ex *= 4;
            }
        }

        /* Parse exponent */
        if(base >= 10 && casecmp(*p, (uint32_t)(base == 16 ? 'p' : 'e')))
        {
            uint32_t xx;
            int negx = 0;
            fmt = STRSCAN_NUM; p++;
            if(*p == '+' || *p == '-') negx = (*p++ == '-');
            if(!vp_char_isdigit(*p)) return STRSCAN_ERROR;
            xx = (*p++ & 15);
            while(vp_char_isdigit(*p))
            {
                xx = xx * 10 + (*p & 15);
                if(xx >= STRSCAN_MAXEXP) return STRSCAN_ERROR;
                p++;
            }
            ex += negx ? -(int32_t)xx : (int32_t)xx;
        }

        /* Parse suffix */
        if(*p)
        {
            /* u8, u16, u32, u64 */
            /* i8, i16, i32, i64 */
            /* f */
            while(vp_char_isspace(*p)) p++;
            if(*p) return STRSCAN_ERROR;
        }
        if(p < pe) return STRSCAN_ERROR;

        /* Fast path for decimal 32 bit integers */
        if(fmt == STRSCAN_INT && base == 10 &&
            (dig < 10 || (dig == 10 && *sp <= '2' && x < 0x80000000u))) 
        {
            o->u64 = (int32_t)x;
            return STRSCAN_INT;
        }

        /* Dispatch to base-specific parser */
        if(base == 16)
            fmt = strscan_hex(sp, o, fmt, ex, dig);
        else if(base == 8)
            fmt = strscan_oct(sp, o, fmt, ex, dig);
        else if(base == 2)
            fmt = strscan_bin(sp, o, fmt, ex, dig);
        else
            fmt = strscan_dec(sp, o, fmt, ex, dig);

        return fmt;
    }
}