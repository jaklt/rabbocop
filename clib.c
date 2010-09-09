#include <stdint.h>
#include "clib.h"

static char bit_count_table[65536];

void init_bit_count()
{
    int i;
    for (i=1; i<65536; i++)
        bit_count_table[i] = bit_count_table[i / 2] + (i & 1u);
}

inline int bit_count(uint64_t u)
{
    uint64_t p1, p2, p3, p4;
    p4 = (u & 0xffff000000000000LLU) >> 48;
    p3 = (u & 0x0000ffff00000000LLU) >> 32;
    p2 = (u & 0x00000000ffff0000LLU) >> 16;
    p1 = (u & 0x000000000000ffffLLU);

    return bit_count_table[p1] + bit_count_table[p2]
         + bit_count_table[p3] + bit_count_table[p4];
}


/**
 * Index of bit.
 *   May be search as Find the log base 2.
 */

#if 1
static /* const */ char bit_index_table1[65536];
static /* const */ char bit_index_table2[65536];
static /* const */ char bit_index_table3[65536];
static /* const */ char bit_index_table4[65536];

/*
   TODO inicializace jde provest staticky
        jen asi 45% zrychleni proti switch verzi, zvazit neco z:
            http://graphics.stanford.edu/~seander/bithacks.html
*/

#define set_bit_index(n,c) \
    bit_index_table##n [1      ] =   0 + c; \
    bit_index_table##n [1 <<  1] =   1 + c; \
    bit_index_table##n [1 <<  2] =   2 + c; \
    bit_index_table##n [1 <<  3] =   3 + c; \
    bit_index_table##n [1 <<  4] =   4 + c; \
    bit_index_table##n [1 <<  5] =   5 + c; \
    bit_index_table##n [1 <<  6] =   6 + c; \
    bit_index_table##n [1 <<  7] =   7 + c; \
    bit_index_table##n [1 <<  8] =   8 + c; \
    bit_index_table##n [1 <<  9] =   9 + c; \
    bit_index_table##n [1 << 10] =  10 + c; \
    bit_index_table##n [1 << 11] =  11 + c; \
    bit_index_table##n [1 << 12] =  12 + c; \
    bit_index_table##n [1 << 13] =  13 + c; \
    bit_index_table##n [1 << 14] =  14 + c; \
    bit_index_table##n [1 << 15] =  15 + c;

void init_bit_index()
{
    set_bit_index(1,  0);
    set_bit_index(2, 16);
    set_bit_index(3, 32);
    set_bit_index(4, 48);
}

int bit_index(uint64_t u)
{
    uint64_t p1, p2, p3, p4;
    p4 = (u & 0xffff000000000000LLU) >> 48;
    p3 = (u & 0x0000ffff00000000LLU) >> 32;
    p2 = (u & 0x00000000ffff0000LLU) >> 16;
    p1 = (u & 0x000000000000ffffLLU);

    return bit_index_table1[p1] + bit_index_table2[p2]
         + bit_index_table3[p3] + bit_index_table4[p4];
}

#else
int bit_index(uint64_t u) // {{{
{
    switch (u) {
        case 0x0000000000000001: return 0;
        case 0x0000000000000002: return 1;
        case 0x0000000000000004: return 2;
        case 0x0000000000000008: return 3;
        case 0x0000000000000010: return 4;
        case 0x0000000000000020: return 5;
        case 0x0000000000000040: return 6;
        case 0x0000000000000080: return 7;
        case 0x0000000000000100: return 8;
        case 0x0000000000000200: return 9;
        case 0x0000000000000400: return 10;
        case 0x0000000000000800: return 11;
        case 0x0000000000001000: return 12;
        case 0x0000000000002000: return 13;
        case 0x0000000000004000: return 14;
        case 0x0000000000008000: return 15;
        case 0x0000000000010000: return 16;
        case 0x0000000000020000: return 17;
        case 0x0000000000040000: return 18;
        case 0x0000000000080000: return 19;
        case 0x0000000000100000: return 20;
        case 0x0000000000200000: return 21;
        case 0x0000000000400000: return 22;
        case 0x0000000000800000: return 23;
        case 0x0000000001000000: return 24;
        case 0x0000000002000000: return 25;
        case 0x0000000004000000: return 26;
        case 0x0000000008000000: return 27;
        case 0x0000000010000000: return 28;
        case 0x0000000020000000: return 29;
        case 0x0000000040000000: return 30;
        case 0x0000000080000000: return 31;
        case 0x0000000100000000: return 32;
        case 0x0000000200000000: return 33;
        case 0x0000000400000000: return 34;
        case 0x0000000800000000: return 35;
        case 0x0000001000000000: return 36;
        case 0x0000002000000000: return 37;
        case 0x0000004000000000: return 38;
        case 0x0000008000000000: return 39;
        case 0x0000010000000000: return 40;
        case 0x0000020000000000: return 41;
        case 0x0000040000000000: return 42;
        case 0x0000080000000000: return 43;
        case 0x0000100000000000: return 44;
        case 0x0000200000000000: return 45;
        case 0x0000400000000000: return 46;
        case 0x0000800000000000: return 47;
        case 0x0001000000000000: return 48;
        case 0x0002000000000000: return 49;
        case 0x0004000000000000: return 50;
        case 0x0008000000000000: return 51;
        case 0x0010000000000000: return 52;
        case 0x0020000000000000: return 53;
        case 0x0040000000000000: return 54;
        case 0x0080000000000000: return 55;
        case 0x0100000000000000: return 56;
        case 0x0200000000000000: return 57;
        case 0x0400000000000000: return 58;
        case 0x0800000000000000: return 59;
        case 0x1000000000000000: return 60;
        case 0x2000000000000000: return 61;
        case 0x4000000000000000: return 62;
        case 0x8000000000000000: return 63;
        default: return -1;
    }
} // }}}
#endif


void __attribute((constructor)) init()
{
    init_bit_index();
    init_bit_count();
}
