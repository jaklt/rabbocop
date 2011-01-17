#include <stdio.h>
#include <stdlib.h>
#include "clib.h"

void bits(int64_t bs)
{
    int64_t b;

    putchar('[');
    while (bs) {
        b = bs & (-bs);
        printf("%d, ", bit_index(b));
        bs ^= b;
    }
    puts("]");
}

void bit_index_speed_test()
{
    int i;
    for (i=0; i<194967296; i++) bit_index(i);
}

void bit_index_test()
{
    puts("bit_index_test started");
    int i;
    for (i=0; i<64; i++)
        if (!(bit_index(1LLU << i) == i))
            printf(" -> false (%d)\n", i);
}

void eval_speed_test()
{
    puts("eval_speed_test started");
    int i;
    uint64_t b = 17241230320233502635LLU;

    /* try eval some pseudogames */
    for (i=0; i<10000000; i++) {
        eval(((uint64_t)1+4+16+64) << (i % 64),
             ((uint64_t)2+32)      << (i % 64),
             ((uint64_t)128+512)   << (i % 64),
             ((uint64_t)256+1024)  << (i % 64),
             ((uint64_t)2048)      << (i % 64),
             ((uint64_t)4096)      << (i % 64),
             ((uint64_t)1+4+16+64) << ((~i) % 64),
             ((uint64_t)2+32)      << ((~i) % 64),
             ((uint64_t)128+512)   << ((~i) % 64),
             ((uint64_t)256+1024)  << ((~i) % 64),
             ((uint64_t)2048)      << ((~i) % 64),
             ((uint64_t)4096)      << ((~i) % 64));
    }
}

int main(int argc, char *argv[])
{
    /* bit_index_test(); */
    eval_speed_test();
    return 0;
}

