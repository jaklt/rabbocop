#include <stdio.h>
#include <stdlib.h>
#include "clib.h"

void bits(int64_t bs)
{
    int64_t b;

    printf("[");
    while (bs) {
        b = bs & (-bs);
        printf("%d, ", bit_index(b));
        bs ^= b;
    }
    printf("]\n");
}

void bit_index_speed_test()
{
    int i;
    for (i=0; i<194967296; i++) bit_index(i);
}

void bit_index_test()
{
    printf("bit_index_test started\n");
    int i;
    for (i=0; i<64; i++)
        if (!(bit_index(1LLU << i) == i))
            printf(" -> false (%d)\n", i);
}

int main(int argc, char *argv[])
{
    bit_index_test();
    return 0;
}

