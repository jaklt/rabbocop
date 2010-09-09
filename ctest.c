#include <stdio.h>
#include <stdlib.h>
#include "clib.h"


int main(int argc, char *argv[])
{
    int i;
    for (i=0; i<194967296; i++)
        bit_index(i);

    /* printf("%i\n", bit_index(16)); */
	return 0;
}

