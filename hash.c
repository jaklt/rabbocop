#include <string.h>
#include "clib.h"

/* Mozna bude potreba neco jako lock */

#define HASH_SIZE 65000

struct record {
    int hash;
    int value;
    int depth:7;
    unsigned int used:1;
};

static struct record table[HASH_SIZE];

void reset_hash()
{
    memset(table, 0, sizeof(struct record)*HASH_SIZE);
}

int get_hash(uint64_t hash)
{
    return table[hash % HASH_SIZE].value;
}

int find_hash(uint64_t hash, int depth)
{
    return table[hash % HASH_SIZE].used
        && table[hash % HASH_SIZE].depth <= depth;
}

void add_hash(uint64_t hash, int depth, int value)
{
    table[hash % HASH_SIZE] = (struct record) {
        hash, value, depth, 1
    };
}
