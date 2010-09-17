#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "clib.h"

/* Mozna bude potreba neco jako lock */

static unsigned int HASH_SIZE = 13107200;

struct record {
    uint64_t hash;
    void *best;
    int depth:7;
    unsigned int used:1;
};

static struct record *table;

static int reset_count=0, get_count=0, find_count=0, add_count=0, overwritten_count=0;

void info_hash()
{
    printf("info hashtable - reset: %d, get: %d, find: %d, add: %d, ow: %d\n",
            reset_count, get_count, find_count, add_count, overwritten_count);
}

void reset_hash(int size)
{
    reset_count++;
    get_count = find_count = add_count = overwritten_count = 0;
    if (table) free(table);

    if (size > 0)
        HASH_SIZE = size * 1000000 / sizeof(struct record);

    table = (struct record *) calloc(HASH_SIZE, sizeof(struct record));
    if (!table) exit(2);
}

void *get_hash(uint64_t hash)
{
    get_count++;
    return table[hash % HASH_SIZE].best;
}

int find_hash(uint64_t hash, int depth)
{
    find_count++;
    return table[hash % HASH_SIZE].used
        && table[hash % HASH_SIZE].depth >= depth
        && table[hash % HASH_SIZE].hash == hash;
}

void add_hash(uint64_t hash, int depth, void *best)
{
    add_count++;
    overwritten_count += table[hash % HASH_SIZE].used;
    table[hash % HASH_SIZE] = (struct record) {
        hash, best, depth, 1
    };
}
