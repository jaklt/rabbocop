#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "clib.h"

/* Mozna bude potreba neco jako lock */

static unsigned int HASH_SIZE = 100000; // 13107200;

struct record {
    uint64_t hash;
    void *best;
    int depth:6;
    unsigned int player:1;
    unsigned int used:1;
};

static struct record *table;

static int reset_count=0, get_count=0, find_count=0, add_count=0, overwritten_count=0;

void info_hash()
{
    printf("info hashtable - reset: %d, get: %d, find: %d, add: %d, ow: %d\n",
            reset_count, get_count, find_count, add_count, overwritten_count);
}

/** size in kilobytes, 0 means only reset (no resize) */
void reset_hash(int size)
{
    reset_count++;
    get_count = find_count = add_count = overwritten_count = 0;
    if (table) free(table);

    if (size > 0)
        HASH_SIZE = size * 1000 / sizeof(struct record);

    table = (struct record *) calloc(HASH_SIZE, sizeof(struct record));
    if (!table) exit(2);
    if (size > 0)
        printf("log Set transposition table size to %dMB (%u entries)\n",
                size, HASH_SIZE);
}

#define ix ((hash ^ player) % HASH_SIZE)

void *get_hash(uint64_t hash, int player)
{
    get_count++;

    return table[ix].best;
}

int find_hash(uint64_t hash, int depth, int player)
{
    find_count++;

    return table[ix].used
        && table[ix].depth >= depth
        && table[ix].player == player
        && table[ix].hash  == hash;
}

void add_hash(uint64_t hash, int depth, int player, void *best)
{
    add_count++;

    overwritten_count += table[ix].used;
    table[ix] = (struct record) {hash, best, depth, player, 1};
}

int get_hash_size()
{
    return HASH_SIZE;
}

void *get_by_index(int i)
{
    return table[i].best;
}

int empty_by_index(int i)
{
    return table && table[i].best;
}
