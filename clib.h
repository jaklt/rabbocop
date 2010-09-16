#ifndef H_CLIB
#define H_CLIB
#include <stdint.h>

#define GOLD   0
#define SILVER 1

#define RABBIT   0
#define CAT      1
#define DOG      2
#define HORSE    3
#define CAMEL    4
#define ELEPHANT 5

#define RIGHT_SIDE  0x0101010101010101ULL
#define LEFT_SIDE   0x8080808080808080ULL
#define UPPER_SIDE  0xff00000000000000ULL
#define BOTTOM_SIDE 0x00000000000000ffULL

int bit_index(uint64_t u);
int bit_count(uint64_t u);
uint64_t hash_piece(int player, int piece, int position);
int64_t steps_from_position(int pl, int pie, int pos);
void init();

void info_hash();
void reset_hash();
int find_hash(uint64_t hash, int depth);
void *get_hash(uint64_t hash);
void add_hash(uint64_t hash, int depth, void *best);
void clean();

#endif
