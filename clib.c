#include <stdint.h>
#include "clib.h"

long int random(void);


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

static char bit_index_table1[65536];
static char bit_index_table2[65536];
static char bit_index_table3[65536];
static char bit_index_table4[65536];

/*
   TODO jen asi 45% zrychleni proti switch verzi, zvazit neco z:
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


static uint64_t bit_zobrist_table[2][6][64];

void init_zobrist()
{
    int i, j, k;
    for (i=0; i<2; i++)
        for (j=0; j<6; j++)
            for (k=0; k<64; k++)
                bit_zobrist_table[i][j][k] = (((uint64_t) random()) << 40)
                        ^ (((uint64_t) random()) << 20) ^ ((uint64_t) random());
}

uint64_t hash_piece(int player, int piece, int position)
{
    return bit_zobrist_table[player][piece][position];
}


static int64_t bit_steps_table[2][6][64];

void init_steps_table()
{
    int i, j, k;
    uint64_t x;

    for (i=0; i<2; i++)
        for (j=0; j<6; j++)
            for (k=0; k<64; k++) {
                x = 1LLU << k;

                bit_steps_table[i][j][k] |=
                    (1LLU << (k + 8)) * ((UPPER_SIDE  & x) == 0) |
                    (1LLU << (k - 8)) * ((BOTTOM_SIDE & x) == 0) |
                    (1LLU << (k + 1)) * ((LEFT_SIDE   & x) == 0) |
                    (1LLU << (k - 1)) * ((RIGHT_SIDE  & x) == 0);
            }

    for (i=0; i<2; i++)
        for (k=0; k<64; k++) {
            x = 1LLU << k;

            bit_steps_table[i][RABBIT][k] ^=
                (1LLU << (k + 8)) * ((UPPER_SIDE  & x) == 0) * (i == SILVER) |
                (1LLU << (k - 8)) * ((BOTTOM_SIDE & x) == 0) * (i == GOLD);
        }
}

int64_t steps_from_position(int pl, int pie, uint64_t pos)
{
    return bit_steps_table[pl][pie][bit_index(pos)];
}

int immobilised(uint64_t pl_pieces, uint64_t op_pieces, uint64_t tested)
{
    uint64_t adjecent_pos = steps_from_position(GOLD, ELEPHANT, tested);
    return ((pl_pieces & adjecent_pos) == 0) && (op_pieces & adjecent_pos);
}


/*
 * In order not to repeat positions, this is forbidden board setup.
 */
uint64_t forb_b[2][6] = {
    {0, 0, 0, 0, 0, 0}, /* Golds   position */
    {0, 0, 0, 0, 0, 0}  /* Silvers position */
};

void forbid_board(BOARD_AS_PARAMETER)
{
    forb_b[0][0] = gr; forb_b[0][1] = gc; forb_b[0][2] = gd;
    forb_b[0][3] = gh; forb_b[0][4] = gm; forb_b[0][5] = ge;

    forb_b[1][0] = sr; forb_b[1][1] = sc; forb_b[1][2] = sd;
    forb_b[1][3] = sh; forb_b[1][4] = sm; forb_b[1][5] = se;
}

int is_forbidden(BOARD_AS_PARAMETER)
{
    /* prevent repetition */
    return ( forb_b[0][0] == gr && forb_b[0][1] == gc
          && forb_b[0][2] == gd && forb_b[0][3] == gh
          && forb_b[0][4] == gm && forb_b[0][5] == ge

          && forb_b[1][0] == sr && forb_b[1][1] == sc
          && forb_b[1][2] == sd && forb_b[1][3] == sh
          && forb_b[1][4] == sm && forb_b[1][5] == se);
}


extern void init_eval();

void __attribute((constructor)) init()
{
    init_bit_index();
    init_bit_count();
    init_steps_table();
    init_zobrist();
    init_eval();
}
