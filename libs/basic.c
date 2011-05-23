#include <time.h>
#include "clib.h"
extern void srandom(unsigned int);


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


extern void init_bit_index();
extern void init_bit_count();
extern void init_steps_table();
extern void init_zobrist();
extern void init_eval();

void __attribute((constructor)) init()
{
    srandom((unsigned int) time(NULL));
    init_bit_index();
    init_bit_count();
    init_steps_table();
    init_zobrist();
    init_eval();
}
