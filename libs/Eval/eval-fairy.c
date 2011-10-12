/**
 * This evaluation function comes from bot_Fairy by Ola Hansson as can be
 * found in arimaa.com but with a few modifications.
 */
#include <stdint.h>
#include "../clib.h"


#define INFINITY 100000

#define EMPTY_SQUARE 0x0U
#define EMPTY 0x0U
#define OFF_BOARD_SQUARE 0x9FU
#define OFF_BOARD 0x18U
#define GOLD 0x10U
#define SILVER 0x8U
#define OFF_BOARD_PIECE 0x7U
#define ELEPHANT_PIECE 0x6U
#define CAMEL_PIECE 0x5U
#define HORSE_PIECE 0x4U
#define DOG_PIECE 0x3U
#define CAT_PIECE 0x2U
#define RABBIT_PIECE 0x1U
#define EMPTY_PIECE 0x0U
#define PIECE_MASK 0x7U
#define OWNER_MASK 0x18U
#define FLIP_SIDE GOLD^SILVER
#define TRUE 1
#define FALSE 0
#define NORTH -10
#define SOUTH 10
#define EAST 1
#define WEST -1
#define OWNER(square) (bp->board[square] & OWNER_MASK) // who owns a (piece on a) square?
#define PIECE(square) (bp->board[square] & PIECE_MASK) // what piece is on a square?
#define BOARD(square) (bp->board[square]) // What is on a square?  Returns the owner | piece combination.
#define ROW(square) (9-square/10) // what row is a square in?  1 = bottom, 8 = top
#define COL(square) (square%10) // what column is a square in?  1 = left (a), 8 = right (h)
const int direction[4]={NORTH,EAST,SOUTH,WEST};
const int trap[4]={33,36,63,66};

typedef struct
{
    unsigned char board[100];
        /*****
        11 - 88 = actual board, edges around for easier move generation and evaluation

        11 12 ... 17 18    a8 b8 ... g8 h8
        21 22 ... 27 28    a7 b7 ... g7 h7
        ............... == ...............
        71 72 ... 77 78    a2 b2 ... g2 h2
        81 82 ... 87 88    a1 b1 ... g1 h1

        directions:
            -10 : North (up, towards silver)
            +10 : South (down, towards gold)
            +1 : East (right from gold's view, left from silver's view)
            -1 : West (left from gold's view, right from silver's view)

        highest bit - is square off the board?
            (board[x]&0x80U)==0 : square is on board.
            (board[x]&0x80U)==0x80U : square is off the board.
        second, third bit - currently unused.
        fourth, fifth bit - who owns the piece?
            (board[x] & OWNER_MASK)==OFF_BOARD : square is off the board - both bits are set
            (board[x] & OWNER_MASK)==GOLD : gold piece - first bit is set
            (board[x] & OWNER_MASK)==SILVER : silver piece - second bit is set
            (board[x] & OWNER_MASK)==EMPTY : empty square - none of the bits are set
        remaining three bits - which kind of piece is it?
            (board[x]&0x7U) gives which piece it is.
                (board[x] & PIECE_MASK)==6 : Elephant
                (board[x] & PIECE_MASK)==5 : Camel
                (board[x] & PIECE_MASK)==4 : Horse
                (board[x] & PIECE_MASK)==3 : Dog
                (board[x] & PIECE_MASK)==2 : Cat
                (board[x] & PIECE_MASK)==1 : Rabbit
            Special cases:
                (board[x] & PIECE_MASK)==0 : Empty
                (board[x] & PIECE_MASK)==7 : Off the board
        *****/
} board_t;

#define ELEPHANT_VALUE 20000
#define CAMEL_VALUE 5000
#define HORSE_VALUE 3000
#define DOG_VALUE 1800
#define CAT_VALUE 1500
#define RABBIT_VALUE 1000

#define RABBIT_FREE_AHEAD 1000
#define RABBIT_FRIENDLY_AHEAD 500
#define RABBIT_FREE_SIDE 300
#define RABBIT_FRIENDLY_SIDE 200

// board constants
static const int trap_square[4]={33,36,63,66};
//static const int direction[4]={NORTH,EAST,SOUTH,WEST};

static const int adjacent_trap[100]={0,0,0,0,0,0,0,0,0,0,0,

                                      0, 0, 0, 0, 0, 0, 0, 0,     0,0,
                                      0, 0,33, 0, 0,36, 0, 0,     0,0,
                                      0,33, 0,33,36, 0,36, 0,     0,0,
                                      0, 0,33, 0, 0,36, 0, 0,     0,0,
                                      0, 0,63, 0, 0,66, 0, 0,     0,0,
                                      0,63, 0,63,66, 0,66, 0,     0,0,
                                      0, 0,63, 0, 0,66, 0, 0,     0,0,
                                      0, 0, 0, 0, 0, 0, 0, 0,     0,0,

                                     0,0,0,0,0,0,0,0,0};

static const int adjacent2_trap[100]={0,0,0,0,0,0,0,0,0,0,0,

                                       0, 0,33, 0, 0,36, 0, 0,     0,0,
                                       0,33, 0,33,36, 0,36, 0,     0,0,
                                      33, 0, 0,36,33, 0, 0,36,     0,0,
                                       0,33,63,33,36,66,36, 0,     0,0,
                                       0,63,33,63,66,36,66, 0,     0,0,
                                      63, 0, 0,66,63, 0, 0,66,     0,0,
                                       0,63, 0,63,66, 0,66, 0,     0,0,
                                       0, 0,63, 0, 0,66, 0, 0,     0,0,

                                      0,0,0,0,0,0,0,0,0};

int EVAL_Eval(board_t *bp)

// Evaluation is done from gold's perspective.  At the end of the evaluation, it's adjusted to be seen from current player's perspective.

{
    // evaluation constants
    static const int piece_value[7]={0,RABBIT_VALUE,CAT_VALUE,DOG_VALUE,HORSE_VALUE,CAMEL_VALUE,ELEPHANT_VALUE};
    // variables

    // utility variables
    int side_mask[OWNER_MASK];

    // loop variables
    int square; 
    int side;
    int trap;
    int dir;
    int i;

    // value variables
    int value=0;
    int material_value[2]={0,0};
    int trap_value[2]={0,0};
    int rabbit_value[2]={0,0};

    // how many of the pieces do the players have, and where are they?
    int elephants[2]={0,0};
    int elephant_pos[2][1];
    int camels[2]={0,0};
    int camel_pos[2][1];
    int horses[2]={0,0};
    int horse_pos[2][2];
    int dogs[2]={0,0};
    int dog_pos[2][2];
    int cats[2]={0,0};
    int cat_pos[2][2];
    int rabbits[2]={0,0};
    int rabbit_pos[2][8];

    // trap evaluation variables
    int trap_adjacent[2];
    int trap_adjacent_strength[2];
    int trap_adjacent_strongest[2];

    // material evaluation variables
    int material[100]; // What is the piece on this square worth?
    int piece_frozen;
    int piece_adjacent_stronger_enemy;
    int piece_adjacent_empty;
    int piece_adjacent_strongest[2];
    int piece_adjacent[2];
    int piece_adjacent_trap;

    // rabbit evaluation variables
    int row;

    // Initialize some evaluation stuff

    side_mask[GOLD]=0;
    side_mask[SILVER]=1;

    // Determine extra information about the board state

    for (square=11; square<=88; square++) // loop over board, initialize board state info and find where all the pieces are.
    {
        if (square%10==9) square+=2;
        switch (PIECE(square))
        {
            case ELEPHANT_PIECE :
                elephant_pos[side_mask[OWNER(square)]][elephants[side_mask[OWNER(square)]]]=square;
                elephants[side_mask[OWNER(square)]]++;
                break;
            case CAMEL_PIECE :
                camel_pos[side_mask[OWNER(square)]][camels[side_mask[OWNER(square)]]]=square;
                camels[side_mask[OWNER(square)]]++;
                break;
            case HORSE_PIECE :
                horse_pos[side_mask[OWNER(square)]][horses[side_mask[OWNER(square)]]]=square;
                horses[side_mask[OWNER(square)]]++;
                break;
            case DOG_PIECE :
                dog_pos[side_mask[OWNER(square)]][dogs[side_mask[OWNER(square)]]]=square;
                dogs[side_mask[OWNER(square)]]++;
                break;
            case CAT_PIECE :
                cat_pos[side_mask[OWNER(square)]][cats[side_mask[OWNER(square)]]]=square;
                cats[side_mask[OWNER(square)]]++;
                break;
            case RABBIT_PIECE :
                rabbit_pos[side_mask[OWNER(square)]][rabbits[side_mask[OWNER(square)]]]=square;
                rabbits[side_mask[OWNER(square)]]++;
                break;
        }
        if (OWNER(square)==GOLD || OWNER(square)==SILVER) {
            material[square]=piece_value[PIECE(square)];
        } else {
            material[square]=0;
        }
    }

    // Evaluate trap squares, decide trap ownership.

    for (trap=0; trap<4; trap++) {
        for (side=0; side<2; side++) {
            trap_adjacent[side]=0;
            trap_adjacent_strength[side]=0;
            trap_adjacent_strongest[side]=0;
        }
        for (dir=0; dir<4; dir++) {
            switch (OWNER(trap_square[trap]+direction[dir])) {
                case GOLD :
                    trap_adjacent[0]++;
                    trap_adjacent_strength[0]+=PIECE(trap_square[trap]+direction[dir]);
                    if (PIECE(trap_square[trap]+direction[dir])>trap_adjacent_strongest[0])
                    {
                        trap_adjacent_strongest[0]=PIECE(trap_square[trap]+direction[dir]);
                    }
                    break;
                case SILVER :
                    trap_adjacent[1]++;
                    trap_adjacent_strength[1]+=PIECE(trap_square[trap]+direction[dir]);
                    if (PIECE(trap_square[trap]+direction[dir])>trap_adjacent_strongest[1])
                    {
                        trap_adjacent_strongest[1]=PIECE(trap_square[trap]+direction[dir]);
                    }
                    break;
            }
        }
        // Basically, 200 points are given out per trap.  50 to whoever has the strongest piece by the trap, 
        // and 150 points split according to total strength of pieces, with two neutral strength added.

        // case 1 - only one side has pieces by the trap.
        if (trap_adjacent[0]>0 && trap_adjacent[1]==0) {
            trap_value[0]+=50+trap_adjacent_strength[0]*150/(trap_adjacent_strength[0]+1);
        }
        if (trap_adjacent[1]>0 && trap_adjacent[0]==0) {
            trap_value[1]+=50+trap_adjacent_strength[1]*150/(trap_adjacent_strength[1]+1);
        }
        // case 2 - both sides have pieces by the trap.
        if (trap_adjacent[0]>0 && trap_adjacent[1]>0) {
            // subcase 1 - they are equally strong.  Split 100 points according to number of pieces.
            if (trap_adjacent_strongest[0]==trap_adjacent_strongest[1]) {
                trap_value[0]+=trap_adjacent_strength[0]*200/(trap_adjacent_strength[0]+trap_adjacent_strength[1]+1);
                trap_value[1]+=trap_adjacent_strength[1]*200/(trap_adjacent_strength[0]+trap_adjacent_strength[1]+1);
            }
            // subcase 2 - gold is stronger.  Give 50 points to gold, and split 50 according to number of pieces.
            if (trap_adjacent_strongest[0]>trap_adjacent_strongest[1])
            {
                trap_value[0]+=50+trap_adjacent_strength[0]*150/(trap_adjacent_strength[0]+trap_adjacent_strength[1]+1);
                trap_value[1]+=trap_adjacent_strength[1]*150/(trap_adjacent_strength[0]+trap_adjacent_strength[1]+1);
            }
            // subcase 3 - silver is stronger.  Give 50 points to silver, and split 50 according to number of pieces.
            if (trap_adjacent_strongest[1]>trap_adjacent_strongest[0])
            {
                trap_value[0]+=trap_adjacent_strength[0]*150/(trap_adjacent_strength[0]+trap_adjacent_strength[1]+1);
                trap_value[1]+=50+trap_adjacent_strength[1]*150/(trap_adjacent_strength[0]+trap_adjacent_strength[1]+1);
            }
        }
        // special case - give minus for (possible) frames
        if (OWNER(trap_square[trap])==GOLD && trap_adjacent[1]>2)
        {
            material[trap_square[trap]]=material[trap_square[trap]]*4/5; // Trapped piece loses 20% of its value
        }
        if (OWNER(trap_square[trap])==SILVER && trap_adjacent[0]>2)
        {
            material[trap_square[trap]]=material[trap_square[trap]]*4/5; // Trapped piece loses 20% of its value
        }
    }

    // Evaluate material and individual pieces.

    for (side=0; side<2; side++) {
        for (i=0; i<cats[side]; i++) {
            switch (side) {
                case 0 : 
                    row=ROW(cat_pos[0][i]);
                    break;
                case 1 :
                    row=9-ROW(cat_pos[1][i]);
                    break;
            }
            if (row>3) {
                material[cat_pos[side][i]]=material[cat_pos[side][i]]*197/200; // Advanced cat lose 1.5 % of its value
            } else if (row==3) {
                material[cat_pos[side][i]]=material[cat_pos[side][i]]*199/200; // Slightly advanced cat lose 0.5 % of its value
            }
        }

        for (i=0; i<dogs[side]; i++) {
            switch (side) {
                case 0 : 
                    row=ROW(dog_pos[0][i]);
                    break;
                case 1 :
                    row=9-ROW(dog_pos[1][i]);
                    break;
            }
            if (row>3) {
                material[dog_pos[side][i]]=material[dog_pos[side][i]]*197/200; // Advanced cat lose 1.5 % of its value
            } else if (row==3) {
                material[dog_pos[side][i]]=material[dog_pos[side][i]]*199/200; // Slightly advanced cat lose 0.5 % of its value
            }
        }
    }

    for (square=11; square<=88; square++) {
        if (square%10==9) square+=2;
        if (OWNER(square)==GOLD || OWNER(square)==SILVER) {
            // Check if it's frozen, number of adjacent empty, strongest adjacent, and all that
            piece_adjacent[0]=0;
            piece_adjacent[1]=0;
            piece_adjacent_empty=0;
            piece_adjacent_strongest[0]=0;
            piece_adjacent_strongest[1]=0;
            for (dir=0; dir<4; dir++) {
                switch (OWNER(square+direction[dir])) {
                    case GOLD :
                        piece_adjacent[0]++;
                        if (PIECE(square+direction[dir])>piece_adjacent_strongest[0])
                        {
                            piece_adjacent_strongest[0]=PIECE(square+direction[dir]);
                        }
                        break;
                    case SILVER :
                        piece_adjacent[1]++;
                        if (PIECE(square+direction[dir])>piece_adjacent_strongest[1])
                        {
                            piece_adjacent_strongest[1]=PIECE(square+direction[dir]);
                        }
                        break;
                    case EMPTY :
                        piece_adjacent_empty++;
                        break;
                }
            }
            switch (OWNER(square)) {
                case GOLD :
                    piece_adjacent_stronger_enemy=piece_adjacent_strongest[1]>PIECE(square);
                    piece_frozen=piece_adjacent_stronger_enemy && piece_adjacent[0]==0;
                    break;
                case SILVER :
                    piece_adjacent_stronger_enemy=piece_adjacent_strongest[0]>PIECE(square);
                    piece_frozen=piece_adjacent_stronger_enemy && piece_adjacent[1]==0;
                    break;
            }
            if (piece_frozen) {
                material[square]=material[square]*9/10; // Frozen piece loses 10% of its value
            }
            if (piece_adjacent_empty==0) {
                material[square]=material[square]*199/200; // Immobile piece loses 0.5% of its value
            }
            if ((piece_frozen || piece_adjacent_empty==0) && piece_adjacent_stronger_enemy) // Our piece has limited mobility, and there is a stronger enemy piece adjacent
            {
                // Check if it's held hostage or threatened by a capture
                if (adjacent_trap[square]) // It's adjacent to a trap
                {
                    // If we have no other piece next to the trap, then consider this piece to be threatened, losing 30% of its value
                    piece_adjacent_trap=0;
                    for (dir=0; dir<4; dir++) {
                        if (OWNER(adjacent_trap[square]+direction[dir])==OWNER(square))
                        {
                            piece_adjacent_trap++;
                        }
                    }
                    if (piece_adjacent_trap==1) {
                        material[square]=material[square]*7/10;
                    }
                }
                if (adjacent2_trap[square] && BOARD(adjacent2_trap[square])==EMPTY_SQUARE) 
                // It's two steps away from an empty trap
                {
                    // If we have no piece next to the trap,
                    // Really - should check so that there is a free path to trap.
                    // then consider this piece to be threatened, losing 30% of its value
                    piece_adjacent_trap=0;
                    for (dir=0; dir<4; dir++) {
                        if (OWNER(adjacent2_trap[square]+direction[dir])==OWNER(square))
                        {
                            piece_adjacent_trap++;
                        }
                    }
                    if (piece_adjacent_trap==0) {
                        material[square]=material[square]*7/10;
                    }
                }
            }
            // Another case - if adjacent to a trap, and no other friendly piece adjacent, various possibilities for being threatened....
            switch (OWNER(square)) {
                case GOLD :
                    material_value[0]+=material[square];
                    break;
                case SILVER :
                    material_value[1]+=material[square];
                    break;
            }
        }
    }

    // Evaluate rabbits

    for (i=0; i<rabbits[0]; i++) {
        row=ROW(rabbit_pos[0][i]);
        rabbit_value[0]+=(row-1)*(row-1)*(row-1);
        if (row==7) {
            switch (OWNER(rabbit_pos[0][i]+NORTH)) {
                case EMPTY :
                    rabbit_value[0]+=RABBIT_FREE_AHEAD;
                    break;
                case GOLD :
                    rabbit_value[0]+=RABBIT_FRIENDLY_AHEAD;
                    break;
            }
            switch (OWNER(rabbit_pos[0][i]+EAST)) {
                case EMPTY :
                    rabbit_value[0]+=RABBIT_FREE_SIDE;
                    break;
                case GOLD :
                    rabbit_value[0]+=RABBIT_FRIENDLY_SIDE;
                    break;
            }
            switch (OWNER(rabbit_pos[0][i]+WEST)) {
                case EMPTY :
                    rabbit_value[0]+=RABBIT_FREE_SIDE;
                    break;
                case GOLD :
                    rabbit_value[0]+=RABBIT_FRIENDLY_SIDE;
                    break;
            }
        }
    }
    for (i=0; i<rabbits[1]; i++) {
        row=9-ROW(rabbit_pos[1][i]);
        rabbit_value[1]+=(row-1)*(row-1);
        if (row==7) {
            switch (OWNER(rabbit_pos[1][i]+SOUTH)) {
                case EMPTY :
                    rabbit_value[1]+=RABBIT_FREE_AHEAD;
                    break;
                case SILVER :
                    rabbit_value[1]+=RABBIT_FRIENDLY_AHEAD;
                    break;
            }
            switch (OWNER(rabbit_pos[1][i]+EAST)) {
                case EMPTY :
                    rabbit_value[1]+=RABBIT_FREE_SIDE;
                    break;
                case SILVER :
                    rabbit_value[1]+=RABBIT_FRIENDLY_SIDE;
                    break;
            }
            switch (OWNER(rabbit_pos[1][i]+WEST)) {
                case EMPTY :
                    rabbit_value[1]+=RABBIT_FREE_SIDE;
                    break;
                case SILVER :
                    rabbit_value[1]+=RABBIT_FRIENDLY_SIDE;
                    break;
            }
        }
    }

    // Add up all the factors

    value+=material_value[0]-material_value[1];
    value+=trap_value[0]-trap_value[1];
    value+=rabbit_value[0]-rabbit_value[1];

    return value; /* From golds perspective */
}

void BOARD_Init(board_t *bp, int player)
{
    int i, j;

    for (i=0; i<100; i++)
        BOARD(i)=OFF_BOARD_SQUARE;

    for (i=1; i<9; i++)
        for (j=1; j<9; j++)
            BOARD(10*i+j)=EMPTY_SQUARE;
}

int eval(BOARD_AS_PARAMETER)
{
    uint64_t figs[2][6] = {{gr, gc, gd, gh, gm, ge}, {sr, sc, sd, sh, sm, se}};
    uint64_t h;
    int i,j;

    board_t b;
    board_t *bp = &b;

    /* Win or Loose */
    if (player == GOLD) {
        if ((gr &  UPPER_SIDE) || !sr) return  INFINITY;
        if ((sr & BOTTOM_SIDE) || !gr) return -INFINITY;
    } else {
        if ((sr & BOTTOM_SIDE) || !gr) return -INFINITY;
        if ((gr &  UPPER_SIDE) || !sr) return  INFINITY;
    }

    BOARD_Init(bp, player);

#define b_init_piece(pl,pie,PL,PIE) \
    h = figs[pl][pie];              \
    while (h) {                     \
        i = bit_index(h & (-h));    \
        j = i % 8;                  \
        i /= 8;                     \
        h ^= h & (-h);              \
        BOARD(i*10+j) = (PL | PIE); \
    }

#define b_init_big_piece(pl,pie,PL,PIE) \
    h = figs[pl][pie];                  \
    if (h) {                            \
        i = bit_index(h);               \
        j = i % 8;                      \
        i /= 8;                         \
        BOARD(i*10+j) = (PL | PIE);     \
    }

    b_init_piece    (0,0,GOLD,RABBIT_PIECE)
    b_init_piece    (0,1,GOLD,CAT_PIECE)
    b_init_piece    (0,2,GOLD,DOG_PIECE)
    b_init_piece    (0,3,GOLD,HORSE_PIECE)
    b_init_big_piece(0,4,GOLD,CAMEL_PIECE)
    b_init_big_piece(0,5,GOLD,ELEPHANT_PIECE)
    b_init_piece    (1,0,SILVER,RABBIT_PIECE)
    b_init_piece    (1,1,SILVER,CAT_PIECE)
    b_init_piece    (1,2,SILVER,DOG_PIECE)
    b_init_piece    (1,3,SILVER,HORSE_PIECE)
    b_init_big_piece(1,4,SILVER,CAMEL_PIECE)
    b_init_big_piece(1,5,SILVER,ELEPHANT_PIECE)
    return EVAL_Eval(bp);
}

void init_eval(){}
