#include <stdint.h>
#include "clib.h"

long int random(void);


static int weight_table[6] = {
      90 /* rabbit   */,
     200 /* cat      */,
     400 /* dog      */,
     700 /* horse    */,
    1100 /* camel    */,
    1600 /* elephant */
};

#define INFINITY 100000

#define TRAPS        0x0000240000240000LLU
#define AROUND_TRAPS 0x00245a24245a2400LLU

#define NW 0xf0f0f0f000000000LLU
#define NE 0x0f0f0f0f00000000LLU
#define SW 0x00000000f0f0f0f0LLU
#define SE 0x000000000f0f0f0fLLU

#define AROUND_NW_TRAP (AROUND_TRAPS & NW)
#define AROUND_NE_TRAP (AROUND_TRAPS & NE)
#define AROUND_SW_TRAP (AROUND_TRAPS & SW)
#define AROUND_SE_TRAP (AROUND_TRAPS & SE)

/* TODO make it work */
void init_eval()
{
    /* random change pieces weight */
    weight_table[0] += random() % 9;
    weight_table[1] += random() % 20;
    weight_table[2] += random() % 40;
    weight_table[3] += random() % 70;
    weight_table[4] += random() % 110;
    weight_table[5] += random() % 160;
}

/* Hexa table:
    .... ....   0 = ....  8 = |...
    .... ....   1 = ...|  9 = |..|
    ..+. .+..   2 = ..|.  a = |.|.
    .... ....   3 = ..||  b = |.||
    .... ....   4 = .|..  c = ||..
    ..+. .+..   5 = .|.|  d = ||.|
    .... ....   6 = .||.  e = |||.
    .... ....   7 = .|||  f = ||||
*/

#define BEGIN_material_and_position(col)             \
    static inline int material_and_position_ ## col( \
        uint64_t r, uint64_t c, uint64_t d,          \
        uint64_t h, uint64_t m, uint64_t e,          \
        const int rabbit_weight)                     \
    {                                                \
        int sum = 0;                                 \
        double tmp;

#define END_material_and_position \
        return sum;               \
    }

BEGIN_material_and_position(g)
    #include "data/staticeval_g.c"
END_material_and_position

BEGIN_material_and_position(s)
    #include "data/staticeval_s.c"
END_material_and_position


#define RABBIT_WEIGHT(r) \
   (27 + weight_table[RABBIT] * 42 / (8 * (double) bit_count(r)+1))

static inline uint64_t adjecent(uint64_t pos)
{
    uint64_t res = 0;
    while (pos) {
        res |= steps_from_position(0,1, pos & (-pos));
        pos ^= pos & (-pos);
    }
    return res;
}

#define adjecentOne(b) steps_from_position(0,1, (b))

/**
 * Static evaluation functoin.
 * Keep in mind that it doesn't care about immobilization.
 *
 * TODO fix constants
 */
int eval(BOARD_AS_PARAMETER)
{
    int sum = 0, i,j;
    uint64_t figs[2][6] = {{gr, gc, gd, gh, gm, ge}, {sr, sc, sd, sh, sm, se}};
    uint64_t whole[2] = {gr|gc|gd|gh|gm|ge, sr|sc|sd|sh|sm|se};
    uint64_t tmpG, tmpS, tmpG2, tmpS2, tmp1, tmp2;
    const int trap_control[5] = {0, 60, 100, 50, 20};

    /* Win or Loose */
    if (player == GOLD) {
        if ((gr &  UPPER_SIDE) || !sr) return  INFINITY;
        if ((sr & BOTTOM_SIDE) || !gr) return -INFINITY;
    } else {
        if ((sr & BOTTOM_SIDE) || !gr) return -INFINITY;
        if ((gr &  UPPER_SIDE) || !sr) return  INFINITY;
    }

    /* Material and position */
    sum += material_and_position_g(gr,gc,gd,gh,gm,ge, RABBIT_WEIGHT(gr))
         - material_and_position_s(sr,sc,sd,sh,sm,se, RABBIT_WEIGHT(sr));

    /* Having the strongest & the second strongest piece advantage */
    for (i=5, j=2; i>0 && j; i--) {
        /* Only two biggest */
        j -= !!figs[GOLD][i] | !!figs[SILVER][i];

        /* If one of them has big advantage */
        if ((!!figs[GOLD][i]) ^ (!!figs[SILVER][i]))
            sum += (figs[GOLD][i] ? 1 : -1) * weight_table[i];
    }

    /* Controling traps */
#define traps_quantity_advantage(n) \
    ( trap_control[bit_count(AROUND_NW_TRAP & whole[n])] \
    + trap_control[bit_count(AROUND_NE_TRAP & whole[n])] \
    + trap_control[bit_count(AROUND_SW_TRAP & whole[n])] \
    + trap_control[bit_count(AROUND_SE_TRAP & whole[n])])

    sum += traps_quantity_advantage(GOLD) - traps_quantity_advantage(SILVER);
    /* TODO count in stronger piece advantage around traps */

    tmpG  = tmpS  = 0;
    tmpG2 = whole[  GOLD];
    tmpS2 = whole[SILVER];

    for (i=0; i<6; i++) {
        tmpG2 ^= figs[  GOLD][i];
        tmpS2 ^= figs[SILVER][i];

        /* Possibility to be pushed/pulled */
        sum -= bit_count(adjecent(figs[  GOLD][i]) & tmpS2) * weight_table[i]/15
             - bit_count(adjecent(figs[SILVER][i]) & tmpG2) * weight_table[i]/15;

        /* Cannot move */
        tmp1 = adjecent(tmpS2) & figs[  GOLD][i];
        tmp2 = adjecent(tmpG2) & figs[SILVER][i];

        while (tmp1) {
            sum -= (!(adjecentOne(tmp1 & (-tmp1)) & whole[GOLD]))
                 * weight_table[i]/10;
            tmp1 ^= tmp1 & (-tmp1);
        }

        while (tmp2) {
            sum += (!(adjecentOne(tmp2 & (-tmp2)) & whole[SILVER]))
                 * weight_table[i]/10;
            tmp2 ^= tmp2 & (-tmp2);
        }
        /* b & (-b) is right most bit */

        tmpG |= figs[  GOLD][i];
        tmpS |= figs[SILVER][i];
    }

    return sum;
}
