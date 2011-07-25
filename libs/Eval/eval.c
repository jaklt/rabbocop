#include <stdint.h>
#include <math.h>
#include "../clib.h"

extern long int random(void);


static int weight_table[6] = {
      90 /* rabbit   */,
     200 /* cat      */,
     400 /* dog      */,
     700 /* horse    */,
    1100 /* camel    */,
    1600 /* elephant */
};

#define iNFINITY 100000

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

/*
 * Position evaluation.
 */

#define BEGIN_position(col)                  \
    static inline int position_ ## col(      \
        uint64_t r, uint64_t c, uint64_t d,  \
        uint64_t h, uint64_t m, uint64_t e,  \
        const int rabbit_weight)             \
    {                                        \
        int sum = 0;                         \
        double tmp;

#define END_position \
        return sum;  \
    }

BEGIN_position(g)
    #include "../../data/staticeval_g.c"
END_position

BEGIN_position(s)
    #include "../../data/staticeval_s.c"
END_position

/*
 * FAME: Fritz's Arimaa Material Evaluator.
 */
#define L1 256.0
#define L2 85.0
#define A 1.5
#define B 600.0
#define C 2.0
#define normal 33.69565217391303
double msa[8] = { L1, L2, L2/A, L2/(A*A), L2/(A*A*A), L2/(A*A*A*A),
	              L2/(A*A*A*A*A), L2/(A*A*A*A*A*A) };

int fame(uint64_t figs[2][6], uint64_t whole[2])
{
	int mi, g, s, cg, cs;

	int gr = bit_count(figs[  GOLD][RABBIT]);
	int sr = bit_count(figs[SILVER][RABBIT]);
	int gl = bit_count(whole[  GOLD]) - gr;
	int sl = bit_count(whole[SILVER]) - sr;
	double gs, ss;

	/* Evaluate differences */
	gs = ss = 0;
	g = s = ELEPHANT + 1;
	cg = cs = mi = 0;
	while (g > RABBIT && s > RABBIT) {
		while (cg == 0 && g >= RABBIT) cg = bit_count(figs[  GOLD][--g]);
		while (cs == 0 && s >= RABBIT) cs = bit_count(figs[SILVER][--s]);

		if      (g > s) gs += msa[mi];
		else if (s > g) ss += msa[mi];

		mi++; cg--; cs--;
	}

	g = gr-(mi-gl);
	g = g > gr ? gr : g;

	s = sr-(mi-sl);
	s = s > sr ? sr : s;

	gs += g * (B / (sr + (C * sl)));
	ss += s * (B / (gr + (C * gl)));

	return round(((gs-ss)/normal) * 1000);
}


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
 * Static evaluation function.
 * Keep in mind that it doesn't care about immobilization.
 *
 * TODO fix constants
 */
int eval(BOARD_AS_PARAMETER)
{
    int sum = 0, i;
    uint64_t figs[2][6] = {{gr, gc, gd, gh, gm, ge}, {sr, sc, sd, sh, sm, se}};
    uint64_t whole[2] = {gr|gc|gd|gh|gm|ge, sr|sc|sd|sh|sm|se};
    uint64_t tmpG, tmpS, tmpG2, tmpS2, tmp1, tmp2;
    const int trap_control[5] = {0, 60, 100, 50, 20};

    /* Win or Loose */
    if (player == GOLD) {
        if ((gr &  UPPER_SIDE) || !sr) return  iNFINITY;
        if ((sr & BOTTOM_SIDE) || !gr) return -iNFINITY;
    } else {
        if ((sr & BOTTOM_SIDE) || !gr) return -iNFINITY;
        if ((gr &  UPPER_SIDE) || !sr) return  iNFINITY;
    }

    /* Position evaluation */
    sum += position_g(gr,gc,gd,gh,gm,ge, RABBIT_WEIGHT(gr))
         - position_s(sr,sc,sd,sh,sm,se, RABBIT_WEIGHT(sr));

	/* Material evaluation */
	sum += fame(figs, whole);

    /* Controlling traps */
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
        sum -= bit_count(adjecent(figs[  GOLD][i]) & tmpS2) * weight_table[i]/30
             - bit_count(adjecent(figs[SILVER][i]) & tmpG2) * weight_table[i]/30;

        /* Cannot move */
        tmp1 = adjecent(tmpS2) & figs[  GOLD][i];
        tmp2 = adjecent(tmpG2) & figs[SILVER][i];

        while (tmp1) {
            sum -= (!(adjecentOne(tmp1 & (-tmp1)) & whole[GOLD]))
                 * weight_table[i]/20;
            tmp1 ^= tmp1 & (-tmp1);
        }

        while (tmp2) {
            sum += (!(adjecentOne(tmp2 & (-tmp2)) & whole[SILVER]))
                 * weight_table[i]/20;
            tmp2 ^= tmp2 & (-tmp2);
        }
        /* b & (-b) is right most bit */

        tmpG |= figs[  GOLD][i];
        tmpS |= figs[SILVER][i];
    }

    return sum;
}
