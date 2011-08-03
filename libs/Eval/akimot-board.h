/** 
 *  @file board.h
 *
 *  @brief Board representation.
 *  @full Board representation with all its nuiances is defined here. 
 *  Main pillar is class Board itself representing board and its manipulation 
 *  along with move generation, verification and playing.
 */

#pragma once

#include <queue>
#include <list>

using std::list;

/**max number of pieces per player*/
#define MAX_PIECES  16
#define MAX_STEPS  80

#define STEPS_IN_MOVE 4

typedef unsigned long long u64;

#define IS_PLAYER(player) (player == GOLD || player == SILVER)
#define IS_PIECE(piece) ( piece > 0 && piece <= 6)

#define NORTH 8
#define SOUTH -8
#define EAST 1
#define WEST -1

#define BIT_EMPTY -1

#define GOLD        0
#define SILVER      1
#define NO_PLAYER   2

#define NO_SQUARE    -1

#define NO_PIECE    0
#define RABBIT      1
#define CAT         2
#define DOG         3
#define HORSE       4
#define CAMEL       5
#define ELEPHANT    6
#define PIECE_NUM   6

#define OPP(player) (1 - player)				 //opponent
#define BIT_ON(n) (1ULL << (n))          //creates empty board with one bit set on n

#define STEP_PASS     0
#define STEP_SINGLE   1
#define STEP_PUSH     2
#define STEP_PULL     3
//no step is possible ( not even pass ! - position repetition )
#define STEP_NULL     4

#define SQUARE_DISTANCE(s1, s2) (abs(s1/8 - s2/8) + abs(s1%8 - s2%8))

typedef int player_t;
typedef int piece_t;
typedef int coord_t;

typedef uint stepType_t;

#define BIT_LEN 64
#define NOT_1_RANK  0x00ffffffffffffffULL
#define NOT_8_RANK  0xffffffffffffff00ULL
#define NOT_A_FILE  0xfefefefefefefefeULL
#define NOT_H_FILE  0x7f7f7f7f7f7f7f7fULL
#define TRAPS       0x0000240000240000ULL
#define FULL        0xffffffffffffffffULL

extern u64 winRank[2]; 
extern u64 stepOffset_[2][7][64]; 

/**
* Left index bit. 
*
* Returns highest order bit and shifts the given bitset !  
*/
int lix(u64& b);

/**
* Bit getter. 
*/
bool getBit(const u64& b, int n);

/**
* Mask of all neighbors of given bitset.
* */
u64 neighbors(u64);

/**
* Mask of sphere.
*/
u64 sphere(int center, int radius);

/**
* Mask of circle. 
*/
u64 circle(int center, int radius);


class Board;

/**
 * Piece on board is a soldier. 
 */
class Soldier
{
  public: 
    Soldier(player_t player, piece_t piece, coord_t coord);

    piece_t  piece() { return piece_; }
    player_t player() { return player_; }
    coord_t  coord() { return coord_; }

  private: 
    Soldier(){};

    player_t player_;
    piece_t piece_;
    coord_t coord_;
};

typedef list<Soldier> SoldierList;
typedef SoldierList::iterator SoldierListIter;

class OB_BOARD;

/**
 * One step of a player.
 *
 * Represents one of the following:
 *   single-step steps - i.e. move of the piece
 *   double-step steps - i.e. push/pulls
 *   pass moves
 *   no step moves     - i.e. resignation 
*/ 
class Step
{
  public:
		Step();
		Step(stepType_t, player_t);
    Step(stepType_t, player_t, piece_t, coord_t, coord_t);
    Step(stepType_t, player_t, piece_t, coord_t, coord_t, piece_t, coord_t, coord_t);

    player_t getPlayer() const;
    bool isPushPull() const;

    /**
     * Actual step count.
     *
     * Single == 1, push/pull == 2, NULL/PASS == 0 
     */
    int count() const;

    /**
     * Checks whether step moves any piece. 
     *
     * @return false if step is STEP_PASS/STEP_NULL otherwise true.
     */
	bool operator== (const Step&) const;
    bool operator<(const Step&) const;

    void setValues( stepType_t, player_t, piece_t, coord_t, coord_t );
    void setValues( stepType_t, player_t, piece_t, coord_t, coord_t, 
                    piece_t, coord_t, coord_t );

	protected:
    stepType_t    stepType_;    
    player_t      player_;      
    piece_t       piece_;  
    coord_t      from_;     
    coord_t      to_;        

    piece_t       oppPiece_;  
    coord_t      oppFrom_;
    coord_t      oppTo_;

    friend class Board;
    friend class OB_Board;
};

typedef list<Step> StepList;
typedef StepList::iterator StepListIter;



//steps
typedef Step  StepArray[MAX_STEPS];
//heuristics for steps (separated because used VERY LITTLE)
typedef float  HeurArray[MAX_STEPS];

typedef list<int> intList;

typedef u64 Bitboard[2][7];

extern "C" {
/**
 * Full goal check.
 *
 * Done through limited full width search.
 */
int const goalCheck(player_t player, int stepLimit, u64 figures[2][6]);

void init_stepOffsets();
}

/**
 * Board representation.
 *
 * One of the pillars of the whole program. Defines mechanisms for:
 * \li position load
 * \li step generation, step/move playing/unplaying
 * \li search extensions
 * \li repetitions handling and other mics
 *
 */
class Board
{

  public:
    Board(){};
	/*
    void *operator new(size_t size);
    void operator delete(void* p);
	*/

    /**
     * Reachability check.
     *
     * Used in goalCheck. 
     */
    int reachability(int from, int to, player_t player, 
                    int limit, int used) const;

    /**
     * Push Pull Step generation for one piece. 
     *
     * @param coord  Steps are generated for piece at this coord.
     * @param player Player to generate steps for. 
     * @param piece  Piece ( for time save). 
     * @param steps  Steps are stored in this array.
     * @param stepsnum Size of step array.
     */
    inline void genStepsOneTuned(coord_t coord, player_t player, piece_t piece, 
                              StepArray& steps, int& stepsNum, u64 victims) const;

    /**
     * Calculates not frozen mask.
     */
    u64 calcMovable(player_t player) const;

    /**
     * Calculates weaker pieces.
     *
     * @param player Calculation is done for this player. 
     * @param weaker This array is filled. 
     */
    void calcWeaker(player_t player, u64 (&weaker)[7]) const;

    void setSquare(coord_t, player_t, piece_t);
    void delSquare(coord_t, player_t);											
    void delSquare(coord_t, player_t, piece_t);											

    /**
     * Piece getter for coord.
     */
    piece_t	getPiece(coord_t, player_t) const;

    /**
     * Player getter for coord.
     */
    player_t getPlayer(coord_t) const;

    /**
     * Weaker mask getter.
     *
     * @param player Target player.
     * @param piece Reference piece.
     * @return Mask of player pieces weaker than reference piece.
     */
    u64 weaker(player_t player, piece_t piece) const;

    /**
     * Stronger mask getter.
     * Analogical to weaker mask getter.
     *
     * @param player Target player.
     * @param piece Reference piece.
     * @return Mask of player pieces stronger than reference piece.
     */
    u64 stronger(player_t player, piece_t piece) const;

    /**
     * Making the step.
     *
     * One of the crucial methods in the boardstructure.
     * @param step Step to be made (kills are resolved as well).
     * @param update If true - board structure is updated (added
     * steps, frozenBoard update).
     */
	void makeStep(const Step& step);


    Bitboard bitboard_;

    /**Last made step.*/
    Step lastStep_;

	// move consists of up to 4 steps ( push/pull  counting for 2 ),
    uint  moveCount_;

	// step is either pass or single piece step or push/pull step,
	// thus stepCount_ takes values 0 - 4 
    uint  stepCount_;

    player_t toMove_;
	player_t winner_;
};
