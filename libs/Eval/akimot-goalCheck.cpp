/**
 * Distributed under Open Software Licence with agreement of former author.
 * The original work is avaliable on https://github.com/tomik/akimot
 *
 * @author: Tomas Kozelek (tomas(dot)kozelek(at)gmail.com)
 * @modified: Tomas Jakl
 */

#include <cstdlib>
#include <limits.h>
#include <math.h>

typedef unsigned long long u64;
typedef unsigned int uint;



#define GRAND_MAX 0xFFFFFFFF

#include "akimot-board.h"

//---------------------------------------------------------------------
//  section Soldier
//---------------------------------------------------------------------


Soldier::Soldier(player_t player, piece_t piece, coord_t coord)
: player_(player), piece_(piece), coord_(coord)
{
}

//--------------------------------------------------------------------- 

/*
string Soldier::toString()
{
  string pieceRefStr(" RCDHMErcdhme");

  ss << pieceRefStr[piece_ + 6 * player_] << coordToStr(coord_);
string coordToStr(coord_t coord):  ss << columnRefStr[coord % 8] << coord / 8 + 1; 
  return ss.str();
}
*/

//---------------------------------------------------------------------
//  section Step
//---------------------------------------------------------------------

Step::Step( )
{
  stepType_ = STEP_NULL;
}

//---------------------------------------------------------------------
/* this constructor is mainly used for 
 * STEP_NULL or step_pass which don't use other values than stepType */
Step::Step( stepType_t stepType, player_t player )
{
  stepType_ = stepType;
  player_   = player;
}

//---------------------------------------------------------------------

Step::Step( stepType_t stepType, player_t player, piece_t piece, coord_t from, coord_t to){
  stepType_ = stepType;
  player_   = player;
  piece_    = piece;
  from_     = from;
  to_       = to;
  oppPiece_ = NO_PIECE;
  oppFrom_  = NO_SQUARE;
  oppTo_    = NO_SQUARE;
}

//---------------------------------------------------------------------

Step::Step( stepType_t stepType, player_t player, piece_t piece, coord_t from, coord_t to, 
            piece_t oppPiece, coord_t oppFrom, coord_t oppTo)
{
  stepType_ = stepType;
  player_   = player;
  piece_    = piece;
  from_     = from;
  to_       = to;
  oppPiece_ = oppPiece;
  oppFrom_  = oppFrom;
  oppTo_    = oppTo;
}

//---------------------------------------------------------------------

void Step::setValues( stepType_t stepType, player_t player, piece_t piece, coord_t from, coord_t to)
{
  stepType_ = stepType;
  player_   = player;
  piece_    = piece;
  from_     = from;
  to_       = to;
}

//---------------------------------------------------------------------

void Step::setValues( stepType_t stepType, player_t player, piece_t piece, coord_t from, coord_t to, 
            piece_t oppPiece, coord_t oppFrom, coord_t oppTo)
{
  stepType_ = stepType;
  player_   = player;
  piece_    = piece;
  from_     = from;
  to_       = to;
  oppPiece_ = oppPiece;
  oppFrom_  = oppFrom;
  oppTo_    = oppTo;
}

//---------------------------------------------------------------------

player_t Step::getPlayer() const 
{
  return player_;
}


//---------------------------------------------------------------------

bool Step::isPushPull() const
{
  return (stepType_ == STEP_PUSH || stepType_ == STEP_PULL);
}

//---------------------------------------------------------------------

int Step::count() const
{
  switch (stepType_){
    case STEP_SINGLE: return 1;
    case STEP_PUSH: return 2;
    case STEP_PULL: return 2;
    default : return 0;
  }
}

//---------------------------------------------------------------------

bool Step::operator== ( const Step& other) const
{
  if  ( stepType_ == other.stepType_ ){ // necessary condition ! 
    if ((stepType_ == STEP_PASS && player_ == other.player_) ||  stepType_ == STEP_NULL ) 
      return true;
    if ( other.player_ == player_ && other.piece_ == piece_ && other.from_ == from_ && other.to_ == to_ ) {
      if ( stepType_ == STEP_SINGLE ) 
        return true;
      else
        if ( other.oppPiece_ == oppPiece_ && other.oppFrom_ == oppFrom_ && other.oppTo_ == oppTo_ )
          return true;
    }
  }

  return false;
}

//---------------------------------------------------------------------

bool Step::operator< ( const Step& other) const
{
  //TODO optimize
  if (*this == other){ 
    return false;
  }

  int items[] = {stepType_ - other.stepType_, player_ - other.player_, 
            from_ - other.from_, to_ - other.to_, 
            piece_ - other.piece_, 
            oppFrom_ - other.oppFrom_, oppTo_ - other.oppTo_, 
            oppPiece_ - other.oppPiece_};
  const int itemsNum = sizeof(items)/sizeof(int);

  for (int i = 0; i < itemsNum; i++){
    if (items[i] < 0 ) {
     return true;
    }
    if (items[i] > 0 ) {
     return false;
    }
  }
  return false; //equality
}

//---------------------------------------------------------------------
//  section Board
//---------------------------------------------------------------------


const int bdirection[4]={NORTH, EAST, SOUTH, WEST};

u64   stepOffset_[2][7][64];
u64   winRank[2] = { 0xff00000000000000ULL ,0x00000000000000ffULL};

int lix(u64& b){

  /*if (!b) {
    return -1;
  }
  */

   static const char LogTable256[] = 
  {
   -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
  };

  unsigned r;     
  register u64 t, tt, ttt; // temporaries

  if ((ttt = b >> 32)){
    if ((tt = ttt >> 16))
    {
      r = (t = tt >> 8) ? 56 + LogTable256[t] : 48 + LogTable256[tt];
    }
    else 
    {
      r = (t = ttt >> 8) ? 40 + LogTable256[t] : 32 + LogTable256[ttt];
    }
  }else{
    if ((tt = b >> 16))
    {
      r = (t = tt >> 8) ? 24 + LogTable256[t] : 16 + LogTable256[tt];
    }
    else 
    {
      r = (t = b >> 8) ? 8 + LogTable256[t] : LogTable256[b];
    }
  }

  //TODO what happens for BIT_ON(-1) ? :(
  b ^= BIT_ON(r);
  return r;

}

//---------------------------------------------------------------------


u64 neighbors( u64 target )
{
  u64 x;

  x =  (target & NOT_H_FILE) << 1;
  x |= (target & NOT_A_FILE) >> 1;
  x |= (target /*& NOT_1_RANK*/) << 8;
  x |= (target /*& NOT_8_RANK*/) >> 8;

  return(x);
}

//---------------------------------------------------------------------

u64 sphere(int center, int radius)
{
  u64 b = BIT_ON(center);
  for (int i = 0; i < radius; i++){
    b |= neighbors(b);
  }
  return b;
}

//---------------------------------------------------------------------

u64 circle(int center, int radius)
{
  return radius == 0 ?
         BIT_ON(center) :
         sphere(center, radius) ^ sphere(center, radius - 1);
}

//---------------------------------------------------------------------

bool getBit(const u64& b, int n){
  return b & BIT_ON(n); 
}

//---------------------------------------------------------------------

// TODO the same as is in libs/Bits/bits.c
void init_stepOffsets()
{
  player_t  player;
  piece_t  piece;
  coord_t  coord_;

  // do rabbits as a special case
  for (player = 0; player < 2; player++)
    for (coord_ = 0; coord_ < BIT_LEN; coord_++) {
      u64  ts = 0ULL;

      ts |= ((BIT_ON(coord_)) & NOT_H_FILE) << 1;  // east
      ts |= ((BIT_ON(coord_)) & NOT_A_FILE) >> 1;  // west
      if (player == 0)
        ts |= ((BIT_ON(coord_)) & NOT_1_RANK) << 8;  //north
      else
        ts |= ((BIT_ON(coord_)) & NOT_8_RANK) >> 8;  //south

      stepOffset_[player][RABBIT][coord_] = ts;
    }

  // Now do the rest
  for (player = 0; player < 2; player++)
    for (piece = 2; piece < 7; piece++)
      for (coord_ = 0; coord_ < BIT_LEN; coord_++) {
        u64  ts = neighbors(BIT_ON(coord_));
        stepOffset_[player][piece][coord_] = ts;
      }
 }

//---------------------------------------------------------------------


void Board::makeStep(const Step& step){

    lastStep_ = step;

    if (step.stepType_ == STEP_NULL){
      winner_ = OPP(toMove_);
      return;
    }

    if (step.stepType_ == STEP_PASS ){
      stepCount_++;
      return;
    }

    //handle push/pull steps
    if (step.isPushPull()) {
      delSquare(step.oppFrom_, OPP(step.player_), step.oppPiece_);
      setSquare(step.oppTo_, OPP(step.player_), step.oppPiece_);
      stepCount_++;
    }

    //update board
    delSquare(step.from_, step.player_, step.piece_);
    setSquare(step.to_, step.player_, step.piece_);
    stepCount_++;

    u64 fullTraps;
    u64 dieHard; 

    //traps stuff 
    for (int player = 0; player < 2; player++){
      fullTraps = (TRAPS & bitboard_[player][0]);
      dieHard = fullTraps ^ (fullTraps & neighbors(bitboard_[player][0]));
      //full traps gold now contains to-be removed pieces 

      if (dieHard){
        int trap = lix(dieHard);
        if ( trap != BIT_EMPTY){
          delSquare(trap, player);
          //no more than one dead per player
        }
      }
    } 
}

//--------------------------------------------------------------------- 


int const goalCheck(player_t player, int stepLimit, u64 figures[2][6])
{
  Board* bb = new Board();
  bb->bitboard_[GOLD][1] = figures[GOLD][0];
  bb->bitboard_[GOLD][2] = figures[GOLD][1];
  bb->bitboard_[GOLD][3] = figures[GOLD][2];
  bb->bitboard_[GOLD][4] = figures[GOLD][3];
  bb->bitboard_[GOLD][5] = figures[GOLD][4];
  bb->bitboard_[GOLD][6] = figures[GOLD][5];

  bb->bitboard_[SILVER][1] = figures[SILVER][0];
  bb->bitboard_[SILVER][2] = figures[SILVER][1];
  bb->bitboard_[SILVER][3] = figures[SILVER][2];
  bb->bitboard_[SILVER][4] = figures[SILVER][3];
  bb->bitboard_[SILVER][5] = figures[SILVER][4];
  bb->bitboard_[SILVER][6] = figures[SILVER][5];

  u64 rabbits = bb->bitboard_[player][RABBIT];
  int from;
  while ( (from = lix(rabbits)) != -1) {

    u64 goals = winRank[player] & sphere(from, stepLimit);
    int to;
    while ( (to = lix(goals)) != -1) {
      if (bb->reachability(from, to, player, stepLimit, 0) != -1){
		delete(bb);
        return 1;
      }
    }
  }
  delete(bb);
  return 0;
}

//---------------------------------------------------------------------

//--------------------------------------------------------------------- 

int Board::reachability(int from, int to, player_t player, int limit, int used) const
{
  u64 movable = calcMovable(player);
  u64 victims[7];
  calcWeaker(player, victims);

  int reserve = limit - used - SQUARE_DISTANCE(from, to);
  //distance limit for pieces lookup
  //TODO optimize ! to + 1 and "referer from"
  int distLimit = reserve + 3;

  if (from == to){
    return used;
  }

  int pseudoReserve = reserve;
  //someone is blocking the to field 
  if (getPlayer(to) != NO_PLAYER){
    //might be friend
    pseudoReserve -= 1;
    //or opponent
    if (getPlayer(to) == OPP(player)){
      //generalize not only for rabbits
      pseudoReserve -= 1;
    }
  }

  if (pseudoReserve < 0 || getPlayer(from) == NO_PLAYER) return -1;

  StepArray steps;
  //u64 passive = BIT_ON(from);

  for (int i = 0; i <= distLimit; i ++){
    u64 iFarAway = bitboard_[player][0] & circle(from, i);

    u64 active = iFarAway & movable;
  /*
    u64 passive = passive & ~movable;
    for (int j = 0; j < i + 1; j ++){
      passive |= neighbors(passive);
    }
    */
    int bit;
    while ( (bit = lix(active)) != -1){
      int len = 0;
      //genStepsOne(bit, player, steps, len);
      piece_t piece = getPiece(bit, player);
      genStepsOneTuned(bit, player, piece, steps, len, victims[piece]);

      for (int j = 0; j < len; j++){

        int newfrom = from;
        if (steps[j].from_ == from){
          newfrom = steps[j].to_;
        }
        //moving another piece => must have reserve
        if (from == newfrom && ! reserve) continue;

        //new board - makestep - recurse
        Board* bb = new Board();
        bb->makeStep(steps[j]);
        int r = bb->reachability(newfrom, to, player, limit, 
                                  used + steps[j].count());
        delete(bb);
        if (r != -1) return r;
      }
    }
  }
  return -1;
}

//--------------------------------------------------------------------- 

void Board::genStepsOneTuned(coord_t from, player_t player, piece_t piece,
                                StepArray& steps, int & stepsNum, u64 victims) const
{
  u64 empty = ~(bitboard_[0][0] | bitboard_[1][0]);
  u64 whereStep = empty & stepOffset_[player][piece][from];

  //single steps
  for (int i = 0; i < 4; i++) {
    coord_t to = from + bdirection[i];
    if (whereStep & BIT_ON(to)) {
      steps[stepsNum++].setValues(STEP_SINGLE, player, piece, from, to);
    }
  }

  if ( piece == RABBIT || stepCount_ >= 3 || 
       ! (stepOffset_[player][piece][from] & bitboard_[OPP(player)][0])) { 
    return;
  }

  victims &= stepOffset_[player][piece][from];
   
  for (int i = 0; i < 4; i++) {
    coord_t victimFrom = from + bdirection[i];
    if (! (victims & BIT_ON(victimFrom))) {
      continue;
    }

    //pull
    u64 wherePull = empty & stepOffset_[player][piece][from];          
    for (int j = 0; j < 4; j++) {
      coord_t pullerTo = from + bdirection[j];
      if (! (wherePull & BIT_ON(pullerTo))) {
        continue;
      }

      steps[stepsNum++].setValues(STEP_PULL, player, piece, from, pullerTo, 
                            getPiece(victimFrom, OPP(player)), victimFrom, from); 
    }

    //push
    u64 wherePush = empty & stepOffset_[player][piece][victimFrom]; 
    for (int k = 0; k < 4; k++) {
      coord_t victimTo = victimFrom + bdirection[k];
      if (! (wherePush & BIT_ON(victimTo))) {
        continue;
      }

      steps[stepsNum++].setValues(STEP_PUSH, player, piece, from, victimFrom,
                        getPiece(victimFrom, OPP(player)), victimFrom, victimTo);
    }
  } 
};

//--------------------------------------------------------------------- 


u64 Board::calcMovable(player_t player) const
{
  u64 ngb[2];
  ngb[0] = neighbors(bitboard_[0][0]);
  ngb[1] = neighbors(bitboard_[1][0]);
  u64 stronger = bitboard_[OPP(player)][0];
  u64 movable = 0ULL;

  for (int piece = 1; piece < 7; piece++) {
    movable |= bitboard_[player][piece];
    stronger ^= bitboard_[OPP(player)][piece];
    movable &= ngb[player] | (~neighbors(stronger)); 
  }
  return movable;
}

//--------------------------------------------------------------------- 

void Board::calcWeaker(player_t player, u64 (&weaker)[7]) const
{
  weaker[1] = 0ULL;
  for (int i = 2; i < 7; i++){
    weaker[i] = weaker[i-1] | bitboard_[OPP(player)][i-1];
  }
}

//--------------------------------------------------------------------- 

void Board::setSquare(coord_t coord, player_t player, piece_t piece) 
{
  bitboard_[player][piece] |= BIT_ON(coord);
  bitboard_[player][0] |= BIT_ON(coord);
}

//--------------------------------------------------------------------- 

void Board::delSquare(coord_t coord, player_t player)
{
  bitboard_[player][0] ^= BIT_ON(coord);
  for (int i = 1; i < 7; i++ ){
    if (getBit(bitboard_[player][i], coord)){
      bitboard_[player][i] ^= BIT_ON(coord);
      return;
    }
  }
}

//--------------------------------------------------------------------- 

void Board::delSquare(coord_t coord, player_t player, piece_t piece) 
{
  bitboard_[player][0] ^= BIT_ON(coord);
  bitboard_[player][piece] ^= BIT_ON(coord);
}

//--------------------------------------------------------------------- 

piece_t Board::getPiece(coord_t coord, player_t player) const
{
  for (int i = 1; i < 7; i++ ){
    if (getBit(bitboard_[player][i],coord)){
      return i;
    }
  } 

  return NO_PIECE;
}

//--------------------------------------------------------------------- 

player_t Board::getPlayer(coord_t coord) const
{
  if (getBit(bitboard_[GOLD][0], coord))
    return GOLD;

  if (getBit(bitboard_[SILVER][0], coord))
    return SILVER;

  return NO_PLAYER;
}

//--------------------------------------------------------------------- 

u64 Board::weaker(player_t player, piece_t piece) const
{
  u64 res = 0ULL;
  for (int i = piece - 1; i > 0; i--){
    res |= bitboard_[player][i];  
  }
  return res;
}

//--------------------------------------------------------------------- 

u64 Board::stronger(player_t player, piece_t piece) const
{
  u64 res = 0ULL;
  for (int i = piece + 1; i < PIECE_NUM + 1; i++){
    res |= bitboard_[player][i];
  }
  return res;
}
