{-# OPTIONS -fenable-rewrite-rules    #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
module Bits.BitRepresentation (
    -- * Basic types
    Player(..),
    Piece(..),
    Position,
    PlayerBoard,
    Board(..),
    Step(..),
    Move,
    DStep,
    DMove,
    MovePhase,
    PiecePosition,

    -- * Species lists
    pieces,              -- :: [Piece]
    players,             -- :: [Player]
    dPass,               -- :: DStep
    isEmptyBoard,        -- :: Board -> Bool
    emptyBoard,          -- :: Board

    -- * Other helper functions
    (<#>),               -- :: Num a => Player -> Player -> a
    whole,               -- :: Board -> Player -> Int64
    displayBoard,        -- :: Board -> Bool -> String
    oponent,             -- :: Player -> Player
    stepInMove,          -- :: MovePhase -> DStep -> MovePhase
    canPushOrPull,       -- :: MovePhase -> Bool
    isEnd,               -- :: Board -> Bool

    -- * Parsing/creating board or position
    parseBoard,          -- :: Player -> String -> Board
    parseFlatBoard,      -- :: Player -> String -> Board
    parsePosition,       -- :: String -> (Player, Piece, Position)
    parseStep,           -- :: String -> Step
    createBoard,         -- :: Player -> [(Player, Piece, Position)] -> Board

    -- * Stepping
    makeMove,            -- :: Board -> Move -> (Board, Move)
    makeDStep,           -- :: Board -> DStep -> (Board, Move)
    makeDStep',          -- :: Board -> DStep -> Board
    makeStep,            -- :: Board -> Step -> (Board, Move)
    generatePiecesSteps, -- :: Board -> MovePhase -> [(Piece,Int64)] -> DMove
    generateMoveable,    -- :: Board -> Player -> [(Piece, Int64)]
    generateSteps,       -- :: Board -> MovePhase -> DMove
    stepInPiecePosition, -- :: DStep -> PiecePosition -> PiecePosition
    canMakeStep,         -- :: Board -> Step -> Bool
    canMakeDStep,        -- :: Board -> DStep -> Bool
    isStepBy,            -- :: Player -> DStep -> Bool

    -- * Projections
    playerFromChar,      -- :: Char -> Player
    pieceFromChar,       -- :: Char -> Piece
    playerToInt,         -- :: Player -> Int
    positionToStep,      -- :: (Player, Piece, Position) -> Step
    stepToInt,           -- :: Step -> Int32
    dStepToInt,          -- :: DStep -> Int32
) where

import Control.Arrow ((***),(&&&))
import Data.Array
import Data.Bits ((.&.), (.|.), xor, complement, bit, shiftL)
import Data.Char (digitToInt, isUpper, toLower)
import Data.Int (Int64, Int32)
import Data.List (foldl')
import Bits.MyBits

foreign import ccall "clib.h hash_piece" c_hashPiece :: Int -> Int -> Int64
                                                     -> Int64
foreign import ccall "clib.h steps_from_position"
                            c_stepsFromPosition :: Int -> Int -> Int64 -> Int64
-- | arguments: PlayerPieces OponentsStrongerPieces TestedOne
foreign import ccall "clib.h immobilised"
                            immobilised :: Int64 -> Int64 -> Int64 -> Bool


data Player = Gold | Silver
              deriving (Eq, Ord, Enum, Ix, Show)

data Piece = Rabbit | Cat | Dog | Horse | Camel | Elephant
             deriving (Eq, Ord, Enum, Ix, Show)

type Position = Int -- ^ number from [0..63]
type PlayerBoard = Array Piece Int64

data Board = Board { hash        :: !Int64
                   , figures     :: Array Player PlayerBoard
                   , wholeGold   :: !Int64
                   , wholeSilver :: !Int64
                   , mySide      :: Player
                   }

data Step = Step !Piece !Player {- from: -} !Int64 {- to: -} !Int64 | Pass
            deriving (Eq, Ord)

type Move = [Step]
type DStep = (Step, Step)
type DMove = [DStep]
type MovePhase = (Player, Int) -- ^ (active player, steps played in move)
data PiecePosition = PP !Piece !Int64


traps :: Int64
traps = 0x0000240000240000

instance Show Step where
    show Pass = "Pass"
    show (Step piece player from to) =
            showPiece player piece : (pos from ++ dir)
        where
             format :: Show a => a -> Char
             format = toLower.head.show
             d = bitIndex to - bitIndex from
             dir | to == 0 = "x"
                 | d ==  8 = "n"
                 | d == -8 = "s"
                 | d ==  1 = "w"
                 | d == -1 = "e"
                 | otherwise = error
                        ( "Impossible move from: " ++ pos from ++ " to: "
                        ++ pos to
                        ++ " (with " ++ [showPiece player piece] ++ ")")

             pos p = let q = bitIndex p in [ ['a'..'h'] !! (7 - q `mod` 8)
                                           , format $ q `div` 8 + 1]

instance Show Board where
    show = ("\n" ++) . (++ "\n") . flip displayBoard True

instance Eq Board where
    b1 == b2 = hash b1 == hash b2
            && wholeSilver b1 == wholeSilver b2
            && wholeGold b1 == wholeGold b2

---------------------------------------------------------------------

players :: [Player]
players = [Gold, Silver]

pieces :: [Piece]
pieces = [Rabbit .. Elephant]

dPass :: DStep
dPass = (Pass,Pass)

isEmptyBoard :: Board -> Bool
isEmptyBoard b = wholeGold b == 0 && wholeSilver b == 0

emptyBoard :: Board
emptyBoard = createBoard Gold []

---------------------------------------------------------------------

-- | Product of two players. Gold is 1, Silver -1.
(<#>) :: Num a => Player -> Player -> a
p1 <#> p2 | p1 == p2  =  1
          | otherwise = -1

whole :: Board -> Player -> Int64
whole b Gold   = wholeGold b
whole b Silver = wholeSilver b

showPiece :: Player -> Piece -> Char
showPiece Gold Camel   = 'M'
showPiece Silver Camel = 'm'
showPiece col piece    = (if col == Gold then id else toLower)
                            $ head $ show piece

-- | Second argument is: use noflat format or flat.
displayBoard :: Board -> Bool -> String
displayBoard b nonFlat = format [pp | i <- map bit [63,62..0] :: [Int64]
        , let pp | i .&. whole b Gold   /= 0 = g Gold i
                 | i .&. whole b Silver /= 0 = g Silver i
                 | i .&. traps /= 0 = 'x'
                 | otherwise = empty]
    where
        empty | nonFlat   = '.'
              | otherwise = ' '

        -- players piece on i position
        g :: Player -> Int64 -> Char
        g pl i = showPiece pl $ head [p | p <- pieces
                                        , ((figures b ! pl) ! p) .&. i /= 0]

        format :: String -> String
        format xs | nonFlat = (" +------------------------+\n"++) $ fst
            $ foldr (\y (ys,n) -> ([c| c <- show ((n+1) `div` 8) ++ "|"
                                     , n `mod` 8 == 7]
                                        ++ ' ':y:" "
                                        ++ [c| c <- "|\n", n `mod` 8 == 0]
                                        ++ ys
                                  , n+1))
                    (" +------------------------+\n   a  b  c  d  e  f  g  h  "
                    , 0 :: Int) xs
                  | otherwise = "[" ++ xs ++ "]"

oponent :: Player -> Player
oponent Gold = Silver
oponent Silver = Gold
{-# RULES
    "oponent" forall x. oponent (oponent x) = x
  #-}
-- enemy of my enemy is my friend, but wait, that's me!

-- | if DStep argument is (Pass,Pass) then we end this move
stepInMove :: MovePhase -> DStep -> MovePhase
stepInMove (pl,_) (Pass,_) = (oponent pl, 0)
stepInMove (pl,steps) (_,s) = (pl', steps' `mod` 4)
    where
        steps' = steps + if s == Pass then 1 else 2

        pl' = if steps' > 3 then oponent pl
                            else pl

canPushOrPull :: MovePhase -> Bool
canPushOrPull (_,sc) = sc < 3

isEnd :: Board -> Bool
isEnd b = bitCount gr == 0 || bitCount sr == 0
       || bitCount ((gr .&. upp) .|. (sr .&. btm)) /= 0
    where
        fig = figures b
        gr = fig ! Gold ! Rabbit
        sr = fig ! Silver ! Rabbit
        upp = 0xff00000000000000 :: Int64
        btm = 0x00000000000000ff :: Int64

---------------------------------------------------------------------

parseBoard :: Player -> String -> Board
parseBoard pl inp = createBoard pl $ map parsePosition $ words inp

-- | input string format: "[a8 ... h1]"
parseFlatBoard :: Player -> String -> Board
parseFlatBoard pl s =
        createBoard pl . fst $ foldr flatBoardToPositions ([],-1) $ tail s
    -- tail to skip '['

flatBoardToPositions :: Char
                     -> ([(Player, Piece, Position)], Int)
                     -> ([(Player, Piece, Position)], Int)
flatBoardToPositions char (steps, count)
    | char == ']'  = ([],-1)
    | char `elem` " x" = (steps, count+1)
    | otherwise =
        ((playerFromChar char, pieceFromChar char, count+1):steps, count+1)

-- | x in [a..h], y in [1..8] -> y*8 + x
newPosition :: Char -> Char -> Position
newPosition x y = 8 * (digitToInt y - 1) + (7 - index ('a','h') x)

parsePosition :: String -> (Player, Piece, Position)
parsePosition (p:x:y:[]) = (playerFromChar p, pieceFromChar p, newPosition x y)
parsePosition p = error ("Wrong position given: " ++ p)

parseStep :: String -> Step
parseStep (p:x:y:o:[]) =
            Step (pieceFromChar p) (playerFromChar p) (bit from) (bit to)
    where
        from = newPosition x y
        to   = case o of 'n' -> from+8; 's' -> from-8; 'x' -> -1
                         'w' -> from+1; 'e' -> from-1
                         _   -> error "Invalid move direction"
parseStep "" = Pass
parseStep s = error ("Wrong step given: " ++ s)

createBoard :: Player -> [(Player, Piece, Position)] -> Board
createBoard pl xs = fst $ makeMove bo $ map positionToStep xs
    where
        gb = array (Rabbit, Elephant) [(i,0 :: Int64) | i <- pieces]
        sb = array (Rabbit, Elephant) [(i,0 :: Int64) | i <- pieces]

        fi = array (Gold, Silver) [(Gold, gb), (Silver, sb)]
        bo = Board { hash=0, figures=fi, wholeGold=0, wholeSilver=0
                   , mySide=pl }

---------------------------------------------------------------------

-- | Note: take a look at return value, it is different from return value of
-- makeStep or makeDStep.
makeMove :: Board
         -> Move
         -> (Board, Move) -- ^ new board position with full steps sequence
                          --                    (including trapping steps)
makeMove b = foldl' (\(b1, ss1) s -> case makeStep b1 s of
                                   (b2, ss2) -> (b2, ss1 ++ s:ss2)) (b, [])
{-# INLINE makeMove #-}

makeDStep :: Board
          -> DStep
          -> (Board, Move) -- ^ new board position and trapped pieces
makeDStep b1 (s1,s2) = id *** (snd m++) $ makeStep (fst m) s2
    where m = makeStep b1 s1
{-# INLINE makeDStep #-}

makeDStep' :: Board -> DStep -> Board
makeDStep' b1 = (fst $) . makeDStep b1
{-# INLINE makeDStep' #-}

makeStep :: Board
         -> Step
         -> (Board, Move) -- ^ new board position and trapped pieces
makeStep b Pass = (b, [])
makeStep b s@(Step piece player from to) =
        ( b { hash    = hash'
            , figures = figures b // boardDiff
            , wholeGold   = wholeGold'
            , wholeSilver = wholeSilver'
            }
        , trapped)
    where
        wholePlayer = whole b player

        nearByTraps = wholePlayer `xor` from
        isTrapped p = adjecent p .&. nearByTraps == 0

        trapped =
             -- Stepping to unoccupied trap
             [Step piece player to 0 | to .&. traps /= 0, isTrapped to]
             -- Being leaved in trap
          ++ [Step pie player tr 0 | tr <- bits $ (wholePlayer) .&. traps
                                   , isTrapped tr
                                   , let pie = findPiece (figures b ! player) tr]
        steps = s:trapped
        (diffs,hash',wholeDiff) = case steps of
                [Step pie _ f t] -> ( [(pie, f `xor` t)]
                                    , hashHelp pie f t h
                                    , wholePlayer `xor` f `xor` t)
                [Step pie1 _ f1 t1, Step pie2 _ f2 t2]
                         -> ( [(pie1, f1 `xor` t1), (pie2, f2 `xor` t2)]
                            , hashHelp pie1 f1 t1 $ hashHelp pie2 f2 t2 h
                            , wholePlayer `xor` f1 `xor` t1 `xor` f2 `xor` t2)
                _ -> error "makeStep causes three pieces to move"
        h = hash b

        hashHelp pie f t h'=
            h' `xor` hashPiece player pie f `xor` hashPiece player pie t

        -- Step of some piece affects only other pieces this player has.
        boardDiff = [(player, accum xor (figures b ! player) diffs)]

        (wholeGold', wholeSilver')
                | player == Gold = (wholeDiff, wholeSilver b)
                | otherwise      = (wholeGold b, wholeDiff)


generateMoveable :: Board -> Player -> [PiecePosition]
generateMoveable b pl =
        notFrozen (allPieces pl) (allPieces (oponent pl)) (whole b pl) 0
    where
        allPieces pl' = map (\pie -> PP pie (bb pie)) pieces'
            where
                pieces' = [Elephant, Camel .. Rabbit]
                bb = (!) (figures b ! pl')

-- Filter not immobilised pieces
-- Lists of pieces needs to be sorted by stronger
notFrozen :: [PiecePosition] -- ^ players pieces to check
          -> [PiecePosition] -- ^ all oponents pieces
          -> Int64           -- ^ positions of all player pieces
          -> Int64           -- ^ positions of oponents stronger pieces
          -> [PiecePosition]
notFrozen [] _ _ _ = []
notFrozen ((PP pie poss):plPieRest) ops relatives stronger =
        [(PP pie b) | b <- bits poss
                    , not $ immobilised relatives stronger' b]

        ++ notFrozen plPieRest ops' relatives stronger'
    where
        stronger' = stronger .|. bitSum

        !(!bitSum, !ops') = go 0 ops

        go :: Int64 -> [PiecePosition] -> (Int64, [PiecePosition])
        go !sum' [] = (sum', [])
        go !sum' allOpt2@((PP pie' poss'):ops2)
                | pie' > pie = go (sum' .|. poss') ops2
                | otherwise  = (sum', allOpt2)


-- | It doesn't check wheather pieces can move.
-- in PiecePosition - position can contain multiple figures
generatePiecesSteps :: Board -> MovePhase -> [PiecePosition] -> DMove
generatePiecesSteps b mp@(pl,_) pies =
#ifdef canPass
        [dPass | snd mp /= 0] ++
#endif
        genPiecesSteps' b pl canPP pies opPie empty oArr
    where
        opPie = whole b (oponent pl)
        empty = complement $ whole b Gold .|. whole b Silver
        oArr = figures b ! oponent pl
        canPP = canPushOrPull mp

genPiecesSteps' :: Board
                -> Player
                -> Bool            -- ^ can push/pull
                -> [PiecePosition] -- ^ pieces to move
                -> Int64           -- ^ weaker oponents peaces
                -> Int64           -- ^ empty places
                -> PlayerBoard
                -> DMove
genPiecesSteps' _ _ _ [] _ _ _ = []
genPiecesSteps' b pl canPullPush ((PP pie pos):rest) opWeak empty oArr =
        -- pulls
        [(cStep w, Step (findPiece oArr pull) op pull pos)
            | canPullPush
            , w <- bits $! empty .&. adjecent pos
            , pull <- bits $! adjecent pos .&. opWeak']

        -- pushs
        ++
        [(Step (findPiece oArr w) op w to, cStep w)
            | canPullPush
            , w <- bits $! opWeak' .&. adjecent pos
            , to <- bits $! empty .&. adjecent w]

        -- simple steps
        ++ (map (cStep &&& const Pass) $ bits $
                empty .&. stepsFromPosition pl pie pos)

        ++ genPiecesSteps' b pl canPullPush rest opWeak empty oArr
    where
        cStep = Step pie pl pos
        op = oponent pl

        opStrPies = foldr (.|.) 0 $ map (oArr !) [pie .. Elephant]
        opWeak' = opWeak .&. complement opStrPies

generateSteps :: Board -> MovePhase -> DMove
generateSteps b mp@(pl,_) =
        generatePiecesSteps b mp $ generateMoveable b pl

-- | Change PiecePosition by doing step. It doesn't check validity of given
-- step.
stepInPiecePosition :: Step -> PiecePosition -> PiecePosition
stepInPiecePosition Pass piePos = piePos
stepInPiecePosition (Step _ _ fromS toS) pp@(PP pie pos)
    | fromS .&. pos /= 0 = PP pie (pos `xor` toS `xor` fromS)
    | otherwise = pp

-- | Find in array of pieces which piece is on given position
-- | second argument: only one bit number
findPiece :: Array Piece Int64 -> Int64 -> Piece
findPiece a p | a ! Rabbit   .&. p /= 0 = Rabbit
              | a ! Cat      .&. p /= 0 = Cat
              | a ! Dog      .&. p /= 0 = Dog
              | a ! Horse    .&. p /= 0 = Horse
              | a ! Camel    .&. p /= 0 = Camel
              | a ! Elephant .&. p /= 0 = Elephant
findPiece _ _ = error "Inner error in findPiece"

-- | third argument: only one bit number
stepsFromPosition :: Player -> Piece -> Int64 -> Int64
stepsFromPosition pl pie pos =
        c_stepsFromPosition (playerToInt pl) (pieceToInt pie) pos
{-# INLINE stepsFromPosition #-}

-- | argument: only one bit number
adjecent :: Int64 -> Int64
adjecent = stepsFromPosition Gold Elephant
{-# INLINE adjecent #-}

-- | Controls if old valid step is valid step in this board position
canMakeStep :: Board -> Step -> Bool
canMakeStep = canMakeStep' True

canMakeStep' :: Bool -> Board -> Step -> Bool
canMakeStep' _ _ Pass = False
canMakeStep' couldFreeze b (Step pie pl from to) =
        figures b ! pl ! pie .&. from /= 0
        && whole b Gold   .&. to == 0
        && whole b Silver .&. to == 0
        && (not couldFreeze || not (isFrozen b pie pl from))

-- | See canMakeStep
-- Note: it don't check validity of double step, it was valid somewhere else
canMakeDStep :: Board -> DStep -> Bool
canMakeDStep _ (Pass,_) = False
canMakeDStep b (s, Pass) = canMakeStep b s
canMakeDStep b (s1@(Step pie1 pl1 f1 _), Step pie2 pl2 f2 _) =
        canMakeStep b s1
        && figures b ! pl2 ! pie2 .&. f2 /= 0
        && not (isFrozen b pie pl f)
    where
        (pie,pl,f) | pie1 > pie2 = (pie1,pl1,f1)
                   | otherwise   = (pie2,pl2,f2)

isFrozen :: Board -> Piece -> Player -> Int64 -> Bool
isFrozen b pie pl pos = immobilised (whole b pl) opStronger pos
    where
        opStronger = foldl' (.|.) 0
                   $ map (figures b ! oponent pl !) $ tail [pie .. Elephant]

isStepBy :: Player -> DStep -> Bool
isStepBy pl (Step _ pl1 _ _, Pass) = pl == pl1
isStepBy pl (Step pie1 pl1 _ _, Step pie2 pl2 _ _) = pl == pl1 && pie1 > pie2
                                                  || pl == pl2 && pie1 < pie2
isStepBy _ _ = False

---------------------------------------------------------------------

pieceFromChar :: Char -> Piece
pieceFromChar c = case toLower c of
        'e' -> Elephant; 'm' -> Camel; 'h' -> Horse
        'd' -> Dog;      'c' -> Cat;   'r' -> Rabbit
        _ -> error ("Wrong piece character: " ++ [c])

playerFromChar :: Char -> Player
playerFromChar c = if isUpper c || c `elem` "gw"
                   then Gold else Silver

pieceToInt :: Piece -> Int
pieceToInt = index (Rabbit, Elephant)
{-# INLINE pieceToInt #-}

playerToInt :: Player -> Int
playerToInt Gold   = 0
playerToInt Silver = 1
{-# INLINE playerToInt #-}

positionToStep :: (Player, Piece, Position) -> Step
positionToStep (pl,pie,pos) = Step pie pl 0 (bit pos)

-- | Get generated hash number for piece on given position.
hashPiece :: Player -> Piece -> Int64 -> Int64
hashPiece _ _ 0 = 0
hashPiece pl pie pos = c_hashPiece (playerToInt pl) (pieceToInt pie) pos
{-# INLINE hashPiece #-}

-- | Encode step as 16bit number:
--   1 bit - player, 5 bits - from, 5 bits - to, 3 bits - piece
stepToInt :: Step -> Int32
stepToInt Pass = 0x00000007
stepToInt (Step pie pl from to) =
        shiftL pl' 13 .|. shiftL from' 8 .|. shiftL to' 3 .|. pie'
    where
        pie'  = fromIntegral $ pieceToInt pie
        pl'   = fromIntegral $ playerToInt pl
        from' = fromIntegral $ bitIndex from
        to'   = fromIntegral $ bitIndex to

-- | Encode two steps as 32bit number using schema from stepToInt.
dStepToInt :: DStep -> Int32
dStepToInt (s1,s2) = stepToInt s1 .|. shiftL (stepToInt s2) 16
