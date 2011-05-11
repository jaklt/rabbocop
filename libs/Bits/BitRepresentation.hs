{-# LANGUAGE ForeignFunctionInterface #-}
module Bits.BitRepresentation (
    -- * Basic types
    Player(..),
    Piece(..),
    Position,
    PlayerBoard,
    Board(..),
    Step(..),
    Move,
    DMove,
    MovePhase,

    -- * Species lists
    pieces,              -- :: [Piece]
    players,             -- :: [Player]

    -- * Other helper functions
    (<#>),               -- :: Num a => Player -> Player -> a
    displayBoard,        -- :: Board -> Bool -> String
    oponent,             -- :: Player -> Player
    stepInMove,          -- :: MovePhase -> Step -> MovePhase
    isEnd,               -- :: Board -> Bool

    -- * Parsing/creating board or position
    parseBoard,          -- :: Player -> String -> Board
    parseFlatBoard,      -- :: Player -> String -> Board
    parsePosition,       -- :: String -> (Player, Piece, Position)
    parseStep,           -- :: String -> Step
    createBoard,         -- :: Player -> [(Player, Piece, Position)] -> Board

    -- * Stepping
    makeMove,            -- :: Board -> Move -> (Board, Move)
    makeStep,            -- :: Board -> Step -> (Board, Move)
    generatePiecesSteps, -- :: Board -> Player -> Bool -> [(Piece,Int64)]
                         --          -> DMove
    generateMoveable,    -- :: Board -> Player -> [(Piece, Int64)]
    generateSteps,       -- :: Board -> Player -> Bool -> DMove
    canMakeStep,         -- :: Board -> Step -> Bool
    canMakeStep2,        -- :: Board -> (Step,Step) -> Bool
    stepBy,              -- :: Player -> (Step,Step) -> Bool

    -- * Projections
    playerFromChar,      -- :: Char -> Player
    pieceFromChar,       -- :: Char -> Piece
    playerToInt,         -- :: Player -> Int
    positionToStep,      -- :: (Player, Piece, Position) -> Step
) where

import Data.Array
import Data.Bits ((.&.), (.|.), xor, complement, bit)
import Data.Char (digitToInt, isUpper, toLower)
import Data.Int (Int64)
import Bits.MyBits


foreign import ccall "clib.h hash_piece" c_hashPiece :: Int -> Int -> Int
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

type Position = Int -- in [0..63]
type PlayerBoard = Array Piece Int64

data Board = Board { hash    :: !Int64
                   , figures :: Array Player PlayerBoard
                   , whole   :: Array Player Int64
                   , mySide  :: Player }
           | EmptyBoard deriving (Eq)

data Step = Step !Piece !Player {- from: -} !Int64 {- to: -} !Int64 | Pass
            deriving (Eq)

type Move = [Step]
type DMove = [(Step,Step)]
type MovePhase = (Player, Int) -- ^ (active player, steps played in move)

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

---------------------------------------------------------------------

players :: [Player]
players = [Gold, Silver]

pieces :: [Piece]
pieces = [Rabbit .. Elephant]

---------------------------------------------------------------------

-- | Product of two players. Gold is 1, Silver -1.
(<#>) :: Num a => Player -> Player -> a
p1 <#> p2 | p1 == p2  =  1
          | otherwise = -1

showPiece :: Player -> Piece -> Char
showPiece Gold Camel   = 'M'
showPiece Silver Camel = 'm'
showPiece col piece    = (if col == Gold then id else toLower)
                            $ head $ show piece

-- | Second argument is: use noflat format or flat.
displayBoard :: Board -> Bool -> String
displayBoard EmptyBoard _ = "<EmptyBoard>"
displayBoard b nonFlat = format [pp | i <- map bit [63,62..0] :: [Int64]
        , let pp | i .&. whole b ! Gold   /= 0 = g Gold i
                 | i .&. whole b ! Silver /= 0 = g Silver i
                 | i .&. traps /= 0 = 'x'
                 | otherwise = '.']
    where
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
                    (" +------------------------+\n   a  b  c  d  e  f  g  h"
                    , 0 :: Int) xs
                  | otherwise = "[" ++ xs ++ "]"

oponent :: Player -> Player
oponent Gold = Silver
oponent Silver = Gold

-- | if Step argument is Pass then we count this step as one
stepInMove :: MovePhase -> Step -> MovePhase
stepInMove (pl,steps) s = (pl', steps' `mod` 4)
    where
        steps' = steps + if s == Pass then 1 else 2

        pl' = if steps' > 3 then oponent pl
                            else pl

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
        wh = array (Gold, Silver) [(Gold, 0),  (Silver, 0)]
        bo = Board { hash=0, figures=fi, whole=wh, mySide=pl }

---------------------------------------------------------------------

makeMove :: Board -> Move -> (Board, Move)
makeMove b = foldl (\(b1, ss1) s -> case makeStep b1 s of
                                   (b2, ss2) -> (b2, ss1 ++ ss2)) (b, [])
{-# INLINE makeMove #-}

makeStep :: Board -> Step -> (Board, Move)
makeStep b Pass = (b, [])
makeStep b s@(Step piece player from to) =
        (b { hash = hash', figures = figures b // boardDiff
           , whole = accum xor (whole b) wholeDiff }, steps)
    where
        isTrapped p = adjecent p .&. ((whole b ! player) `xor` from) == 0
        trapped =  [Step piece player to 0 | to .&. traps /= 0, isTrapped to]
                ++ [Step pie player tr 0 | tr <- bits $
                                                (whole b ! player) .&. traps
                                         , isTrapped tr
                                         , let pie = findPiece (figures b ! player) tr]
        steps = s : trapped
        diffs = [(pie, f `xor` t) | (Step pie _ f t) <- steps]
        hash' = foldr (\(Step pie pl f t) h -> h
                        `xor` hashPiece pl pie (bitIndex f)
                        `xor` hashPiece pl pie (bitIndex t)) (hash b) steps

        boardDiff = [(player, accum xor (figures b ! player) diffs)]
        wholeDiff = [(player
                     , foldr (\(Step _ _ f t) x -> x `xor` f `xor` t) 0 steps)]

generateMoveable :: Board -> Player -> [(Piece, Int64)]
generateMoveable b pl =
    nimm (allPieces b pl) (allPieces b (oponent pl)) (whole b ! pl) 0

allPieces :: Board -> Player -> [(Piece, Int64)]
allPieces b pl = zip pieces' $ map ((!) (figures b ! pl)) pieces'
    where
        pieces' = [Elephant, Camel .. Rabbit]

-- Filter not immobilised pieces
-- Lists of pieces needs to be sorted by stronger
nimm :: [(Piece, Int64)] -- ^ players pieces to check
     -> [(Piece, Int64)] -- ^ all oponents pieces
     -> Int64            -- ^ map of all player pieces
     -> Int64            -- ^ map of oponents stronger pieces
     -> [(Piece,Int64)]
nimm [] _ _ _ = []
nimm ((pie,poss):plPieRest) ops relatives stronger =
        [(pie,b) | b <- bits poss
                 , not $ immobilised relatives stronger' b]
        ++ nimm plPieRest ops' relatives stronger'
    where
        (ops1,ops2) = span ((> pie) . fst) ops
        bitSum = foldr (.|.) 0 $ map snd ops1
        (ops',stronger') = (ops2, stronger .|. bitSum)

-- | Doesn't check wheather pieces can move.
-- in (piece,position) - position can contain multiple figures
generatePiecesSteps :: Board -> Player -> Bool -> [(Piece,Int64)] -> DMove
generatePiecesSteps b pl canPP pies =
        genPiecesSteps' b pl canPP pies opPie empty
    where
        opPie = whole b ! oponent pl
        empty = complement $ whole b ! Gold .|. whole b ! Silver

genPiecesSteps' :: Board
                -> Player
                -> Bool            -- ^ can push/pull
                -> [(Piece,Int64)] -- ^ pieces to move
                -> Int64           -- ^ weaker oponents peaces
                -> Int64           -- ^ empty places
                -> DMove
genPiecesSteps' _ _ _ [] _ _ = []
genPiecesSteps' b pl canPullPush ((pie,pos):rest) opWeak empty =
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
        ++ zip
            (map cStep $ bits $!
                empty .&. stepsFromPosition pl pie pos)
            [Pass, Pass, Pass, Pass]

        ++ genPiecesSteps' b pl canPullPush rest opWeak' empty
    where
        cStep = Step pie pl pos
        oArr = figures b ! op
        op = oponent pl

        opStrPies = foldr (.|.) 0 $ map (oArr !) [pie .. Elephant]
        opWeak' = opWeak .&. complement opStrPies

-- TODO better ordering
generateSteps :: Board -> Player -> Bool -> DMove
generateSteps b pl canPP = generatePiecesSteps b pl canPP $ generateMoveable b pl

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
        && whole b ! Gold   .&. to == 0
        && whole b ! Silver .&. to == 0
        && (not couldFreeze || not (isFrozen b pie pl from))

-- | See canMakeStep
-- Note: it don't check validity of double step, it was valid somewhere else
canMakeStep2 :: Board -> (Step,Step) -> Bool
canMakeStep2 _ (Pass,_) = False
canMakeStep2 b (s, Pass) = canMakeStep b s
canMakeStep2 b (s1@(Step pie1 pl1 f1 _), Step pie2 pl2 f2 _) =
        canMakeStep b s1
        && figures b ! pl2 ! pie2 .&. f2 /= 0
        && not (isFrozen b pie pl f)
    where
        (pie,pl,f) | pie1 > pie2 = (pie1,pl1,f1)
                   | otherwise   = (pie2,pl2,f2)

isFrozen :: Board -> Piece -> Player -> Int64 -> Bool
isFrozen b pie pl pos = immobilised (whole b ! pl) opStronger pos
    where
        opStronger = foldr (.|.) 0
                   $ map (figures b ! oponent pl !) $ tail [pie .. Elephant]

stepBy :: Player -> (Step,Step) -> Bool
stepBy pl (Step _ pl1 _ _, Pass) = pl == pl1
stepBy pl (Step pie1 pl1 _ _, Step pie2 pl2 _ _) = pl == pl1 && pie1 > pie2
                                                || pl == pl2 && pie1 < pie2
stepBy _ _ = False

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

hashPiece :: Player -> Piece -> Position -> Int64
hashPiece _ _ 0 = 0
hashPiece pl pie pos = c_hashPiece (playerToInt pl) (pieceToInt pie) pos
{-# INLINE hashPiece #-}
