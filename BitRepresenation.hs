{-# LANGUAGE ForeignFunctionInterface #-}
module BitRepresenation (
    -- TODO vyzkouset unboxed typy http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/primitives.html
    Player(..),
    Piece(..),
    Position,
    PlayerBoard,
    Board(..),
    Step(..),
    Move,
    pieces,
    players,
    displayBoard,
    parseBoard,
    oponent,
    makeMove,
    makeStep,
    generateSteps,
) where

import Data.Array
import Data.Bits
import Data.Char (digitToInt, isUpper, toLower)
import Data.Int (Int64)
import Data.List (sort)
import MyBits

foreign import ccall "clib.h hash_piece" c_hashPiece :: Int -> Int -> Int -> Int64
foreign import ccall "clib.h steps_from_position"
                            c_stepsFromPosition :: Int -> Int -> Int -> Int64

data Player = Gold | Silver deriving (Eq, Ord, Enum, Ix, Show)
data Piece = Rabbit | Cat | Dog | Horse | Camel | Elephant
             deriving (Eq, Ord, Enum, Ix, Show)
type Position = Int -- in [0..63]

type PlayerBoard = Array Piece Int64
data Board = Board { hash    :: !Int64
                   , figures :: Array Player PlayerBoard
                   , whole   :: Array Player Int64} deriving (Eq, Show)
data Step = Step !Piece !Player {- from: -} !Int64 {- to: -} !Int64 | Pass
            deriving (Eq)
type Move = [Step]

traps :: Int64
traps = 0x0000240000240000

instance Show Step where
    show Pass = "Pass"
    show (Step piece player from to) = (showPiece player piece) : (pos from ++ dir)
        where
             format :: Show a => a -> Char
             format = toLower.head.show
             d = (bitIndex to) - (bitIndex from)
             dir | to == 0 = "x"
                 | d ==  8 = "n"
                 | d == -8 = "s"
                 | d ==  1 = "w"
                 | d == -1 = "e"
                 | otherwise = error ( "Impossible move from: " ++ pos from
                                    ++ " to: " ++ pos to ++ " (by: "
                                    ++ [showPiece player piece] ++ ")")

             pos p = let q = bitIndex p in [ ['a'..'h'] !! (7 - q `mod` 8)
                                           , format $ q `div` 8 + 1]

players :: [Player]
players = [Gold, Silver]

playersRange :: (Player, Player)
playersRange = (Gold, Silver)

pieces :: [Piece]
pieces = [Rabbit .. Elephant]

piecesRange :: (Piece, Piece)
piecesRange = (Rabbit, Elephant)

showPiece :: Player -> Piece -> Char
showPiece Gold Camel   = 'M'
showPiece Silver Camel = 'm'
showPiece col piece    = (if col == Gold then id else toLower) $ head $ show piece

displayBoard :: Board -> String
displayBoard b = format [pp | i <- map bit [63,62..0] :: [Int64]
        , let pp | i .&. whole b ! Gold   /= 0 = g Gold i
                 | i .&. whole b ! Silver /= 0 = g Silver i
                 | i .&. traps /= 0 = '×'
                 | otherwise = ' ']
    where
        g :: Player -> Int64 -> Char
        g pl i = showPiece pl $ head [p | p <- pieces, ((figures b ! pl) ! p) .&. i /= 0]

        format :: String -> String
        format xs = (" ++++++++++\n +"++) $ fst
            $ foldr (\y (ys,n) -> ((y:[c | c <- "+\n +", n `mod` 8 == 0]) ++ ys, n+1)) ("+++++++++", 0 :: Int) xs


parseBoard :: String -> Board
parseBoard inp = createBoard $ sort $ map parse' $ words inp
    where
        parse' :: String -> (Player, Piece, Position)
        parse' (p:x:y:[]) = (playerFromChar p, pieceFromChar p
                            -- position: (x in [a..h], y in [1..8]) -> y*8 + x
                            , 7 - (index ('a','h') x) + 8*((digitToInt y) - 1))
        parse' p = error ("Wrong position given: " ++ p)

        pieceFromChar :: Char -> Piece
        pieceFromChar c = case toLower c of
                'e' -> Elephant; 'm' -> Camel; 'h' -> Horse
                'd' -> Dog;      'c' -> Cat;   'r' -> Rabbit
                _ -> error ("Wrong piece character: " ++ [c])

        playerFromChar :: Char -> Player
        playerFromChar c = if isUpper c || c == 'g' || c == 'w'
                           then Gold else Silver

        fromPosition :: Int -> Int64
        fromPosition = bit

        createBoard :: [(Player, Piece, Position)] -> Board
        createBoard xs =
            let gb = array piecesRange [(i,0 :: Int64) | i <- pieces]
                sb = array piecesRange [(i,0 :: Int64) | i <- pieces]

                gp = filter (\(p,_,_) -> p == Gold)   xs
                sp = filter (\(p,_,_) -> p == Silver) xs

                gx = map (\(_,s,pos) -> (s, fromPosition pos)) gp
                sx = map (\(_,s,pos) -> (s, fromPosition pos)) sp

                gPB = accum (.|.) gb gx
                gWh = foldr (\(_,a) b -> a .|. b) 0 gx
                sPB = accum (.|.) sb sx
                sWh = foldr (\(_,a) b -> a .|. b) 0 sx

                ha = foldr (\(pl,pie,pos) h -> h `xor` hashPiece pl pie pos) 0 xs
                fi = array playersRange [(Gold, gPB), (Silver, sPB)]
                wh = array playersRange [(Gold, gWh), (Silver, sWh)]
            in
                Board { hash=ha, figures=fi, whole=wh}

{-# INLINE stepsFromPosition #-}
-- | third argument: only one bit number
stepsFromPosition :: Player -> Piece -> Int64 -> Int64
stepsFromPosition pl pie pos =
        c_stepsFromPosition (playerToInt pl) (pieceToInt pie) (bitIndex pos)

{-# INLINE adjecent #-}
-- | argument: only one bit number
adjecent :: Int64 -> Int64
adjecent = stepsFromPosition Gold Elephant

oponent :: Player -> Player
oponent Gold = Silver
oponent Silver = Gold

makeMove :: Board -> Move -> (Board, Move)
makeMove b ss = foldl (\(b1, ss1) s -> case makeStep b1 s of (b2, ss2) -> (b2, ss1 ++ ss2)) (b, []) ss

makeStep :: Board -> Step -> (Board, Move)
makeStep b Pass = (b, [])
makeStep b s@(Step piece player from to) =
        (b { hash = hash', figures = figures b // boardDiff
           , whole = accum xor (whole b) wholeDiff }, steps)
    where
        isTrapped p = adjecent p .&. ((whole b ! player) `xor` from) == 0
        trapped =  [Step piece player to 0 | to .&. traps /= 0, isTrapped to]
                ++ [Step pie player tr 0 | tr <- bits $ (whole b ! player) .&. traps
                                         , isTrapped tr, let pie = findPiece (figures b ! player) tr]
        steps = [s] ++ trapped
        diffs = [(pie, f `xor` t) | (Step pie _ f t) <- steps]
        hash' = foldr (\(Step pie pl f t) h -> h `xor` hashPiece pl pie (bitIndex f) `xor`
                                                 hashPiece pl pie (bitIndex t)) (hash b) steps

        boardDiff = [(player, accum xor (figures b ! player) diffs)]
        wholeDiff = [(player
                     , foldr (\(Step _ _ f t) x -> x `xor` f `xor` t) 0 steps)]

generateSteps :: Board -> Player -> Bool -> [(Step, Step)]
generateSteps b activePl canPullPush = gen oWhole (0 :: Int64) pieces
    where
        -- a* are for active player, o* are for his oponent
        oponentPl = oponent activePl -- his oponent
        ap = figures b ! activePl
        op = figures b ! oponentPl

        oArr = op  -- oponents array
        aWhole = whole b ! activePl; oWhole = whole b ! oponentPl
        allWhole = aWhole .|. oWhole -- all used squares
        empty = complement allWhole

        gen :: Int64 -> Int64 -> [Piece] -> [(Step, Step)]
        gen _  _ [] = []
        gen opStrong opWeak (p:ps) = gen' opStrongNew opWeak p (bits $! ap ! p)
                                        ++ gen opStrongNew opWeakNew ps
            where
                oponentsEqualPiece = oArr ! p
                opStrongNew  = opStrong `xor` oponentsEqualPiece
                opWeakNew = opWeak `xor` oponentsEqualPiece

        gen' :: Int64 -> Int64 -> Piece -> [Int64] -> [(Step, Step)]
        gen' _ _ _ [] = []
        gen' opStrong opWeak pie (pos:xs) =
            (if immobilised aWhole opStrong pos
            then
                []
            else
                -- simple steps
                zip
                    (map cStep possibleStepsFromPos)
                    [Pass, Pass, Pass, Pass]

                -- pulls
                ++
                [(cStep w, Step (findPiece oArr pull) oponentPl pull pos)
                    | canPullPush, w <- possibleStepsFromPos
                    , pull <- bits $! adjecent pos .&. opWeak]

                -- pushs
                ++
                [(Step (findPiece oArr w) oponentPl w to, cStep w)
                    | canPullPush, w <- bits $! opWeak .&. adjecent pos
                    , to <- bits $! empty .&. adjecent w]
            ) ++
                gen' opStrong opWeak pie xs
            where
                possibleStepsFromPos = bits $! empty .&. adjecent pos
                cStep = Step pie activePl pos


-- | arguments: PlayerPieces OponentsStrongerPieces TestedOne
immobilised :: Int64 -> Int64 -> Int64 -> Bool
immobilised ap op p = ap .&. adjP == 0 && op .&. adjP /= 0
    where adjP = adjecent p

-- | second argument: only one bit number
findPiece :: Array Piece Int64 -> Int64 -> Piece
findPiece a p | a ! Rabbit   .&. p /= 0 = Rabbit
              | a ! Cat      .&. p /= 0 = Cat
              | a ! Dog      .&. p /= 0 = Dog
              | a ! Horse    .&. p /= 0 = Horse
              | a ! Camel    .&. p /= 0 = Camel
              | a ! Elephant .&. p /= 0 = Elephant
findPiece _ _ = error "Inner error in findPiece"

pieceToInt :: Piece -> Int
pieceToInt k = index (Rabbit, Elephant) k

playerToInt :: Player -> Int
playerToInt Gold   = 0
playerToInt Silver = 1

hashPiece :: Player -> Piece -> Position -> Int64
-- hashPiece _ _ 0 = 0
hashPiece pl pie pos = c_hashPiece (playerToInt pl) (pieceToInt pie) pos
