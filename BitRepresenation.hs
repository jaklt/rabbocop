module BitRepresenation (
    -- TODO protridit co sem nepatri
    -- TODO vyzkouset unboxed typy http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/primitives.html
    Player(..),
    Piece(..),
    Position,
    PlayerBoard,
    Board(..),
    Step(..),
    pieces,
    players,
    -- piecesRange,
    displayBoard,
    parseBoard,
    oponent,
    makeMove,
    generateSteps,
) where

import Data.Array
import Data.Bits
import Data.Char (digitToInt, isUpper, toLower)
import Data.Int (Int64)
import Data.List (sort)
import MyBits

data Player = Gold | Silver deriving (Eq, Ord, Enum, Ix, Show)
data Piece = Rabbit | Cat | Dog | Horse | Camel | Elephant
             deriving (Eq, Ord, Enum, Ix, Show)
type Position = Int -- in [0..63]

type PlayerBoard = Array Piece Int64
data Board = Board { playerOnMove :: Player, hash :: Int64
                   , board :: Array Player PlayerBoard
                   , whole :: Array Player Int64} deriving (Eq, Show)
data Step = Step Piece Player {- from: -} Int64 {- to: -} Int64 | Pass
type Move = [Step]

rightSide, leftSide, upperSide, bottomSide, traps :: Int64
rightSide  = 0x0101010101010101
leftSide   = 0x8080808080808080
upperSide  = 0xff00000000000000
bottomSide = 0x00000000000000ff
traps = 0x0000240000240000

instance Show Step where
    show Pass = "Pass"
    show (Step piece player from to) = (showPiece player piece) : (pos from ++ dir)
        where
             format :: Show a => a -> Char
             format = toLower.head.show
             d = (bitIndex to) - (bitIndex from)
             dir | d ==  8 = "n"
                 | d == -8 = "s"
                 | d ==  1 = "w"
                 | d == -1 = "e"
                 | otherwise = error ("Impossible move from: " ++ pos from ++ " to: " ++ pos to)

             pos p = let q = bitIndex p in [['a'..'h'] !! (7 - q `mod` 8), format $ q `div` 8 + 1]

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
        g pl i = showPiece pl $ head [p | p <- pieces, ((board b ! pl) ! p) .&. i /= 0]

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
                bo = array playersRange [(Gold, gPB), (Silver, sPB)]
                wh = array playersRange [(Gold, gWh), (Silver, sWh)]
            in
                Board { playerOnMove=Gold, hash=0, board=bo, whole=wh}

-- TODO pomale? treba casto proto radsi predgenerovat?
-- | third argument: only one bit number
stepsFromPosition :: Player -> Piece -> Int64 -> Int64
stepsFromPosition pl pie pos =
    foldr (.|.) 0 ([bit (bi+8) | upperSide  .&. pos == 0, pl /= Silver || pie /= Rabbit]
                ++ [bit (bi-8) | bottomSide .&. pos == 0, pl /= Gold   || pie /= Rabbit]
                ++ [bit (bi+1) | leftSide   .&. pos == 0]
                ++ [bit (bi-1) | rightSide  .&. pos == 0])
    where
        bi = bitIndex pos

-- | argument: only one bit number
adjecent :: Int64 -> Int64
adjecent = stepsFromPosition Gold Elephant

oponent :: Player -> Player
oponent Gold = Silver
oponent Silver = Gold

-- TODO pomerit, zde treba rychlost, mozna udelat makeStep
--      traps!
makeMove :: Board -> Move -> Board
makeMove board [] = board
makeMove board xs =
        board { board = board b // boardDiff
              , whole = accum xor (whole b) [(Silver, sWhDiff), (Gold, gWhDiff)]}
    where
        boardDiff = [(c, accum xor (board b ! c)
                        $ map (\(Step p _ f t) -> (p, f `xor` t))
                        $ filter (\s -> case s of (Step _ pl _ _) -> pl == c
                                                  _ -> False)
                                xs
                     ) | c <- players]

        (sWhDiff, gWhDiff) = foldr (\(Step _ pl f t) (s,g) ->
                                        if pl == Gold then (s, g `xor` f `xor` t)
                                                      else (s `xor` f `xor` t, g))
                                (0,0) xs

generateSteps :: Board -> [(Step, Step)]
generateSteps b = gen oWhole (0 :: Int64) pieces
    where
        -- a* are for active player, o* are for his oponent
        activePl = playerOnMove b    -- active player
        oponentPl = oponent activePl -- his oponent
        ap = board b ! activePl
        op = board b ! oponentPl

        oArr = op  -- oponents array
        aWhole = whole b ! activePl; oWhole = whole b ! oponentPl
        allWhole = aWhole .|. oWhole -- all used squares
        empty = complement allWhole

        gen :: Int64 -> Int64 -> [Piece] -> [(Step, Step)]
        gen _  _ [] = []
        gen opStrong opWeak (p:ps) = gen' opStrongNew opWeak p (bits $ ap ! p)
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
                    | w <- possibleStepsFromPos, pull <- bits $ adjecent pos .&. opWeak]

                -- pushs
                ++
                [(Step (findPiece oArr w) oponentPl w to, cStep w)
                    | w <- bits $ opWeak .&. adjecent pos, to <- bits $ empty .&. adjecent w]
            ) ++
                gen' opStrong opWeak pie xs
            where
                possibleStepsFromPos = bits $ empty .&. adjecent pos
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
