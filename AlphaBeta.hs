{-# LANGUAGE BangPatterns #-}
module AlphaBeta
    ( ABTTable
    , alphaBeta
    , newTT
    ) where

import BitRepresentation
import BitEval
import Hash
import Data.Bits
import Data.Int (Int64, Int32)
import qualified Data.HashTable as H

type ABTTable = TTable (DMove, Int, Int) HObject (Int64, Int, MovePhase)


alphaBeta :: Board
          -> ABTTable
          -> DMove       -- ^ best PV so far
          -> (Int, Int)  -- ^ (alpha, beta)
          -> Int         -- ^ maximal depth
          -> Int         -- ^ actual depth
          -> Player      -- ^ actual player
          -> IO (DMove, Int) -- ^ (steps to go, best value)
alphaBeta board tt pv (alpha, beta) maxDepth actualDepth pl =
        alphaBeta' board tt pv (alpha, beta) remDepth (pl, actualDepth `mod` 4)
    where
        remDepth = maxDepth - actualDepth

alphaBeta' :: Board
           -> ABTTable
           -> DMove       -- ^ best PV so far
           -> (Int, Int)  -- ^ (alpha, beta)
           -> Int         -- ^ depth remaining
           -> MovePhase
           -> IO (DMove, Int) -- ^ (steps to go, best value)
alphaBeta' !board tt !pv aB@(!alpha, !beta) !remDepth mp@(!pl, !stepCount) = do

        inTranspositionTable <- findHash tt ((hash board),remDepth,mp)
        (ttBounds@(al', bet'), maybeBest) <- if inTranspositionTable
            then do
                (ttMove, lowerBound, upperBound)
                    <- getHash tt ((hash board),remDepth,mp)
                return ( (lowerBound, upperBound)
                       , maybeResult lowerBound upperBound ttMove)
            else
                return (aB, Nothing)
        let newAB = (max alpha al', min beta bet')

        case maybeBest of
            Just bestResult -> return bestResult
            Nothing -> do
                res <- if remDepth <= 0 || isEnd board
                            then do
                                e <- eval board pl
                                return ([], e * Gold <#> mySide board)
                            else findBest newAB board tt tailPV
                                          remDepth mp ([], inf) steps
                addHash tt ((hash board),remDepth,mp)
                        (changeTTBounds res ttBounds newAB)

                return res
    where
        inf = -iNFINITY * mySide board <#> pl
        (headPV,tailPV) = case pv of (h:t) -> ([h],t); _ -> ([],[])
        steps = headPV ++ generateSteps board pl (stepCount < 3)

        maybeResult low upp mv | low >= beta  = Just (mv, low)
                               | upp <= alpha = Just (mv, upp)
                               | otherwise    = Nothing

changeTTBounds :: (DMove, Int) -- recursively computed or eval result
               -> (Int, Int)   -- old bounds
               -> (Int, Int)   -- used alphaBeta window
               -> (DMove, Int, Int)
changeTTBounds (mv, score) (ttAlpha, ttBeta) (alpha,beta)
        | score <= alpha = (mv, ttAlpha, score)  -- change upper bound in TT
        | alpha < score && score < beta = (mv, score, score)
        | score >= beta = (mv, score, ttBeta)  -- change lower bound in TT
        | otherwise = (mv, ttAlpha, ttBeta)


findBest :: (Int, Int)   -- ^ Alpha,Beta
         -> Board
         -> ABTTable
         -> DMove        -- ^ Principal variation
         -> Int          -- ^ remaining depth
         -> MovePhase
         -> (DMove, Int)
         -> DMove        -- ^ next steps to try
         -> IO (DMove, Int)
findBest _ _ _ _ _ _ bestResult [] = return bestResult
findBest bounds@(!a,!b) !board tt !pv !remDepth mp@(!pl,_)
             best0@(!_, !bestValue) ((!s1,!s2):ss)
    = do
        (!childPV, !childValue) <-
            alphaBeta' board' tt pv bounds remDepth' mp'

        let bestValue' = cmp bestValue childValue
        let !bounds' | isMaxNode = (max a childValue, b)
                     | otherwise = (a, min b childValue)
        let !best' | bestValue /= bestValue' = ((s1,s2):childPV, childValue)
                   | otherwise               = best0

        if boundsOK bounds' then findBest bounds' board tt []
                                          remDepth mp best' ss
                            else return best' -- Cut off
    where
        mp' = stepInMove mp s2
        remDepth' = remDepth - if s2 == Pass then 1 else 2
        (board', _) = makeMove board [s1,s2]

        boundsOK (!alpha, !beta) = alpha < beta
        isMaxNode = mySide board == pl
        cmp = if isMaxNode then max else min


{-
 - Implementing object for TTable, where we match type parameters as:
 -  e ~ (DMove, Int, Int) { for (PV, lower bound, upper bound) }
 -  o ~ HObject
 -  i ~ (Int64, Int, MovePhase)  { for hash, depth, move phase }
 -}
data HObject = HO { hash0 :: Int64
                  , best  :: (DMove, Int, Int)
                  , depth :: Int
                  , phase :: MovePhase
                  } deriving (Eq)

newTT :: Int -> IO ABTTable
newTT tableSize = do
        ht <- H.new (==) (`mod` ts)
        return TT
           { table     = ht
           , getEntry  = best
           , isValid   = isValid'
           , key       = key' ts
           , saveEntry = saveEntry'
           , empty     = ([], -1, -1)
           }
    where
        -- one entry in table has:
        --   * cover = 12B
        --   * one value of information = 12B
        --     + cover for each composite entry
        --   * 4 steps = 4*12 + 4*12B
        -- total: 12 + (12 + (4*12 + 4*12) + 2*12) + 12 + (12 + 2*12) B
        --        = 192B
        ts = (fromIntegral tableSize) * (500000 `div` 200)

isValid' :: HObject -> (Int64, Int, MovePhase) -> Bool
isValid' e (h,d,mp) = phase e == mp
                   && depth e >= d
                   && hash0 e == h

key' :: Int32 -> (Int64, Int, MovePhase) -> Int32
key' tableSize (h, _, (pl,s)) = fromIntegral . (`mod` tableSize) $
    fromIntegral h `xor` fromIntegral (playerToInt pl)
    `xor` (fromIntegral s `shift` 4)

saveEntry' :: (DMove, Int, Int) -> (Int64, Int, MovePhase) -> HObject
saveEntry' b (h,d,mp) =
        HO { hash0 = h
           , best  = justNeeded b
           , depth = d
           , phase = mp
           }
    where
        justNeeded (!a:(!f):(!c):(!e):_,x,y) = ([a,f,c,e],x,y)
        justNeeded (!a:(!f):(!c):[],x,y)     = ([a,f,c],x,y)
        justNeeded (!a:(!f):[],x,y)          = ([a,f],x,y)
        justNeeded (!a:[],x,y)               = ([a],x,y)
        justNeeded ([],x,y)                  = ([],x,y)
