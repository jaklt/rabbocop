{-# LANGUAGE BangPatterns #-}
module AlphaBeta
    ( ABTTable
    , alphaBeta
    , newTT
    ) where

import Bits.BitRepresentation
import Eval.BitEval
import Hash
import Control.Applicative ((<$>),(<*>))
import Data.Bits
import Data.Int (Int64, Int32)

type ABTTable = TTable (DMove, Int, Int) HObject (Int64, Int, MovePhase)
type KMoves = (DMove, DMove) -- ^ Killer moves
-- TODO make suggestions more general (with mark for Null move)


alphaBeta :: Board
          -> ABTTable
          -> DMove       -- ^ best PV so far
          -> (Int, Int)  -- ^ (alpha, beta)
          -> Int         -- ^ maximal depth
          -> Player      -- ^ actual player
          -> IO (DMove, Int) -- ^ (steps to go, best value)
alphaBeta board tt pv (alpha, beta) maxDepth pl =
        proj <$> alphaBeta' board tt (pv, emptyKM) (alpha, beta) maxDepth (pl, 0)
    where
        proj (a,b,_) = (a,b)

alphaBeta' :: Board
           -> ABTTable
           -> (DMove, KMoves) -- ^ suggestions: best PV so far, killer moves
           -> (Int, Int)      -- ^ (alpha, beta)
           -> Int             -- ^ depth remaining
           -> MovePhase
           -> IO (DMove, Int, KMoves) -- ^ (steps to go, best value, killers)
alphaBeta' !board tt !sugg aB@(!alpha, !beta) !remDepth mp@(!pl, !stepCount) = do

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
        forbidden <- isForbidden board mp

        case maybeBest of
            -- to omit repetitions
            _ | forbidden -> return ([], -iNFINITY, emptyKM)
            -- TODO +/- if starting aB seach as "oponent (mySide board)"?

            Just bestResult -> return bestResult
            Nothing -> do
                res <- if remDepth <= 0 || isEnd board
                            then do
                                e <- eval board pl
                                return ([], e * Gold <#> mySide board, [])
                            else findBest board tt (tailPV, tailKM) newAB
                                          remDepth mp ([], negInf) steps
                addHash tt ((hash board),remDepth,mp)
                        (changeTTBounds res ttBounds newAB)

                return $ repairKM (snd sugg) res
    where
        negInf = -iNFINITY * mySide board <#> pl
        (headPV,tailPV) = case fst sugg of (h:t) -> ([h],t); _ -> ([],[])
        (headKM,tailKM) = case snd sugg of
                            ([],[])      -> ([],    emptyKM)
                            (a:as, [])   -> ([a],   (as,[]))
                            (a:as, b:bs) -> ([a,b], (as,bs))
                            _            -> ([],    emptyKM)
        headKM' = filter ((&&) <$> canMakeStep2 board <*> stepBy pl) headKM
        steps = headPV ++ headKM' ++ generateSteps board pl (stepCount < 3)

        maybeResult low upp mv | low >= beta  = Just (mv, low, emptyKM)
                               | upp <= alpha = Just (mv, upp, emptyKM)
                               | otherwise    = Nothing

emptyKM :: KMoves
emptyKM = ([],[])

repairKM :: KMoves -> (DMove, Int, DMove) -> (DMove, Int, KMoves)
repairKM (km1,km2) (dm,sc,km') = (dm,sc,kms)
    where
        kms | km' == [] || km1 == km' = (km1,km2)
            | otherwise               = (km',km1)

changeTTBounds :: (DMove, Int, DMove) -- recursively computed or eval result
               -> (Int, Int)   -- old bounds
               -> (Int, Int)   -- used alphaBeta window
               -> (DMove, Int, Int)
changeTTBounds (mv, score, _) (ttAlpha, ttBeta) (alpha,beta)
        | score <= alpha = (mv, ttAlpha, score)  -- change upper bound in TT
        | score >= beta  = (mv, score,  ttBeta)  -- change lower bound in TT
        | alpha < score && score < beta = (mv, score, score)
        | otherwise = (mv, ttAlpha, ttBeta)


findBest :: Board
         -> ABTTable
         -> (DMove, KMoves) -- ^ Principal variation, Killer moves
         -> (Int, Int)   -- ^ Alpha,Beta
         -> Int          -- ^ remaining depth
         -> MovePhase
         -> (DMove, Int)
         -> DMove        -- ^ next steps to try
         -> IO (DMove, Int, DMove) -- ^ (PV, score, killer move)
findBest _ _ _ _ _ _ bestResult [] = return $ makeTriple bestResult []
findBest !board tt !sugg bounds@(!a,!b) !remDepth mp@(!pl,_)
             best0@(!_, !bestValue) ((!s1,!s2):ss)
    = do
        (!childPV, !childValue, childKM) <-
            alphaBeta' board' tt sugg bounds remDepth' mp'

        let bestValue' = cmp bestValue childValue
        let !bounds' | isMaxNode = (max a childValue, b)
                     | otherwise = (a, min b childValue)
        let !best' | bestValue /= bestValue' = ((s1,s2):childPV, childValue)
                   | otherwise               = best0

        if boundsOK bounds' then findBest board tt ([],childKM) bounds'
                                          remDepth mp best' ss
                            -- Cut off
                            else return $ makeTriple best' ((s1,s2):fst childKM)
    where
        mp' = stepInMove mp s2
        remDepth' = remDepth - if s2 == Pass then 1 else 2
        (board', _) = makeMove board [s1,s2]

        boundsOK (!alpha, !beta) = alpha < beta
        isMaxNode = mySide board == pl
        cmp = if isMaxNode then max else min

makeTriple :: (a,b) -> c -> (a,b,c)
makeTriple (a,b) c = (a,b,c)


{-|
 - Implementing object for TTable, where we match type parameters as:
 -  e ~ (DMove, Int, Int) { for (PV, lower bound, upper bound) }
 -  o ~ HObject
 -  i ~ (Int64, Int, MovePhase)  { for hash, depth, move phase }
|-}
data HObject = HO { hash0 :: Int64
                  , best  :: (DMove, Int, Int)
                  , depth :: Int
                  , phase :: MovePhase
                  } deriving (Eq)

newTT :: Int -> IO ABTTable
newTT tableSize = do
        ht <- newHT (`mod` ts)
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
