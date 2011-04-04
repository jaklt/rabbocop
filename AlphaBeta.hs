{-# LANGUAGE BangPatterns #-}
module AlphaBeta (alphaBeta) where

import BitRepresentation
import BitEval
import Hash

alphaBeta :: Board
          -> DMove       -- ^ best PV so far
          -> (Int, Int)  -- ^ (alpha, beta)
          -> Int         -- ^ maximal depth
          -> Int         -- ^ actual depth
          -> Player      -- ^ actual player
          -> IO (DMove, Int) -- ^ (steps to go, best value)
alphaBeta board pv (alpha, beta) depth actualDepth pl =
        alphaBeta' board pv (alpha, beta) remDepth (pl, actualDepth `mod` 4)
    where
        remDepth = depth - actualDepth

alphaBeta' :: Board
           -> DMove       -- ^ best PV so far
           -> (Int, Int)  -- ^ (alpha, beta)
           -> Int         -- ^ depth remaining
           -> MovePhase
           -> IO (DMove, Int) -- ^ (steps to go, best value)
alphaBeta' !board !pv aB@(!alpha, !beta) !depth mp@(!pl, !stepCount) = do
        inTranspositionTable <- findHash (hash board) depth mp
        (ttBounds@(al', bet'), maybeBest) <- if inTranspositionTable
            then do
                (ttMove, lowerBound, upperBound) <- getHash (hash board) pl
                return ( (lowerBound, upperBound)
                       , maybeResult lowerBound upperBound ttMove)
            else
                return (aB, Nothing)
        let newAB = (max alpha al', min beta bet')

        case maybeBest of
            Just best -> return best
            Nothing -> do
                res <- if depth <= 0
                            then do
                                e <- eval board pl
                                return ([], e * Gold <#> mySide board)
                            else findBest newAB board tailPV
                                          depth mp ([], inf) steps
                addHash (hash board) depth mp (changeTTBounds res ttBounds newAB)
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
         -> DMove        -- ^ Principal variation
         -> Int          -- ^ Max Depth
         -> MovePhase
         -> (DMove, Int)
         -> DMove
         -> IO (DMove, Int)
findBest _ _ _ _ _ best [] = return best
findBest bounds@(!a,!b) !board !pv !depth mp@(!pl,_)
             best@(!_, !bestValue) ((!s1,!s2):ss) = do
        (!childPV, !childValue) <-
            alphaBeta' board' pv bounds depth' mp'

        let bestValue' = cmp bestValue childValue
        let !bounds' | isMaxNode = (max a childValue, b)
                     | otherwise = (a, min b childValue)
        let !best' | bestValue /= bestValue' = ((s1,s2):childPV, childValue)
                   | otherwise               = best

        if boundsOK bounds' then findBest bounds' board [] depth mp best' ss
                            else return best' -- Cut off
    where
        mp' = stepInMove mp s2
        depth' = depth - if s2 == Pass then 1 else 2
        (board', _) = makeMove board [s1,s2]

        boundsOK (!alpha, !beta) = alpha < beta
        isMaxNode = mySide board == pl
        cmp = if isMaxNode then max else min
