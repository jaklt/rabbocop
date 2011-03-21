{-# LANGUAGE BangPatterns #-}
module AlphaBeta (alphaBeta) where

import BitRepresenation
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
alphaBeta' !board !pv (!alpha, !beta) !depth mp@(!pl, !stepCount) = do
        inTranspositionTable <- findHash (hash board) depth pl stepCount
        (alpha', beta', bestGues) <- if inTranspositionTable
            then do
                bestGues@(_,ttValue) <- getHash (hash board) pl
                return (max alpha ttValue, min beta ttValue, bestGues)
            else
                return (alpha, beta, ([],0))

        if alpha' > beta'
            then
                return bestGues
            else do
                res <- if depth <= 0
                            then do
                                e <- eval board pl
                                return ([], e * Gold <#> mySide board)
                            else findBest (alpha', beta') board tailPV
                                          depth mp ([], inf) steps
                addHash (hash board) depth pl stepCount res
                return res
    where
        inf = -iNFINITY * mySide board <#> pl
        (headPV,tailPV) = case pv of (h:t) -> ([h],t); _ -> ([],[])
        steps = headPV ++ generateSteps board pl (stepCount < 3)


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
