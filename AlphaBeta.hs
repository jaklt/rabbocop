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
alphaBeta board pv (alpha, beta) depth actualDepth player = do
        inTranspositionTable <- findHash (hash board) inverseDepth player modDepth
        (alpha', beta', bestGues) <- if inTranspositionTable
            then do
                bestGues@(_,ttValue) <- getHash (hash board) player
                return (max alpha ttValue, min beta ttValue, bestGues)
            else
                return (alpha, beta, ([],0))

        if alpha' > beta'
            then
                return bestGues
            else do
                res <- if depth <= actualDepth
                            then do
                                e <- eval board player
                                return ([], e * Gold <#> mySide board)
                            else findBest (alpha', beta') ([], inf) steps
                addHash (hash board) inverseDepth player modDepth res
                return res
    where
        inverseDepth = depth - actualDepth
        modDepth = actualDepth `mod` 4

        (headPV,tailPV) = case pv of (h:t) -> ([h],t); _ -> ([],[])
        steps = headPV ++ generateSteps board player (actualDepth `mod` 4 /= 3)

        findBest :: (Int, Int) -> (DMove, Int) -> DMove -> IO (DMove, Int)
        findBest _ best [] = return best
        findBest bounds@(a,b) best@(_, bestValue) ((s1,s2):ss) =
                bounds `seq` best `seq` (s1,s2) `seq` do
                    (childPV, childValue) <-
                        alphaBeta board' tailPV' bounds depth actualDepth' player'

                    let bestValue' = cmp bestValue childValue
                    let bounds' = newBounds childValue

                    best' <- if bestValue /= bestValue'
                                then return ((s1,s2):childPV, childValue)
                                else return best
                    if inBounds bounds best then findBest bounds' best' ss
                                            else return best -- Cut off
            where
                s = s1 : [s2 | s2 /= Pass]
                actualDepth' = actualDepth + (if s2 /= Pass then 2 else 1)
                tailPV' = if [(s1,s2)] == headPV then tailPV else []

                (board', _) = makeMove board s
                player' = if actualDepth' `mod` 4 /= 0 then player
                                                       else oponent player

                newBounds childV | isMaxNode = (cmp a childV, b)
                                 | otherwise = (a, cmp b childV)

        isMaxNode = mySide board == player

        inBounds (a,b) (_, best) | isMaxNode = best < b
                                 | otherwise = best > a

        (cmp, inf) | isMaxNode = (max, -iNFINITY)
                   | otherwise = (min,  iNFINITY)
