module MTDf (search, alpha_beta) where

import Control.Concurrent
import System.IO
import BitRepresenation
import BitEval
import Hash


mtdf :: Board       -- ^ start position
     -> (DMove, Int) -- ^ best value with PV from last time
     -> Int         -- ^ depth
     -> Player      -- ^ player on move
     -> Int         -- ^ upper bound
     -> Int         -- ^ lower bound
     -> IO (DMove, Int) -- ^ IO (steps to go, best value)
mtdf b (best, bestValue) depth pl ub lb =
        best `seq` b `seq` ub `seq` lb `seq` do
            best' <- alpha_beta b best (beta - 1, beta) depth 0 pl True
            (ub', lb') <- newBounds best'

            if lb' >= ub' then return best'
                          else mtdf b best' depth pl ub' lb'
    where
        beta = if bestValue == lb then bestValue + 1 else bestValue
        newBounds (_, bestV) | bestV < beta = return (bestV, lb)
                             | otherwise    = return (ub, bestV)

-- | iterative deepening
search :: Board -> Player -> MVar (DMove, Int) -> IO ()
search board player mvar = search' 1 ([], 0)
    where
        search' :: Int -> (DMove, Int) -> IO ()
        search' depth gues = do
            putStrLn $ "info actual " ++ show gues
            infoHash
            hFlush stdout
            m <- mtdf board gues depth player iNFINITY (-iNFINITY)
            m `seq` swapMVar mvar m
            search' (depth+1) m

-- TODO kontrola vyhry a pripadny konec

alpha_beta :: Board
           -> DMove       -- ^ best PV so far
           -> (Int, Int)  -- ^ (alpha, beta)
           -> Int         -- ^ maximal depth
           -> Int         -- ^ actual depth
           -> Player      -- ^ actual player
           -> Bool        -- ^ is maximalise node
           -> IO (DMove, Int) -- ^ (steps to go, best value)
alpha_beta board pv (alpha, beta) depth actualDepth player isMaxNode = do
        inTranspositionTable <- findHash (hash board) inverseDepth player
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
                                e <- eval board player isMaxNode
                                return ([], e)
                            else findBest (alpha', beta') ([], inf) steps
                addHash (hash board) inverseDepth player res
                return res
    where
        inverseDepth = depth - actualDepth
        (headPV,tailPV) = case pv of (h:t) -> ([h],t); _ -> ([],[])
        steps = headPV ++ generateSteps board player (actualDepth `mod` 4 /= 3)

        findBest :: (Int, Int) -> (DMove, Int) -> DMove -> IO (DMove, Int)
        findBest _ best [] = return best
        findBest bounds@(a,b) best@(_, bestValue) ((s1,s2):ss) =
                bounds `seq` best `seq` (s1,s2) `seq` do
                    (childPV, childValue) <-
                        alpha_beta board' tailPV' bounds depth actualDepth' player' isMaxNode'

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
                (player', isMaxNode') = if actualDepth' `mod` 4 /= 0
                                            then (player, isMaxNode)
                                            else (oponent player, not isMaxNode)

                newBounds childV | isMaxNode = (cmp a childV, b)
                                 | otherwise = (a, cmp b childV)

        inBounds (a,b) (_, best) | isMaxNode = best < b
                                 | otherwise = best > a

        (cmp, inf) | isMaxNode = (max, -iNFINITY)
                   | otherwise = (min,  iNFINITY)

