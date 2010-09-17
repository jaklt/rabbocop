module MTDf (search, alpha_beta) where

import Data.Time.Clock
import BitRepresenation
import BitEval
import Hash

timeIsOk :: UTCTime -> Int -> IO Bool
timeIsOk t maxTime = do
    a <- getCurrentTime
    return (diffUTCTime a t < (fromIntegral maxTime))

mtdf :: Board       -- ^ start position
     -> (Move, Int) -- ^ best value with PV from last time
     -> Int         -- ^ depth
     -> Player      -- ^ player on move
     -> Int         -- ^ upper bound
     -> Int         -- ^ lower bound
     -> IO (Move, Int) -- ^ IO (steps to go, best value)
mtdf b best@(_, bestValue) depth pl ub lb =
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
search :: Board -> Player -> Int -> IO (Move, Int)
search board player maxTime = do t <- getCurrentTime
                                 search' 1 ([], 0) t
    where
        search' :: Int -> (Move, Int) -> UTCTime -> IO (Move, Int)
        search' depth gues time = do
            putStrLn $ "info actual " ++ show gues
            timeOk <- gues `seq` timeIsOk time maxTime
            infoHash
            if timeOk
                then do
                    m <- mtdf board gues depth player iNFINITY (-iNFINITY)
                    search' (depth+1) m time
                else return gues

-- TODO kontrola vyhry a pripadny konec
--      PV v hashi

alpha_beta :: Board
           -> (Move, Int) -- ^ best value with PV so far
           -> (Int, Int)  -- ^ (alpha, beta)
           -> Int         -- ^ maximal depth
           -> Int         -- ^ actual depth
           -> Player      -- ^ actual player
           -> Bool        -- ^ is maximalise node
           -> IO (Move, Int) -- ^ (steps to go, best value)
alpha_beta board gues (alpha, beta) depth actualDepth player isMaxNode = do
        inTranspositionTable <- findHash (hash board) (depth-actualDepth)
        (alpha', beta', bestGues) <- if inTranspositionTable
            then do
                bestGues@(_,ttValue) <- getHash (hash board)
                return (max alpha ttValue, min beta ttValue, bestGues)
            else
                return (alpha, beta, ([],0))

        if alpha' > beta'
            then
                return bestGues
            else do
                res <- if depth <= actualDepth
                            then return ([], eval board player isMaxNode)
                            else findBest (alpha', beta') ([], inf) steps
                addHash (hash board) (depth-actualDepth) res
                return res
    where
        -- TODO ke steps pridat/preferovat napovedu z gues
        steps = generateSteps board player (actualDepth `mod` 4 /= 3)

        findBest :: (Int, Int) -> (Move, Int) -> [(Step, Step)] -> IO (Move, Int)
        findBest _ best [] = return best
        findBest bounds@(a,b) best@(_, bestValue) ((s1,s2):ss) =
                bounds `seq` best `seq` (s1,s2) `seq` do
                    (childPV, childValue) <-
                        alpha_beta board' gues bounds depth actualDepth' player' isMaxNode'

                    bestValue' <- return $ cmp bestValue childValue
                    bounds' <- return $ newBounds childValue

                    best' <- if bestValue /= bestValue'
                                then return (m ++ childPV, childValue)
                                else return best
                    if inBounds bounds best then findBest bounds' best' ss
                                            else return best -- Cut off
            where
                s = [s1] ++ [s2 | s2 /= Pass]
                actualDepth' = actualDepth + (if s2 /= Pass then 2 else 1)

                (board', m) = makeMove board s
                (player', isMaxNode') = if actualDepth' `mod` 4 /= 0
                                            then (player, isMaxNode)
                                            else (oponent player, not isMaxNode)

                newBounds childV | isMaxNode = (cmp a childV, b)
                                 | otherwise = (a, cmp b childV)

        inBounds (a,b) (_, best) | isMaxNode = best < b
                                 | otherwise = best > a

        (cmp, inf) | isMaxNode = (max, -iNFINITY)
                   | otherwise = (min,  iNFINITY)

