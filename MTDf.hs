{-# LANGUAGE ForeignFunctionInterface #-}
module MTDf (search, alpha_beta) where

import Data.Int (Int64)
import Data.Time.Clock
import BitRepresenation
import BitEval

foreign import ccall "clib.h reset_hash" resetHash :: IO ()
foreign import ccall "clib.h find_hash" findHash :: Int64 -> Int -> Bool
foreign import ccall "clib.h get_hash"   getHash :: Int64 -> Int
foreign import ccall "clib.h add_hash"   addHash :: Int64 -> Int -> Int -> IO ()

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
            resetHash
            if timeOk
                then do
                    m <- mtdf board gues depth player iNFINITY (-iNFINITY)
                    search' (depth+1) m time
                else return gues

-- TODO kontrola vyhry a pripadny konec
--      pri malem poctu figurek nedopocitava tahy (tahne mene figurama)
--
--      PV v hashi

alpha_beta :: Board
           -> (Move, Int) -- ^ best value with PV so far
           -> (Int, Int)  -- ^ (alpha, beta)
           -> Int         -- ^ maximal depth
           -> Int         -- ^ actual depth
           -> Player      -- ^ actual player
           -> Bool        -- ^ is maximalise node
           -> IO (Move, Int) -- ^ (steps to go, best value)
alpha_beta board gues (alpha, beta) depth actualDepth player isMaxNode
        -- | findHash (hash board) actualDepth = return ([Pass], getHash (hash board))
        | otherwise = do
            res <- if depth <= actualDepth
                        then return ([], eval board player isMaxNode)
                        else findBest (alpha, beta) ([], inf) steps
            addHash (hash board) actualDepth (snd res)
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

{-
function AlphaBetaWithMemory(n : node_type; alpha , beta , d : integer) : integer;
    if retrieve(n) == OK then /* Transposition table lookup */
        if n.lowerbound >= beta then return n.lowerbound;
        if n.upperbound <= alpha then return n.upperbound;
        alpha := max(alpha, n.lowerbound);
        beta := min(beta, n.upperbound);
    if d == 0 then g := evaluate(n); /* leaf node */
    else if n == MAXNODE then
        g := -INFINITY; a := alpha; /* save original alpha value */
        c := firstchild(n);
        while (g < beta) and (c != NOCHILD) do
            g := max(g, AlphaBetaWithMemory(c, a, beta, d - 1));
            a := max(a, g);
            c := nextbrother(c);
    else /* n is a MINNODE */
        g := +INFINITY; b := beta; /* save original beta value */
        c := firstchild(n);
        while (g > alpha) and (c != NOCHILD) do
            g := min(g, AlphaBetaWithMemory(c, alpha, b, d - 1));
            b := min(b, g);
            c := nextbrother(c);
    /* Traditional transposition table storing of bounds */
    /* Fail low result implies an upper bound */
    if g <= alpha then n.upperbound := g; store n.upperbound;
    /* Found an accurate minimax value - will not occur if called with zero window */
    if g >  alpha and g < beta then
        n.lowerbound := g; n.upperbound := g; store n.lowerbound, n.upperbound;
    /* Fail high result implies a lower bound */
    if g >= beta then n.lowerbound := g; store n.lowerbound;
    return g;
-}
