module MTDf (search, alpha_beta) where

import Data.Time.Clock
import BitRepresenation
import BitEval

timeIsOk :: UTCTime -> IO Bool
timeIsOk t = do
    a <- getCurrentTime
    return (diffUTCTime a t < 15)

mtdf :: Board       -- ^ start position
     -> (Move, Int) -- ^ best value with PV from last time
     -> Int         -- ^ depth
     -> Player      -- ^ player on move
     -> Int         -- ^ upper bound
     -> Int         -- ^ lower bound
     -> (Move, Int) -- ^ (steps to go, best value)
mtdf b best@(_, bestValue) depth pl ub lb =
        best' `seq` b `seq` ub `seq` lb `seq`
                    if lb' >= ub' then best'
                                  else mtdf b best' depth pl ub' lb'
    where
        beta = if bestValue == lb then bestValue + 1 else bestValue
        best'@(_, bestValue') = alpha_beta b best (beta - 1, beta) depth 0 pl True
        (ub', lb') = if bestValue' < beta then (bestValue', lb) else (ub, bestValue')

-- | iterative deepening
search :: Board -> UTCTime -> Player -> IO (Move, Int)
search b t p = search' 1 ([], 0)
    where
        search' :: Int -> (Move, Int) -> IO (Move, Int)
        search' depth gues = do
            print gues
            timeOk <- gues `seq` timeIsOk t
            if timeOk
                then search' (depth+1) (mtdf b gues depth p iNFINITY (-iNFINITY))
                else return gues

-- TODO kontrola vyhry a pripadny konec
--      pri malem poctu figurek nedopocitava tahy (tahne mene figurama)

alpha_beta :: Board
           -> (Move, Int) -- ^ best value with PV so far
           -> (Int, Int)  -- ^ (alpha, beta)
           -> Int         -- ^ maximal depth
           -> Int         -- ^ actual depth
           -> Player      -- ^ actual player
           -> Bool        -- ^ is maximalise node
           -> (Move, Int) -- ^ (steps to go, best value)
alpha_beta board gues (alpha, beta) depth actualDepth player isMaxNode
        -- TODO zde transposition table
        | depth <= actualDepth = ([], eval board player)
        | isMaxNode = findBest (alpha, beta) ([], -iNFINITY) steps
        | otherwise = findBest (alpha, beta) ([],  iNFINITY) steps
    where
        -- TODO ke steps pridat/preferovat napovedu z gues
        steps = generateSteps board player (actualDepth `mod` 4 /= 3)

        findBest :: (Int, Int) -> (Move, Int) -> [(Step, Step)] -> (Move, Int)
        findBest _ best [] = best
        findBest bounds@(a,b) best@(_, bestValue) ((s1,s2):ss) =
                bounds `seq` best `seq` (s1,s2) `seq`
                    if inBounds bounds best then findBest bounds' best' ss
                                            else best
            where
                -- TODO gues zohlednit
                s = [s1] ++ [s2 | s2 /= Pass]
                (board', m) = makeMove board s
                (player', isMaxNode') = if (actualDepth + 1) `mod` 4 /= 0
                                            then (player, isMaxNode)
                                            else (oponent player, not isMaxNode)
                actualDepth' = actualDepth + 1 + (if s2 /= Pass then 1 else 0)
                (childPV, childValue) =
                    alpha_beta board' gues bounds depth actualDepth' player' isMaxNode'

                bestValue' = cmp bestValue childValue
                bounds' | isMaxNode = (cmp a childValue, b)
                        | otherwise = (a, cmp b childValue)
                best' = if bestValue /= bestValue' then (m ++ childPV, childValue)
                                                   else best

        inBounds (a,b) (_, best) | isMaxNode = best < b
                                 | otherwise = best > a

        cmp | isMaxNode = max
            | otherwise = min

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
