{-# LANGUAGE CPP #-}
module Eval.MonteCarloEval (getValueByMC) where

import Control.Applicative ((<$>))
import System.Random
import Eval.BitEval
import Bits.BitRepresentation

#ifndef noHeavyPlayout
import Data.List (sortBy)

bestOfN :: Int
bestOfN = 3
#endif


depth, simulations :: Int
depth       =   4 -- ^ simulation depth in moves
simulations =  10 -- ^ number of simulations

-- | Run `simulations`-times Monte Carlo playouts and return average score.
getValueByMC :: Board -> MovePhase -> DStep -> IO Int
getValueByMC b mp dstep = do
    s <- mapM (randomSimulation mp depth dstep) $ replicate simulations b
    return $ sum s `div` simulations


-- | Returns pseudorandom move (and (Pass,Pass) if player is immobilised)
randomMove :: Board -> MovePhase -> DStep -> IO Board
randomMove b mp@(pl,_) dstep = do
        -- find random moveable pieces with which we will move in this move
        count <- randomRIO (1,3)
        pies <- randomSubList count moveable

        randomMove' b mp pl pies dstep
    where
        moveable = generateMoveable b pl

randomMove' :: Board -> MovePhase -> Player -> [PiecePosition] -> DStep -> IO Board
randomMove' b' _ _ [] _ = return b'
randomMove' b' mp'@(pl',_) pl pies ds
    | null steps || pl' /= pl = return b'
    | otherwise  = do
#ifndef noHeavyPlayout
        -- Depending on heuristics, prefer steps with higher score.
        --
        -- Which means that from N random steps we choose one with highest
        -- evalStep.
        randSteps <- map withEval <$> randomSubList bestOfN steps
        let randStep@(s1,s2) = fst . head $ sortBy cmp randSteps
#else
        let ln = length steps
        rand <- randomRIO (0,ln-1)
        let randStep@(s1,s2) = steps !! rand
#endif
        let (b'',sts) = makeMove b' [s1,s2]
        randomMove' b'' (stepInMove mp' randStep) pl (repair pies sts) randStep
    where
        steps = generatePiecesSteps b' mp' pies

#ifndef noHeavyPlayout
        ds' | snd mp' == 0   = dPass
            | otherwise      = ds

        cmp a c = compare (snd c) (snd a)
        withEval s = (s, evalStep b' mp' ds' s)
#endif

repair :: [PiecePosition] -> [Step] -> [PiecePosition]
repair piePos [] = piePos
repair piePos (st:sts) =
    repair (map (stepInPiecePosition st) piePos) sts


randomSimulation :: MovePhase -> Int -> DStep -> Board -> IO Int
randomSimulation mp 0 _ b = eval b mp
randomSimulation mp@(pl,_) d dstep b =
    if null (generateSteps b mp) || isEnd b    -- TODO speedup??
        then evalImmobilised b pl
        else do
            b' <- randomMove b mp dstep
            randomSimulation (opponent pl,0) (d-1) dPass b'


randomSubList :: Int    -- ^ length of sublist
              -> [a]
              -> IO [a]
randomSubList n lst
        | n >= len  = return lst
        | otherwise = go n lst
    where
        len = length lst

        go 0 _ = return []
        go rest ps = do
            i <- randomRIO (0, len-(n-rest)-1)
            l <- go (rest-1) (take i ps ++ drop (i+1) ps)
            return $ ps !! i : l
