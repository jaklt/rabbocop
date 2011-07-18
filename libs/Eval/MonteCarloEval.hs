module Eval.MonteCarloEval (getValueByMC) where

import Bits.BitRepresentation
import Eval.BitEval
import System.Random


depth, simulations :: Int
depth       =  16 -- ^ simulation depth
simulations =  10 -- ^ number of simulations

getValueByMC :: Board -> MovePhase -> IO Int
getValueByMC b mp = do
    s <- mapM (randomSimulation mp depth)
       $ replicate simulations b
    return $ sum s `div` simulations


-- TODO +/- 1 discussion on immobilisation
-- TODO when player changes, check weather is end of game?

-- | Returns pseudorandom move (and (Pass,Pass) if player is immobilised)
randomStep :: Board -> MovePhase -> IO DStep
randomStep b mp@(pl,_) = do
        r <- randomRIO (0,119)
        return $ findOne moveable (length moveable) r
    where
        moveable = generateMoveable b pl

        findOne [] _ _ = (Pass,Pass)
        findOne steps ln r
            | pieSteps == [] = findOne (h ++ tail t) (ln-1) r
            | otherwise      = pieSteps !! (r `mod` length pieSteps)
            where
                (h,t) = splitAt (r `mod` ln) steps
                pieSteps = generatePiecesSteps b mp [head t]


randomSimulation :: MovePhase -> Int -> Board -> IO Int
randomSimulation (pl,_) 0 b = eval b pl
randomSimulation mp@(pl,_) d b = do
    case generateSteps b mp of
        [] -> evalImmobilised b pl
        _  -> do
            s <- randomStep b mp
            randomSimulation (stepInMove mp s) (d-1) (makeDStep' b s)

