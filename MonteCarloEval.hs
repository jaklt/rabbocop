module MonteCarloEval (getValueByMC) where

import BitRepresentation
import BitEval
import System.Random


depth, simulations :: Int
depth       =  20 -- ^ simulation depth
simulations = 100 -- ^ number of simulations

getValueByMC :: Board -> MovePhase -> IO Int
getValueByMC b mp = do
    s <- mapM (randomSimulation mp depth) $ replicate simulations b
    return $ sum s `div` simulations

-- TODO +/- 1 discussion on empty steps
randomSimulation :: MovePhase -> Int -> Board -> IO Int
randomSimulation (pl,_) 0 b = eval b pl
randomSimulation mp@(pl,sc) d b =
    case generateSteps b pl (sc < 2) of
        [] -> evalImmobilised b pl
        xs -> do
            (s1,s2) <- chooseRandomly xs
            randomSimulation (stepInMove mp s2) (d-1) (fst $ makeMove b [s1,s2])

chooseRandomly :: [a] -> IO a
chooseRandomly xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

-- TODO makeMove could be rewriten by makeStep
-- TODO measuring length is ineficient
