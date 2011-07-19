module Eval.MonteCarloEval (getValueByMC) where

import Bits.BitRepresentation
import Eval.BitEval
import System.Random


depth, simulations :: Int
depth       =   4 -- ^ simulation depth in moves
simulations =  10 -- ^ number of simulations

getValueByMC :: Board -> MovePhase -> IO Int
getValueByMC b mp = do
    s <- mapM (randomSimulation mp depth) $ replicate simulations b
    return $ sum s `div` simulations


-- | Returns pseudorandom move (and (Pass,Pass) if player is immobilised)
randomMove :: Board -> MovePhase -> IO Board
randomMove b mp@(pl,_) = do
        -- find random moveable pieces which will move in this step
        count <- randomRIO (1,3)
        pies <- randomSubList count moveable

        rndMove b mp pies
    where
        moveable = generateMoveable b pl

        rndMove :: Board -> MovePhase -> [PiecePosition] -> IO Board
        rndMove b' _ [] = return b'
        rndMove b' mp'@(pl',_) pies
            | null steps || pl' /= pl = return b'
            | otherwise  = do
                rand <- randomRIO (0,ln-1)
                let randStep@(s1,s2) = steps !! rand
                    (b'',sts) = makeMove b' [s1,s2]
                rndMove b'' (stepInMove mp' randStep) (repair pies sts)
            where
                steps = generatePiecesSteps b' mp pies
                ln    = length steps

        repair :: [PiecePosition] -> [Step] -> [PiecePosition]
        repair piePos [] = piePos
        repair piePos (st:sts) =
            repair (map (stepInPiecePosition st) piePos) sts


randomSimulation :: MovePhase -> Int -> Board -> IO Int
randomSimulation (pl,_) 0 b = eval b pl
randomSimulation mp@(pl,_) d b = do
    case generateSteps b mp of    -- TODO speedup??
        [] -> evalImmobilised b pl
        _  -> do
            b' <- randomMove b mp
            randomSimulation (oponent pl,0) (d-1) b'


-- TODO +/- 1 discussion on immobilisation
-- TODO when player changes, check weather is end of game?

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
