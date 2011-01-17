{-# LANGUAGE BangPatterns #-}
module MTDf (search) where

import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)
import AlphaBeta
import BitRepresenation (Board, Player, DMove)
import BitEval (iNFINITY)
import Hash (infoHash)


mtdf :: Board       -- ^ start position
     -> (DMove, Int) -- ^ best value with PV from last time
     -> Int         -- ^ depth
     -> Player      -- ^ player on move
     -> Int         -- ^ upper bound
     -> Int         -- ^ lower bound
     -> IO (DMove, Int) -- ^ IO (steps to go, best value)
mtdf !b (!best, bestValue) depth pl !ub !lb = do
        best' <- alphaBeta b best (beta - 1, beta) depth 0 pl True
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
            _ <- m `seq` swapMVar mvar m
            search' (depth+1) m

-- TODO kontrola vyhry a pripadny konec
