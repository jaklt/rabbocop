{-# LANGUAGE BangPatterns #-}
module MTDf (search) where

import AlphaBeta
import BitRepresentation (Board(..), DMove)
import BitEval (iNFINITY)

import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)


mtdf :: Board       -- ^ start position
     -> ABTTable
     -> (DMove, Int) -- ^ best value with PV from last time
     -> Int         -- ^ depth
     -> Int         -- ^ upper bound
     -> Int         -- ^ lower bound
     -> IO (DMove, Int) -- ^ IO (steps to go, best value)
mtdf !b tt (!best, bestValue) depth !ub !lb = do
        best' <- alphaBeta b tt best (beta - 1, beta) depth 0 pl
        (ub', lb') <- newBounds best'

        if lb' >= ub' then return best'
                      else mtdf b tt best' depth ub' lb'
    where
        pl = mySide b
        beta = if bestValue == lb then bestValue + 1 else bestValue
        newBounds (_, bestV) | bestV < beta = return (bestV, lb)
                             | otherwise    = return (ub, bestV)

-- | iterative deepening
search :: Int -> Board -> MVar (DMove, Int) -> IO ()
search tableSize board mvar = search' 1 ([], 0) =<< newTT tableSize
    where
        search' :: Int -> (DMove, Int) -> ABTTable -> IO ()
        search' depth gues tt = do
            -- putStrLn $ "info actual " ++ show gues
            hFlush stdout
            m <- mtdf board tt gues depth iNFINITY (-iNFINITY)
            _ <- m `seq` swapMVar mvar m
            search' (depth+1) m tt

-- TODO kontrola vyhry a pripadny konec
