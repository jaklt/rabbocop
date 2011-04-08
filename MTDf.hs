{-# LANGUAGE BangPatterns #-}
module MTDf (search) where

import AlphaBeta
import BitRepresentation (Board(..), DMove)
import BitEval (iNFINITY)

import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)


mtdf :: Board        -- ^ start position
     -> ABTTable
     -> (DMove, Int) -- ^ best value with PV from last time
     -> Int          -- ^ depth
     -> Int          -- ^ lower bound
     -> Int          -- ^ upper bound
     -> IO (DMove, Int) -- ^ IO (steps to go, best value)
mtdf !b tt (!best, bestValue) depth !lb !ub = do
        best'@(_, bestV') <- alphaBeta b tt best (beta - 1, beta) depth 0 pl

        let (lb', ub') | bestV' < beta = (lb, bestV')
                       | otherwise     = (bestV', ub)

        if lb' >= ub' then return best'
                      else mtdf b tt best' depth lb' ub'
    where
        pl = mySide b
        beta | bestValue == lb = bestValue + 1
             | otherwise       = bestValue

-- | iterative deepening
search :: Int -> Board -> MVar (DMove, Int) -> IO ()
search tableSize board mvar = search' 1 ([], 0) =<< newTT tableSize
    where
        search' :: Int -> (DMove, Int) -> ABTTable -> IO ()
        search' depth gues tt = do
            -- putStrLn $ "info actual " ++ show gues
            hFlush stdout
            !m <- mtdf board tt gues depth (-iNFINITY) iNFINITY
            _ <- swapMVar mvar m
            search' (depth+1) m tt

-- TODO kontrola vyhry a pripadny konec
