{-# LANGUAGE BangPatterns #-}
module MTDf (newSearch) where

import AlphaBeta
import BitRepresentation (Board(..), DMove)
import BitEval (iNFINITY)

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)


newSearch :: Int  -- ^ table size
          -> IO (Board -> MVar (DMove, Int) -> IO ())
newSearch tableSize = iterative <$> newTT tableSize

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
iterative :: ABTTable -> Board -> MVar (DMove, Int) -> IO ()
iterative tt board mvar = search' 1 ([], 0)
    where
        search' :: Int -> (DMove, Int) -> IO ()
        search' depth gues = do
            -- putStrLn $ "info actual " ++ show gues
            hFlush stdout
            !m <- mtdf board tt gues depth (-iNFINITY) iNFINITY
            _ <- swapMVar mvar m
            search' (depth+1) m
