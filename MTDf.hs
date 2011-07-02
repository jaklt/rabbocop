{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
#ifdef ENGINE
module Main (main) where
import AEI
#else
module MTDf (newSearch) where
#endif

import AlphaBeta
import Bits.BitRepresentation (Board(..), DMove)
import Eval.BitEval (iNFINITY)

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)


newSearch :: Int  -- ^ table size
          -> IO (Board -> MVar (DMove, String) -> IO ())
newSearch tableSize = iterative <$> newTT tableSize

mtdf :: Board        -- ^ start position
     -> ABTTable
     -> (DMove, Int) -- ^ best value with PV from last time
     -> Int          -- ^ depth
     -> Int          -- ^ lower bound
     -> Int          -- ^ upper bound
     -> IO (DMove, Int) -- ^ IO (steps to go, best value)
mtdf !b tt (!best, bestValue) depth !lb !ub = do
        best'@(_, bestV') <- alphaBeta b tt best (beta - 1, beta) depth pl

        let (lb', ub') | bestV' < beta = (lb, bestV')
                       | otherwise     = (bestV', ub)

        if lb' >= ub' then return best'
                      else mtdf b tt best' depth lb' ub'
    where
        pl = mySide b
        beta | bestValue == lb = bestValue + 1
             | otherwise       = bestValue

-- | iterative deepening
iterative :: ABTTable -> Board -> MVar (DMove, String) -> IO ()
iterative tt board mvar = search' 1 ([], 0)
    where
        search' :: Int -> (DMove, Int) -> IO ()
        search' depth gues = do
#ifdef VERBOSE
            putStrLn $ "info actual " ++ show gues
#endif
            hFlush stdout
            m@(pv,sc) <- mtdf board tt gues depth (-iNFINITY) iNFINITY
            _ <- swapMVar mvar (pv, show sc)
            search' (depth+1) m

#ifdef ENGINE
main :: IO ()
main = runAEIInterface newSearch
#endif
