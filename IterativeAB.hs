{-# LANGUAGE BangPatterns #-}
module IterativeAB (newSearch) where

import AlphaBeta
import BitRepresentation (Board(..), DMove)
import BitEval (iNFINITY)

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)


newSearch :: Int  -- ^ table size
          -> IO (Board -> MVar (DMove, Int) -> IO ())
newSearch tableSize = iterative <$> newTT tableSize

-- | iterative deepening
iterative :: ABTTable -> Board -> MVar (DMove, Int) -> IO ()
iterative tt board mvar = search' 1 ([], 0)
    where
        search' :: Int -> (DMove, Int) -> IO ()
        search' !depth best = do
            -- putStrLn $ "info actual " ++ show best
            hFlush stdout
            m <- alphaBeta board tt (fst best) (-iNFINITY, iNFINITY)
                           depth 0 (mySide board)
            _ <- m `seq` swapMVar mvar m
            search' (depth+1) m
