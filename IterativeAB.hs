{-# LANGUAGE BangPatterns #-}
module IterativeAB (search) where

import AlphaBeta
import BitRepresentation (Board(..), DMove)
import BitEval (iNFINITY)

import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)


-- | iterative deepening
search :: Int -> Board -> MVar (DMove, Int) -> IO ()
search tableSize board mvar = search' 1 ([], 0) =<< newTT tableSize
    where
        search' :: Int -> (DMove, Int) -> ABTTable -> IO ()
        search' !depth best tt = do
            -- putStrLn $ "info actual " ++ show best
            hFlush stdout
            m <- alphaBeta board tt (fst best) (-iNFINITY, iNFINITY)
                           depth 0 (mySide board)
            _ <- m `seq` swapMVar mvar m
            search' (depth+1) m tt
