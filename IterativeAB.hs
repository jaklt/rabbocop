{-# LANGUAGE BangPatterns #-}
module IterativeAB (search) where

import Control.Concurrent (MVar, swapMVar)
import System.IO (hFlush, stdout)
import AlphaBeta
import BitRepresenation (Board(..), DMove)
import BitEval (iNFINITY)
import Hash (infoHash)

-- | iterative deepening
search :: Board -> MVar (DMove, Int) -> IO ()
search board mvar = search' 1 ([], 0)
    where
        search' :: Int -> (DMove, Int) -> IO ()
        search' !depth best = do
            putStrLn $ "info actual " ++ show best
            infoHash
            hFlush stdout
            m <- alphaBeta board (fst best) (-iNFINITY, iNFINITY)
                           depth 0 (mySide board)
            _ <- m `seq` swapMVar mvar m
            search' (depth+1) m
