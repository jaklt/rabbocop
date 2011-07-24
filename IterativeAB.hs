{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
#ifdef ENGINE
module Main (main) where
import AEI
#else
module IterativeAB (newSearch) where
#endif

import AlphaBeta
import Bits.BitRepresentation (Board(..), DMove)
import Eval.BitEval (iNFINITY)

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, swapMVar)
#ifdef VERBOSE
import System.IO (hFlush, stdout)
#endif


newSearch :: Int  -- ^ table size
          -> IO (Board -> MVar (DMove, String) -> IO ())
newSearch tableSize = iterative <$> newHTables tableSize

-- | iterative deepening
iterative :: ABTables -> Board -> MVar (DMove, String) -> IO ()
iterative tt board mvar = search' 1 ([], 0)
    where
        search' :: Int -> (DMove, Int) -> IO ()
        search' !depth best = do
#ifdef VERBOSE
            putStrLn $ "info actual " ++ show best
            hFlush stdout
#endif
#ifdef WINDOW
            let val = snd best
            let win = (val-WINDOW, val+WINDOW)
            n@(_,val') <- alphaBeta board tt win depth (mySide board,0)

            m@(pv,sc) <-
                if val' < fst win || snd win < val'
                    then alphaBeta board tt (-iNFINITY, iNFINITY)
                                   depth (mySide board, 0)
                    else return n
#else
            m@(pv,sc) <- alphaBeta board tt (-iNFINITY, iNFINITY)
                                   depth (mySide board, 0)
#endif
            _ <- m `seq` swapMVar mvar (pv, show sc ++ " depth:" ++ show depth)
            search' (depth+1) m

#ifdef ENGINE
main :: IO ()
main = runAEIInterface newSearch
#endif
