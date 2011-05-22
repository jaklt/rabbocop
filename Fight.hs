module Main (main) where

import System.Environment
import AEI (SearchEngine)
import Control.Concurrent

import Eval.BitEval (forbidBoard)
import Bits.BitRepresentation
import Helpers
import qualified MCTS
import qualified IterativeAB
import qualified MTDf

hashSize, time, maxTime :: Num a => a
hashSize = 200 -- in MB
time     = 3   -- in seconds
maxTime  = 20  -- in minutes

board :: Board
board = parseFlatBoard Gold
            "[rdrccrdrrhremrhr                                RHRMERHRRDRCCRDR]"


newEngine :: String -> IO SearchEngine
newEngine engName =
    case engName of
        "MCTS"        -> MCTS.newSearch hashSize
        "IterativeAB" -> IterativeAB.newSearch hashSize
        "MTDf"        -> MTDf.newSearch hashSize
        _ -> error "Unknown engine"

fight :: Int -> SearchEngine -> SearchEngine -> IO ()
fight n srch1 srch2 = go board Gold
    where
        go :: Board -> Player -> IO ()
        go b pl
            | isEnd b = do
                putStrLn "Game ended:"
                print b
            | otherwise = do
                mvar <- newMVar ([],0)
                forbidBoard board
                let (engine,n') | pl == Gold = (srch1,1)
                                | otherwise  = (srch2,n)
                let b' = b { mySide = pl }

                thread <- forkIO $ engine b' mvar
                threadDelay (3000000 * time * n' `div` 4)
                (pv, val) <- takeMVar mvar
                killThread thread

                let b'' = fst $ manageJustOneMove b' pv
                putStrLn $ "\t" ++ show pl ++ "'s PV " ++ show (pv,val)
                print b''

                if pv == []
                    then putStrLn "empty move!"
                    else go b'' (oponent pl)

main :: IO ()
main = do
    args <- getArgs
    [e1,e2] <- mapM newEngine $ take 2 args
    lock <- newEmptyMVar :: IO (MVar ())

    thread <- forkIO $ do
        case drop 2 args of
            "nxtime":n:_ -> fight (read n) e1 e2
            _ ->            fight 1 e1 e2
            -- "eqtime":depth:_ ->
        putMVar lock ()
    _ <- forkIO $ do
        threadDelay (maxTime * 60 * 1000000)
        killThread thread
        putMVar lock ()
    _ <- readMVar lock
    putStrLn "quit"

