
{-|
 - This module was used for plain testing during development. However we
 - didn't have to finish it, because our testing model had changed.
 -}

module Main (main) where

import System.Environment
import Control.Concurrent
import Control.Monad
import System.Mem (performGC)

import AEI
import Bits.BitRepresentation
import Helpers
import Eval.BitEval
import qualified MCTS
import qualified IterativeAB
import qualified MTDf


hashSize, time, maxTime :: Num a => a
quiet :: Bool

hashSize =   200 -- in MB
time     =     5 -- in seconds
maxTime  =    60 -- in minutes
quiet    =  True -- don't print board after each step?

board :: Board
board = parseFlatBoard Gold
            "[rdrccrdrrhremrhr                                RHRMERHRRDRCCRDR]"

wait :: Integer -> IO ()
wait long = mapM_ threadDelay $ map fromIntegral $ divs long
    where
        divs 0 = []
        divs n = (n `mod` d) : replicate (fromIntegral (n `div` d)) d
        d = 100000000 :: Integer

newEngine :: String -> IO SearchEngine
newEngine engName =
    case engName of
        "MCTS"        -> MCTS.newSearch hashSize
        "IterativeAB" -> IterativeAB.newSearch hashSize
        "MTDf"        -> MTDf.newSearch hashSize
        _ -> error "Unknown engine"

fight :: Int -> MVar Int -> SearchEngine -> SearchEngine -> IO ()
fight n mv srch1 srch2 = go board Gold
    where
        go :: Board -> Player -> IO ()
        go b pl
            | isEnd b = do
                putStrLn "Game ended:"
                print b
            | otherwise = do
                performGC
                mvar <- newMVar ([],"Nothing computed")
                forbidBoard board
                let (engine,n') | pl == Gold = (srch1,1)
                                | otherwise  = (srch2,n)
                let b' = b { mySide = pl }

                thread <- forkIO $ engine b' mvar
                wait $ 1000000 * time * fromIntegral n'
                (pv, val) <- takeMVar mvar
                killThread thread

                let b'' = fst $ manageJustOneMove b' pv
                putStrLn $ "\t" ++ show pl ++ "'s PV " ++ show (pv,val)
                unless quiet (print b'')

                when (pl == Silver)
                    (return . const () =<< swapMVar mv =<< eval b'' (Gold,0))

                if pv == []
                    then do
                        _ <- swapMVar mv =<< evalImmobilised b'' pl
                        putStrLn "empty move!"
                    else go b'' (opponent pl)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Starging Fight with args: " ++ show args
    putStrLn $ "And settings -     time: " ++ show (time     :: Int) ++ "\n"
            ++ "             -  maxTime: " ++ show (maxTime  :: Int) ++ "\n"
            ++ "             - hashSize: " ++ show (hashSize :: Int) ++ "\n"
    [e1,e2] <- mapM newEngine $ take 2 args
    lock <- newEmptyMVar :: IO (MVar ())
    score <- newMVar 0

    _ <- forkIO $ do
        case drop 2 args of
            "nxtime":n:_ -> fight (read n) score e1 e2
            []           -> fight 1        score e1 e2
            _            -> putStrLn "wrong argument(s)"
            -- "eqtime":depth:_ ->
        putMVar lock ()
    _ <- forkIO $ do
        wait $ (maxTime :: Integer) * 60 * 1000000
        putStrLn "time is up"
        putMVar lock ()
    _ <- readMVar lock

    res <- readMVar score
    putStr $ case res of
                _ | res > 0 -> "Gold"
                _ | res < 0 -> "Silver"
                _ -> "none"
    putStrLn $ " won " ++ show res

    putStrLn "quit"

