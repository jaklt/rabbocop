module Main where

import Data.Bits
import Data.List
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Prelude


import AlphaBeta
import Bits.BitRepresentation
import Eval.BitEval
import Eval.MonteCarloEval
import Helpers
import MTDf
import MCTS
import IterativeAB
import Bits.MyBits
import Test.TestPositions
import Test.TestBitRepresentation


testMyBits :: Bool
testMyBits = and [bitIndex (bit i) == i | i <- [0..63]]


{- -------------------------------------------------------
 - Testing Timing
 - ------------------------------------------------------- -}

testTiming :: IO ()
testTiming = do
        showHeader "testTiming"
        {-
        mvar <- newMVar ([],0)
        search <- IterativeAB.newSearch 200
        thread <- forkIO $ search testBoard'  mvar
        threadDelay 10000000
        (pv, val) <- takeMVar mvar
        print (pv, val)
        killThread thread
        -- -}
        -- {-
        tt <- AlphaBeta.newTT 200
        best <- alphaBeta testBoard' tt [] (-iNFINITY, iNFINITY) 7 pl
        print best
        -- -}
        printBoard testBoard'
    where
        pl = Gold
        testBoard' = parseFlatBoard pl
                        $ "[r r cr r"
                        ++ "r    rdr"
                        ++ " dxEcxh "
                        ++ "        "
                        ++ " h e C  "
                        ++ "HmxR x R"
                        ++ "RR rCRD "
                        ++ "RDR    R]"


{- -------------------------------------------------------
 - Testing MCTS
 - ------------------------------------------------------- -}
data Show a => CTree a = CT a [CTree a]

instance Show a => Show (CTree a) where
    show = ("CalculTree\n" ++) . s 0 where
        s :: Show a => Int -> CTree a -> String
        s i (CT a subtrs) = replicate (4*i) ' ' ++ show a
                          ++ "\n" ++ (concat $ map (s (i+1)) subtrs)


mm2c :: MMTree -> IO (CTree (MovePhase, String, Int, (Step,Step)))
mm2c mt = do
        tn <- nodeTreeNode mt
        case tn of
            Leaf -> return $ CT (mp, "Leaf", 0, st) [] 
            _    -> do
                ch  <- mapM mm2c $ children tn
                val <- nodeValue mt
                nb  <- nodeVisitCount mt
                return $ CT (mp, show val, nb, st) ch 
    where
        mp = movePhase mt
        st = step mt


simpleMMTree :: Board -> IO MMTree
simpleMMTree b = do
    newLeaf <- newMVar Leaf
    return $ MT
        { board = b
        , movePhase = (mySide b, 0)
        , treeNode = newLeaf
        , step = (Pass, Pass)
        }

{-
printChildrens :: MMTree -> IO ()
printChildrens mt = do
        putStrLn "-------------------- (showing averages)"
        forM_ (sortBy (toOrdered) $ children $ treeNode mt) (\ch -> do
                putStrLn $ show (val ch, num ch)
                         ++ "  " ++ show (step ch)
                         ++ "  " ++ show (valueUCB ch (visitCount $ treeNode mt)))
        putStrLn "--------------------"
    where
        toOrdered n1 n2 | (val n1) * fromIntegral (num n2)
                           - (val n2) * fromIntegral (num n2) < 0 = LT
                        | otherwise = GT
        val = value  . treeNode
        num = visitCount . treeNode
-}


testMCTS :: IO ()
testMCTS = do
        showHeader "starting MonteCarlo test:"
        getValueByMC b (Gold, 0) >>= putStrLn.("MonteCarlo:\t"++).show
        putStrLn $ "iNFINITY:\t" ++ show (iNFINITY :: Int)

        showHeader "starting MCTS test:"
        tree <- simpleMMTree b
        tt   <- MCTS.newTT 200
        mt' <- foldM (\mt _ -> do
                _ <- improveTree mt tt 0
                return mt
                ) tree [1 .. 310 :: Int]

        print =<< step <$> descendByUCB1 mt'
        {-
        printChildrens mt'
        -- print =<< mm2c mt'
        (mt'',_) <- improveTree mt'
        print.step.fst.descendByUCB1 $ mt''

        printChildrens mt''
        -}

        printBoard b
    where
        b = parseFlatBoard Silver "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"

{- ------------------------------------------------------- -}

showMove :: Show b => ([(Step,Step)], b) -> String
showMove ([],a) = show ("Empty Move",a)
showMove (ss,a) = "( " ++ foldr (\c b -> show c ++ " " ++ b) "" ss' ++ ", " ++ show a ++ ")"
    where
        ss' = foldr (\(s1,s2) b -> if s2 /= Pass then s1:s2:b else s1:b) [] ss

main :: IO ()
main = do
    putStrLn $ "- testMyBits\t- " ++ show testMyBits
    testSteps
    testEval

    testPositions
    -- testTiming
    -- testMCTS

    {-
    let n = 500 :: Int
    print.(`div`n).sum =<< mapM (\_ -> getValueByMC testBoard' (Silver, 0)) [1 .. n]
    printBoard testBoard'

    where
        testBoard' = parseFlatBoard Silver "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"
    -}


startSilver, startGold :: String
startSilver = "ra8 rb8 rc8 rd8 re8 rf8 rg8 rh8 ha7 db7 cc7 ed7 me7 cf7 dg7 hh7 "
startGold   = "Ra1 Rb1 Rc1 Rd1 Re1 Rf1 Rg1 Rh1 Ha2 Db2 Cc2 Md2 Ee2 Cf2 Dg2 Hh2 "

testBoard, testBoard2, testBoard5 :: Board
testBoard  = parseBoard Gold $ startSilver ++ startGold
testBoard2 = parseBoard Gold "Rb3 Ra3 Mf4 dg4 db2 re8"
testBoard5 = parseBoard Gold "Rc1 Rf1 rf8"
