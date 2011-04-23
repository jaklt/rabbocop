module Main where

import Data.Bits
import Control.Concurrent
import Prelude

import AlphaBeta
import Eval.BitEval
import Eval.MonteCarloEval
import Bits.BitRepresentation
import MTDf
import MCTS
import IterativeAB
import Helpers
import Bits.MyBits
import Test.TestPositions


testMyBits :: Bool
testMyBits = and [bitIndex (bit i) == i | i <- [0..63]] -- && bitIndex 222 == -1

{- -------------------------------------------------------
 - Testing Timing
 - ------------------------------------------------------- -}

testTiming :: IO ()
testTiming = do
        showHeader "testTiming"
        -- {-
        mvar <- newMVar ([],0)
        search <- IterativeAB.newSearch 236250
        thread <- forkIO $ search testBoard'  mvar
        threadDelay 10000000
        (pv, val) <- takeMVar mvar
        print (pv, val)
        killThread thread
        -- -}
        {-
        tt <- newTT 200
        best <- alphaBeta testBoard' tt [] (-iNFINITY, iNFINITY) 7 0 pl
        print best
        -- -}
        putStrLn $ displayBoard testBoard' True
    where
        pl = Silver
        testBoard' = parseFlatBoard pl
                        $ "[        "
                        ++ "     r  "
                        ++ "  x  x  "
                        ++ "  D     "
                        ++ "     d  "
                        ++ "  x  x  "
                        ++ "  R     "
                        ++ "        ]"


{- -------------------------------------------------------
 - Testing MCTS
 - ------------------------------------------------------- -}
data Show a => CTree a = CT a [CTree a]

instance Show a => Show (CTree a) where
    show = ("CalculTree\n" ++) . s 0 where
        s :: Show a => Int -> CTree a -> String
        s i (CT a subtrs) = replicate (4*i) ' ' ++ show a
                          ++ "\n" ++ (concat $ map (s (i+1)) subtrs)

mm2c :: MMTree -> CTree (MovePhase, Int, Int, (Step,Step))
mm2c mt = CT (movePhase mt, val, num, step mt) subtrees
    where
        (val,num,subtrees) = case treeNode mt of
                                Leaf -> (0,0,[])
                                tn -> (value tn, number tn, map mm2c $ children tn)

simpleMMTree :: Board -> MMTree
simpleMMTree b =
    MT { board = b
       , movePhase = (mySide b, 0)
       , treeNode = Leaf
       , step = (Pass, Pass)
       }

testMCTS :: IO ()
testMCTS = do
        let b = testBoard5
        showHeader "starting MonteCarlo test:"
        getValueByMC b (Gold, 0) >>= putStrLn.("MonteCarlo:\t"++).show
        putStrLn $ "iNFINITY:\t" ++ show (iNFINITY :: Int)

        showHeader "starting MCTS test:"
        (mt1,_) <- improveTree $ simpleMMTree b
        (mt2,_) <- improveTree mt1
        (mt3,_) <- improveTree mt2
        (mt4,_) <- improveTree mt3

        print $ mm2c mt1
        print $ mm2c mt2
        print $ mm2c mt3
        print $ mm2c mt4

        let (bst,_) = descendByUCB1 mt3
        print $ step bst

        putStrLn $ displayBoard b True

{- ------------------------------------------------------- -}

showMove :: Show b => ([(Step,Step)], b) -> String
showMove ([],a) = show ("Empty Move",a)
showMove (ss,a) = "( " ++ foldr (\c b -> show c ++ " " ++ b) "" ss' ++ ", " ++ show a ++ ")"
    where
        ss' = foldr (\(s1,s2) b -> if s2 /= Pass then s1:s2:b else s1:b) [] ss

main :: IO ()
main = do
    putStrLn $ "- testMyBits: " ++ show testMyBits
    putStrLn $ "- testMakeMove: " ++ show (testBoard3 == testBoard4)

    testPositions
    testEval
    -- testTiming
    -- testMCTS

    {-
    forM_ [1 .. 20 :: Int] $ \_ ->
        print =<< getValueByMC testBoard' (Silver, 0)

    where
        testBoard' = parseFlatBoard Silver "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"
    -}


startSilver, startGold :: String
startSilver = "ra8 rb8 rc8 rd8 re8 rf8 rg8 rh8 ha7 db7 cc7 ed7 me7 cf7 dg7 hh7 "
startGold   = "Ra1 Rb1 Rc1 Rd1 Re1 Rf1 Rg1 Rh1 Ha2 Db2 Cc2 Md2 Ee2 Cf2 Dg2 Hh2 "

testBoard, testBoard2, testBoard3, testBoard4, testBoard5
    :: Board
testBoard  = parseBoard Gold $ startSilver ++ startGold
testBoard2 = parseBoard Gold "Rb3 Ra3 Mf4 dg4 db2 re8"
testBoard3 = fst $ makeMove testBoard2
                             [ Step Rabbit Gold (bit 22) (bit 21)
                             , Step Dog Silver (bit 14) (bit 22)]
testBoard4 = parseBoard Gold "Ra3 Mf4 dg4 db3 re8"
testBoard5 = parseBoard Gold "Rc1 Rf1 rf8"
