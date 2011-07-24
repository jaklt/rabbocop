module Main where

import Data.Bits
import Data.List
import Control.Concurrent
import Control.Monad
import Prelude

import AlphaBeta
import Bits.BitRepresentation
import Bits.MyBits
import Eval.BitEval
import Helpers
import IterativeAB
import MCTS
import Test.TestBitRepresentation
import Test.TestPositions


testMyBits :: IO ()
testMyBits = putStrLn $ "- testMyBits\t- " ++ show basicBitIndexTest
    where basicBitIndexTest = and [bitIndex (bit i) == i | i <- [0..63]]


{- -------------------------------------------------------
 - Testing Timing
 - ------------------------------------------------------- -}

testTiming :: IO ()
testTiming = do
        showHeader "testTiming"
        {-
        mvar <- newMVar ([],"Nothing")
        search <- IterativeAB.newSearch 200
        thread <- forkIO $ search testBoard' mvar
        threadDelay 10000000
        print =<< takeMVar mvar
        killThread thread
        -- -}
        -- {-
        tables <- AlphaBeta.newHTables 200
        best <- alphaBeta testBoard' tables (-iNFINITY, iNFINITY) 8 (pl,0)
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


mm2c :: MMTree -> IO (CTree (MovePhase, String, Int, DStep))
mm2c mt = mm2c' 1 mt

mm2c' :: Int -> MMTree -> IO (CTree (MovePhase, String, Int, DStep))
mm2c' d mt = do
        tn <- nodeTreeNode mt
        let val = value tn
        let nb  = visitCount tn
        if isLeaf d tn
            then return $ CT (mp, "Leaf " ++ show val, nb, st) []
            else do
                ch  <- mapM (mm2c' (d+1)) $ children tn
                return $ CT (mp, show val, nb, st) $ sortBy cmp' ch
    where
        mp = movePhase mt
        st = step mt

        cmp' :: CTree (MovePhase, String, Int, DStep)
             -> CTree (MovePhase, String, Int, DStep) -> Ordering
        cmp' (CT (_,a,_,_) _) (CT (_,b,_,_) _) = compare a b


simpleMMTree :: Board -> IO MMTree
simpleMMTree b = do
    newLeaf <- newMVar leaf
    return $ MT
        { board = b
        , movePhase = (mySide b, 0)
        , treeNode = newLeaf
        , step = (Pass, Pass)
        }

printChildren :: MCTSTables -> MMTree -> IO ()
printChildren _ mt = do
        putStrLn "-------------------- (showing averages)"
        putStrLn "-- value visits step ucb --------------"
        tn <- nodeTreeNode mt
        childs <- forM (children tn) (\ch -> do
            val' <- nodeValue ch
            num' <- nodeVisitCount ch
            let valueUCB' = val' / fromIntegral num' :: Double
            let step' = step ch
            return (valueUCB',(val',num',step')))

        forM_ (sortBy toOrdered childs) $ \(valueUCB',(val',num',step')) ->
                putStrLn $ show val'
                         ++ "  " ++ show num'
                         ++ "  " ++ show step'
                         ++ "  " ++ show valueUCB'
        putStrLn "--------------------"
    where
        toOrdered (n1,_) (n2,_) | n1 - n2 < 0 = LT
                                | otherwise   = GT

testMCTS :: IO ()
testMCTS = do
        showHeader "starting MonteCarlo test:"
        putStrLn $ "iNFINITY:\t" ++ show (iNFINITY :: Int)

        showHeader "starting MCTS test:"
        mt <- simpleMMTree b
        tables <- MCTS.newHTables 200
        foldM_ (\mt' _ -> do
                _ <- improveTree tables mt' 0
                return mt'
                ) mt [1 .. 310 :: Int]

        -- print =<< step <$> descendByUCB1 tables mt 1
        printChildren tables mt
        print =<< mm2c mt
        printBoard b

        while $ do
            s <- getLine
            case ltrim s of
                ""    -> improveTree tables mt 0  >> return True
                'c':_ -> printChildren tables mt  >> return True
                't':_ -> mm2c mt >>= print        >> return True
                'm':_ -> constructMove tables mt 0 >>= print >> return True
                'b':_ -> printBoard b             >> return True
                'h':_ -> putStrLn "c children, t tree, b board, q quit, m construct move"
                         >> return True
                _ -> return False
    where
        b = parseFlatBoard Silver "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"

while :: IO Bool -> IO ()
while ac = flip when (while ac) =<< ac

{- ------------------------------------------------------- -}

showMove :: Show b => ([DStep], b) -> String
showMove ([],a) = show ("Empty Move",a)
showMove (ss,a) = "( " ++ foldr (\c b -> show c ++ " " ++ b) "" ss' ++ ", " ++ show a ++ ")"
    where
        ss' = foldr (\(s1,s2) b -> if s2 /= Pass then s1:s2:b else s1:b) [] ss

main :: IO ()
main = do
    testMyBits
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
