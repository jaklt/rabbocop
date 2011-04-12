module TestPositions
    ( testPositions
    , testEval
    ) where

import Control.Concurrent
import Control.Applicative
import Control.Monad

import BitRepresentation
import BitEval
import IterativeAB
import Helpers
import MCTS
import MTDf


type TestCase = (String, [String] -> Bool, Player)


displayResult :: Show a
              => (a -> a -> Bool ) -> (a, Board) -> [(a, Board)] -> IO ()
displayResult _ _ [] = putStrLn "OK"
displayResult cmp (n1, b1) (h@(n2,b2):rest) = do
        if cmp n1 n2
            then displayResult cmp h rest
            else do
                putStrLn "FAILED:"
                putStrLn $ unlines $ mergeStrings
                                    [ lines $ displayBoard b1 True
                                    , boardSizedSpace
                                    , lines $ displayBoard b2 True]
                putStrLn $ replicate 24 ' ' ++ show n1 ++ " % " ++ show n2

boardSizedSpace :: [String]
boardSizedSpace = replicate 11 "       "

mergeStrings :: [[String]] -> [String]
mergeStrings s | null s     = []
               | any null s = []
mergeStrings s = (concat $ map head s) : mergeStrings (map tail s)


moveContainOR :: [String] -> [String] -> Bool
moveContainOR [] _ = False
moveContainOR (st:rest) moves = st `elem` moves || moveContainOR rest moves

moveContainAND :: [String] -> [String] -> Bool
moveContainAND [] _ = True
moveContainAND (st:rest) moves = st `elem` moves && moveContainOR rest moves

testSearchFunction :: (Int -> Board -> MVar (DMove, Int) -> IO ()) -> IO ()
testSearchFunction srch = go positionCases 1
    where
        testTime  = 10000000
        tableSize = 236250

        go :: [TestCase] -> Int -> IO ()
        go [] _ = return ()
        go ((brd, positive, pl):rest) i = do
            let board' = parseFlatBoard pl brd
            mvar <- newMVar ([],0)
            thread <- forkIO $ srch tableSize board' mvar
            threadDelay testTime
            (pv, sc) <- takeMVar mvar
            killThread thread
            let move = justOneMove board' pv
            let (board'', move') = makeMove board' move

            if positive (map show move)
                then do
                    putStrLn $ show i ++ ". test: " ++ brd ++ " - OK"
                else do
                    putStrLn $ "\ntest - FAILED:"
                    putStrLn $ unlines $ mergeStrings
                                            [ lines $ displayBoard board'  True
                                            , boardSizedSpace
                                            , lines $ displayBoard board'' True ]
                    print (move', sc)
            go rest (i+1)

testPositions :: IO ()
testPositions = do
        showHeader "testPositions"
        -- testSearchFunction IterativeAB.search
        testSearchFunction MTDf.search
        testSearchFunction MCTS.search

-- | Test if all lists from evalCases are `eval-decreasing'
testEval :: IO ()
testEval = do
        showHeader "testEval"
        mapM_ testEval' $ zip [1..] evalCases

-- TODO show which board is considered better than which
testEval' :: (Int, [(String, Player)]) -> IO ()
testEval' (num,tests) = do
        putStr $ "\t" ++ show num ++ ". - "
        evaluated <- forM tests $ \(b,pl) -> do
            let brd = parseFlatBoard pl b
            (\e -> if pl == Gold then (1*e,brd) else ((-1)*e, brd))
                <$> eval brd pl

        displayResult (>) (iNFINITY + 1, EmptyBoard) evaluated


-- -------------------------------------------
--            T E S T   C A S E S
-- -------------------------------------------

positionCases :: [TestCase]
positionCases =
    [ -- See my goal
      ( "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"
      , moveContainOR ["re2s"]
      , Silver)
      -- See oponents goal
    , ( "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"
      , moveContainOR ["Cd1e", "Cf1w", "re2n", "re2w", "re2e"]
      , Gold)
      -- Can immobilise oponent?
    , ( "[ rrrrrrrR                                                       ]"
      , (||) <$> moveContainAND ["rb8w", "rc8w", "rb8s"]
             <*> moveContainAND ["rb8w", "rc8s", "rc7w"]
      , Silver)
      -- Hard example from Kozeleks thesis (page 29)
    , ( "[rrrrrrrrhdcm c h Mx  x   e               Dx dxD H CE C HRRRRRRRR]"
      , moveContainAND ["Ed2n", "Ed3n", "Ed4n", "Ed5n"]
      , Gold)
    ]

-- | List of lists of decreasing positions (in eval point of view)
evalCases :: [[(String, Player)]]
evalCases =
    [
        [ ( "[rrrccrrr"
          ++ "r d  dhr"
          ++ " hx  x  "
          ++ " H   E  "
          ++ "  e    R"
          ++ "RmDCCxHR"
          ++ "   RR D "
          ++ "R R  R  ", Gold) -- bot played Dc3s Hb5e Rh4n Rh5n !!
        , ( "[rrrccrrr"
          ++ "r d  dhr"
          ++ " hx  x R"
          ++ "  e  E  "
          ++ "        "
          ++ "RmxCCxHR"
          ++ "  DRR D "
          ++ "R R  R  ", Gold) -- it's definitely better to have horse
        ]
    ]
