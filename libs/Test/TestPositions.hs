module Test.TestPositions
    ( testPositions
    , testEval
    ) where

import Control.Concurrent
import Control.Applicative
import Control.Monad

import Bits.BitRepresentation
import Eval.BitEval
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
moveContainAND (st:rest) moves = st `elem` moves && moveContainAND rest moves

testSearchFunction :: (Int -> IO (Board -> MVar (DMove, String) -> IO ()))
                   -> IO ()
testSearchFunction newSrch = go positionCases 1 >> putStrLn ""
    where
        testTime  = 10000000
        tableSize = 200

        go :: [TestCase] -> Int -> IO ()
        go [] _ = return ()
        go ((brd, positive, pl):rest) i = do
            let board' = parseFlatBoard pl brd
            mvar <- newMVar ([],"0")
            srch <- newSrch tableSize
            thread <- forkIO $ srch board' mvar
            threadDelay testTime
            (pv, sc) <- takeMVar mvar
            killThread thread
            let move = justOneMove board' pv
            let (board'', _) = makeMove board' move

            putStr $ show i ++ ". test"
            if positive (map show move)
                then do
                    putStrLn $ ": " ++ brd ++ " - OK"
                else do
                    putStrLn $ " - FAILED:"
                    putStrLn $ unlines $ mergeStrings
                                            [ lines $ displayBoard board'  True
                                            , boardSizedSpace
                                            , lines $ displayBoard board'' True
                                            ]
                    putStrLn $ show (pv, sc) ++ "\n"
            go rest (i+1)

testPositions :: IO ()
testPositions = do
        showHeader "testPositions"
        testSearchFunction IterativeAB.newSearch
        testSearchFunction MTDf.newSearch
        testSearchFunction MCTS.newSearch

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

        displayResult (>) (iNFINITY * iNFINITY, EmptyBoard) evaluated

-- -------------------------------------------
--            T E S T   C A S E S
-- -------------------------------------------

positionCases :: [TestCase]
positionCases =
    [ -- See my goal
      ( "[rd   rdr"
      ++ "r  rc  r"
      ++ " h    h "
      ++ "  cE    "
      ++ " M r r  "
      ++ " H    H "
      ++ "RReR RDR"
      ++ "  DC CRR]"
      , moveContainAND ["re2s", "re3s", "re4s", "rf4w"]
      , Silver)
      -- See oponents goal
    , ( "[rd   rdrr  rc  r h    h   cE     M r     H    H RReRrRDR  DC CRR]"
      , moveContainOR ["Cd1e", "Cf1w", "re2n", "re2e"]
      , Gold)
      -- Can immobilise oponent?
    , ( "[ rrrrrrrR                                                       ]"
      , \ss -> moveContainAND ["rb8w", "rc8w", "rb8s"] ss
            || moveContainAND ["rb8w", "rc8s", "rc7w"] ss
            || moveContainAND ["rb8w", "rc8w", "rd8s", "rd7w"] ss -- not immob.
            || moveContainAND ["rb8w", "rc8w", "rd8w", "rc8s"] ss --  but useful
      , Silver)
      -- Hard example from Kozeleks thesis (page 29)
    , ( "[rrrrrrrrhdcm c h Mx  x   e               Dx dxD H CE C HRRRRRRRR]"
      , moveContainAND ["Ed2n", "Ed3n", "Ed4n", "Ed5n"]
      , Gold)
    ]

-- | List of lists of decreasing positions (in eval point of view)
evalCases :: [[(String, Player)]]
evalCases =
    [ [ ( "[rrrccrrr"
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
    , [ -- same as previous, but from oponents perspective
        ( "[rrrccrrrr d  dhr hx  x R  e  E          RmxCCxHR  DRR D R R  R  "
        , Silver)
      , ( "[rrrccrrrr d  dhr hx  x   H   E    e    RRmDCCxHR   RR D R R  R  "
        , Silver)
      ]
    , [ ( "[rrrcdrhc"
        ++ "rrr emhd"
        ++ "  x  x  "
        ++ "        "
        ++ "        "
        ++ " HxE x  "
        ++ "RRM  RHR"
        ++ "RDRCCRDR", Gold) -- it's better to kill rabbit (rd3w Ee3w)
      , ( "[rrrcdrhc"
        ++ "rrr emhd"
        ++ "  x  x  "
        ++ "        "
        ++ "        "
        ++ " HxE x  "
        ++ "RRMr RHR"
        ++ "RDRCCRDR", Gold) -- than pushing him down (rd3s Ee3w)
      ]

    , [ ( "[rrrrrrrr"
        ++ "hdcm c h"
        ++ " Mx  x  "
        ++ " e      "
        ++ "        "
        ++ " DxE xD "
        ++ "H C  C H"
        ++ "RRRRRRRR]", Gold)
      , ( "[rrrrrrrr"
        ++ "hdcm c h"
        ++ " Mx  x  "
        ++ " e      "
        ++ "        "
        ++ " Dx dxDH"
        ++ "HC E CRR"
        ++ "RRRRRR  ]", Gold)
      , ( "[rrrrrrrr"
        ++ "hdcm c h"
        ++ " Mx  x  "
        ++ " e      "
        ++ "        "
        ++ " Dx dxD "
        ++ "H CE C H"
        ++ "RRRRRRRR]", Gold)
      ]
    ]
