module Main where

import Control.Monad (forM_)
import Data.Bits
import Prelude

import MyBits
import BitEval
import BitRepresenation
import MTDf

import Hash

testMyBits :: Bool
testMyBits = and [bitIndex (bit i) == i | i <- [0..63]] -- && bitIndex 222 == -1

testHash :: IO Bool
testHash = do
        arr <- jajxArr

        let ss = [(Step pie pl from to,i) | i    <- [6,5..0]
                                          , pie  <- pieces,     pl <- players
                                          , from <- [63,62..0], to <- [63,62..0]]

        forM_ ss (\(s,i) -> do
                    let b = fst $ makeStep (createBoard []) s
                    t <- jajxFind arr (hash b) i
                    if t
                        then do
                            (m,v) <- jajxGet arr (hash b)
                            jajxAdd arr (hash b) i (v+1) (Pass:m)
                        else jajxAdd arr (hash b) i (i*531 `div` 26000) [Pass]
                 )
        return True

testHash2 :: IO Bool
testHash2 = do
        resetHash

        let ss = [(Step pie pl from to,i) | i    <- [400,399..0]
                                          , pie  <- pieces,     pl <- players
                                          , from <- [63,62..0], to <- [63,62..0]]

        forM_ ss (\(s,i) -> do
                    let b = fst $ makeStep (createBoard []) s
                    t <- findHash (hash b) i
                    if t
                        then do
                            v <- getHash (hash b)
                            addHash (hash b) i (v+1)
                        else addHash (hash b) i (i*531 `div` 26000)
                 )
        resetHash
        return True

startSilver, startGold :: String
startSilver = "ra8 rb8 rc8 rd8 re8 rf8 rg8 rh8 ha7 db7 cc7 ed7 me7 cf7 dg7 hh7 "
startGold   = "Ra1 Rb1 Rc1 Rd1 Re1 Rf1 Rg1 Rh1 Ha2 Db2 Cc2 Md2 Ee2 Cf2 Dg2 Hh2 "

showMove :: (Show a, Show b) => ([a], b) -> String
showMove ([],a) = show ("Empty Move",a)
showMove (ss,a) = "( " ++ foldr (\c b -> show c ++ " " ++ b) "" ss ++ ", " ++ show a ++ ")"

main :: IO ()
main = do
    putStrLn $ "- testMyBits: " ++ show testMyBits
    putStrLn $ "- testMakeMove: " ++ show (testBoard3 == testBoard4)
    -- hashResult <- testHash
    -- putStrLn $ "- testHash: " ++ show hashResult

    arr <- jajxArr

    jajxAdd arr (hash testBoard) 1 1000 [Pass, Pass]
    res <- jajxGet arr (hash testBoard)
    putStrLn.show $ res

    -- putStrLn $ displayBoard testBoard2 True
    -- putStrLn.show $ generateSteps testBoard2 Gold

    -- putStrLn.show $ alpha_beta testBoard2 ([], 0) (-iNFINITY, iNFINITY) 1 0 Gold True

    putStrLn $ displayBoard testBoard2 True
    res <- search testBoard2 Gold 20
    putStrLn $ showMove res

    {-
    putStrLn $ displayBoard testBoard5 True
    putStrLn $ show $ eval testBoard5 Gold

    putStrLn $ displayBoard testBoard2 True
    res <- alpha_beta testBoard2 ([], 2100) (-iNFINITY, iNFINITY) 10 0 Gold True
    putStrLn $ showMove res
    -}

    where
        testBoard  = parseBoard $ startSilver ++ startGold
        testBoard2 = parseBoard "Rb3 Ra3 Mf4 dg4 db2 re8"
        testBoard3 = fst $ makeMove testBoard2
                                         [ Step Rabbit Gold (bit 22) (bit 21)
                                         , Step Dog Silver (bit 14) (bit 22)]
        testBoard4 = parseBoard "Ra3 Mf4 dg4 db3 re8"
        testBoard5 = parseBoard "Rc1 rf8"
