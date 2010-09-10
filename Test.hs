module Main where

import Data.Bits
import Data.Time.Clock
import Prelude

import MyBits
import BitRepresenation
import BitEval
import MTDf

testMyBits :: Bool
testMyBits = and [bitIndex (bit i) == i | i <- [0..63]] -- && bitIndex 222 == -1

startSilver, startGold :: String
startSilver = "ra8 rb8 rc8 rd8 re8 rf8 rg8 rh8 ha7 db7 cc7 ed7 me7 cf7 dg7 hh7 "
startGold   = "Ra1 Rb1 Rc1 Rd1 Re1 Rf1 Rg1 Rh1 Ha2 Db2 Cc2 Md2 Ee2 Cf2 Dg2 Hh2 "

main :: IO ()
main = do
    -- putStrLn $ "- testMyBits: " ++ show testMyBits
    -- putStrLn $ "- testMakeMove: " ++ show (testBoard3 == testBoard4)

    -- putStrLn.show $ foldr seq 0 [bitIndex i | i <- [0..74967296]] -- velmi pomale
    -- putStrLn.show $ generateSteps testBoard2 Gold

    -- putStrLn $ displayBoard testBoard2
    -- putStrLn.show $ alpha_beta testBoard2 ([], 0) (-iNFINITY, iNFINITY) 1 0 Gold True

    t <- getCurrentTime
    res <- search testBoard2 t Gold
    putStrLn.show $ res
    where
        testBoard  = parseBoard $ startSilver ++ startGold
        testBoard2 = parseBoard "Rb3 Ra3 Mf4 dg4 db2"
        testBoard3 = makeMove testBoard2 [ Step Rabbit Gold (bit 22) (bit 21)
                                         , Step Rabbit Gold (bit 21) 0
                                         , Step Dog Silver (bit 14) (bit 22)]
        testBoard4 = parseBoard "Ra3 Mf4 dg4 db3"

