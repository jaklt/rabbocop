module Eval (eval, iNFINITY) where

import BitRepresenation
import MyBits
import Data.Bits

iNFINITY :: Int
iNFINITY = 26000 -- az petinasobek kazdy figury

pieceValue :: Piece -> Int
pieceValue Elephant = 800
pieceValue Camel    = 600
pieceValue Horse    = 500
pieceValue Dog      = 400
pieceValue Cat      = 300
pieceValue Rabbit   = 100

pieceToInt :: Piece -> Int
pieceToInt k = index (Elephant, Rabbit) k

{-
.... ....   0 = ....  8 = |...
.... ....   1 = ...|  9 = |..|
..+. .+..   2 = ..|.  a = |.|.
.... ....   3 = ..||  b = |.||
.... ....   4 = .|..  c = ||..
..+. .+..   5 = .|.|  d = ||.|
.... ....   6 = .||.  e = |||.
.... ....   7 = .|||  f = ||||
-}

center1 = 0x0000001818000000
center2 = 0x00003c24243c0000
center3 = 0x007e424242427e00
center4 = 0xff818181818181ff

upperSide  = 0xff00000000000000
bottomSide = 0x00000000000000ff

aroundTraps = 0x00245a24245a2400

{-# INLINE eval #-}
eval :: Board -> Int
eval b = sum [ ((position pl m v) + (trapsControl m v) + (winOrLose pl pie m))
                 * (if pl == Gold then 1 else -1)
             | pl <- players, pie <- pieces, let m = board b ! pl ! pie
                                                 v = pieceValue pie]
    where
        position pl m v = v * (bitCount (center4 .&. m) + 2* bitCount (center3 .&. m)
                              + 3* bitCount (center2 .&. m) + 4* bitCount (center1 .&. m))

        trapsControl m v = v * bitCount (m .&. aroundTraps)

        winOrLose pl Rabbit m | rabbitWon  /= 0 =  iNFINITY
                              | bitCount m == 0 = -iNFINITY
                              | otherwise       = 0
                where
                    rabbitWon = (if pl == Gold then upperSide else bottomSide) .&. m
        winOrLose _ _ _ = 0
