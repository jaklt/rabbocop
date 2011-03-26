{-# LANGUAGE ForeignFunctionInterface #-}
module BitEval (eval, iNFINITY) where

import BitRepresenation (Board(..), Player(..), Piece(..), playerToInt)
import Data.Array ((!))
import Data.Int (Int64)

foreign import ccall "clib.h eval"
    c_eval :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
           -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
           -> Int -> Int

iNFINITY :: Num a => a
iNFINITY = 100000

eval :: Board -> Player -> IO Int
eval b player = return $
        c_eval (fg ! Rabbit) (fg ! Cat)   (fg ! Dog)
               (fg ! Horse)  (fg ! Camel) (fg ! Elephant)
               (fs ! Rabbit) (fs ! Cat)   (fs ! Dog)
               (fs ! Horse)  (fs ! Camel) (fs ! Elephant)
               (playerToInt player)
    where
        fg = figures b ! Gold
        fs = figures b ! Silver
