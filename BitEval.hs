{-# LANGUAGE ForeignFunctionInterface #-}
module BitEval (eval, iNFINITY) where

import BitRepresenation (Board(..), Player(..), Piece(..))
import Data.Array ((!))
import Data.Int (Int64)

foreign import ccall "clib.h eval"
    c_eval :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
           -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int

iNFINITY :: Num a => a
iNFINITY = 100000

eval :: Board -> Player -> IO Int
eval b player = do
        e <- (c_eval (fg ! Rabbit) (fg ! Cat)   (fg ! Dog)
                     (fg ! Horse)  (fg ! Camel) (fg ! Elephant)
                     (fs ! Rabbit) (fs ! Cat)   (fs ! Dog)
                     (fs ! Horse)  (fs ! Camel) (fs ! Elephant))
        return $! if mySide b == Gold then e else -e
    where
        fg = figures b ! Gold
        fs = figures b ! Silver
