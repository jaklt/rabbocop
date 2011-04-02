{-# LANGUAGE ForeignFunctionInterface #-}
module BitEval (
    eval,
    evalImmobilised,
    iNFINITY
) where

import BitRepresentation (Board(..), Player(..), Piece(..), playerToInt, (<#>))
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

evalImmobilised :: Board -> Player -> IO Int
evalImmobilised b pl = do
    e <- eval b pl
    if e >= iNFINITY || e <= -iNFINITY
        then return e
        else return $ pl <#> Silver * iNFINITY
