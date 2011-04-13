{-# LANGUAGE ForeignFunctionInterface #-}
module BitEval (
    eval,
    evalImmobilised,
    forbidBoard,
    iNFINITY
) where

import BitRepresentation (Board(..), Player(..), Piece(..), playerToInt, (<#>))
import Data.Array ((!))
import Data.Int (Int64)

type CBoardFunction a = Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
                     -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
                     -> Int -> IO a

foreign import ccall "clib.h eval"           c_eval :: CBoardFunction Int
foreign import ccall "clib.h forbid_board" c_forbid :: CBoardFunction ()


iNFINITY :: Num a => a
iNFINITY = 100000

eval :: Board -> Player -> IO Int
eval = boardAsCParameter c_eval

evalImmobilised :: Board -> Player -> IO Int
evalImmobilised b pl = do
    e <- eval b pl
    if e >= iNFINITY || e <= -iNFINITY
        then return e
        else return $ pl <#> Silver * iNFINITY

forbidBoard :: Board -> IO ()
forbidBoard b = boardAsCParameter c_forbid b (mySide b)

boardAsCParameter :: CBoardFunction a -> Board -> Player -> IO a
boardAsCParameter cFunction b player =
        cFunction (fg ! Rabbit) (fg ! Cat)   (fg ! Dog)
                  (fg ! Horse)  (fg ! Camel) (fg ! Elephant)
                  (fs ! Rabbit) (fs ! Cat)   (fs ! Dog)
                  (fs ! Horse)  (fs ! Camel) (fs ! Elephant)
                  (playerToInt player)
    where
        fg = figures b ! Gold
        fs = figures b ! Silver
