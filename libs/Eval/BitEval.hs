{-# LANGUAGE ForeignFunctionInterface #-}
module Eval.BitEval (
    eval,
    evalImmobilised,
    forbidBoard,
    isForbidden,
    iNFINITY
) where

import Bits.BitRepresentation
import Data.Array ((!))
import Data.Int (Int64)

type CBoardFunction a = Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
                     -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
                     -> Int -> a

foreign import ccall "clib.h eval"         c_eval   :: CBoardFunction (IO Int)
foreign import ccall "clib.h forbid_board" c_forbid :: CBoardFunction (IO ())
foreign import ccall "clib.h is_forbidden" c_is_forbidden :: CBoardFunction (IO Bool)


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

isForbidden :: Board -> MovePhase -> IO Bool
isForbidden b (pl,0) | pl == mySide b = return False
                     | otherwise = boardAsCParameter c_is_forbidden b pl
isForbidden _ _ = return False


boardAsCParameter :: CBoardFunction a -> Board -> Player -> a
boardAsCParameter cFunction b player =
        cFunction (fg ! Rabbit) (fg ! Cat)   (fg ! Dog)
                  (fg ! Horse)  (fg ! Camel) (fg ! Elephant)
                  (fs ! Rabbit) (fs ! Cat)   (fs ! Dog)
                  (fs ! Horse)  (fs ! Camel) (fs ! Elephant)
                  (playerToInt player)
    where
        fg = figures b ! Gold
        fs = figures b ! Silver
