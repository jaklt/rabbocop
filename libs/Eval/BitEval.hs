{-# LANGUAGE ForeignFunctionInterface #-}
module Eval.BitEval
    ( eval
    , evalImmobilised
    , forbidBoard
    , isForbidden
    , evalStep
    , iNFINITY
    ) where

import Data.Array ((!))
import Data.Bits ((.&.))
import Data.Int (Int64)
import Bits.MyBits (bitCount)
import Bits.BitRepresentation

type CBoardFunction a = Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
                     -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
                     -> Int -> a

foreign import ccall "clib.h eval"         c_eval   :: Int -> CBoardFunction (IO Int)
foreign import ccall "clib.h forbid_board" c_forbid :: CBoardFunction (IO ())
foreign import ccall "clib.h is_forbidden"
                           c_is_forbidden :: CBoardFunction (IO Bool)


iNFINITY :: Num a => a
iNFINITY = 100000

eval :: Board -> MovePhase -> IO Int
eval b (pl,sc) = boardAsCParameter (c_eval (4-sc)) b pl

-- | Evaluate board, where given player is immobilised.
-- Final result is also from this player perspective.
evalImmobilised :: Board -> Player -> IO Int
evalImmobilised b pl = do
    e <- eval b (pl,4)
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

-- | Evaluate step. If it is not the first step in move, give preceeding
-- step also.
evalStep :: Board
         -> MovePhase
         -> DStep  -- ^ Previous step in the same move or (Pass,Pass)
         -> DStep  -- ^ Step to make
         -> Double
evalStep _ _ _ (Pass, _) = 0
evalStep b (pl,_) s1@(s11,s12) s2@(s21,s22)
        | s11 == Pass = standartBonus
        | otherwise   = standartBonus + extendedBonus
    where
        -- my stronger piece is moving opponents weaker piece
        (Step _ _ from1 to1, _) = case s1 of
                                    (Step _ pl' _ _, _) | pl == pl' -> s1
                                    _ -> (s12,s11)
        (Step pie2 pl2 from2 to2, _) = case s2 of
                                    (Step _ pl' _ _, _) | pl == pl' -> s2
                                    _ -> (s22,s21)

        -- Count possible bonuses
        standartBonus = strongPieceBonus + pushPullBonus + killBonus
                      + suicidePenalty + goalBonus
        extendedBonus
            | isContinue && to2 == from1 = -1.0  -- inverse step penalty
            | isContinue =  0.8  -- continuity bonus
            | isNear     =  0.4
            | otherwise  =  0.0
        isContinue = to1 == from2
        isNear = abs ((to1 `div` 8) - (from2 `div` 8))
               + abs ((to1 `mod` 8) - (from2 `mod` 8))
               <= 3     -- locality bound

        -- big bonuses
        killBonus      | any opponentsPiece died = 1.5
                       | otherwise               = 0.0
        suicidePenalty | any myPieces died = -1.5
                       | otherwise         = 0.0
        died = snd $ makeDStep b s2

        goalBonus | pie2 /= Rabbit = 0.0
                  | otherwise =
                        (2.0 *) . fromIntegral $ bitCount . (to2 .&.) $
                                case pl2 of
                                    Gold   -> 0xffff000000000000
                                    Silver -> 0x000000000000ffff

        -- smal bonuses
        pushPullBonus | s22 /= Pass = 0.5
                      | otherwise   = 0.0
        strongPieceBonus = case pie2 of
                            Elephant -> 0.5
                            Camel    -> 0.30
                            Horse    -> 0.20
                            _        -> 0.0

        opponentsPiece (Step _ pl' _ _) = pl' /= pl
        opponentsPiece _ = False
        myPieces       (Step _ pl' _ _) = pl' == pl
        myPieces _ = False

        -- TODO try to rewrite this in C
