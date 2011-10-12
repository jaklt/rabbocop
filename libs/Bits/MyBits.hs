{-# LANGUAGE ForeignFunctionInterface #-}
module Bits.MyBits
    ( bitIndex
    , bitCount
    , rightMostBit
    , bits
    , bitNot
    ) where

import Data.Bits ((.&.))
import Data.Int  (Int64)

foreign import ccall "clib.h bit_index" bitIndex :: Int64 -> Int
foreign import ccall "clib.h bit_count" bitCount :: Int64 -> Int

rightMostBit :: Int64 -> Int64
rightMostBit n = n .&. (-n)
{-# INLINE rightMostBit #-}

-- | Spread given number into list of its bits.
bits :: Int64 -> [Int64]
bits 0 = []
bits n = rightMostBit n : bits (n .&. (n-1))

-- | For 0 returns 1, otherwise returns 0.
bitNot :: Num a => Int64 -> a
bitNot 0 = 1
bitNot _ = 0
