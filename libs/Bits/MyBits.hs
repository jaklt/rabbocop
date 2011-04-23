{-# LANGUAGE ForeignFunctionInterface #-}
module Bits.MyBits (bitIndex, bitCount, rightMostBit, bits) where

import Data.Bits ((.&.))
import Data.Int  (Int64)

foreign import ccall "clib.h bit_index" bitIndex :: Int64 -> Int
foreign import ccall "clib.h bit_count" bitCount :: Int64 -> Int

rightMostBit :: Int64 -> Int64
rightMostBit n = n .&. (-n)
{-# INLINE rightMostBit #-}

bits :: Int64 -> [Int64]
bits 0 = []
bits n = rightMostBit n : bits (n .&. (n-1))
