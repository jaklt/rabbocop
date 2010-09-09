{-# LANGUAGE ForeignFunctionInterface #-}
module MyBits (bitIndex, bitCount, rightMostBit, bits) where

import Data.Bits ((.&.), xor)
import Data.Int  (Int64)
-- import Foreign
-- import Foreign.C.Types

foreign import ccall "clib.h bit_index" bitIndex :: Int64 -> Int
foreign import ccall "clib.h bit_count" bitCount :: Int64 -> Int

{-# INLINE rightMostBit #-}
rightMostBit :: Int64 -> Int64
rightMostBit n = n .&. (-n)

bits :: Int64 -> [Int64]
bits 0 = []
bits n = rmb : bits (n `xor` rmb) where rmb = rightMostBit n
