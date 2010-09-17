{-# LANGUAGE ForeignFunctionInterface #-}
module Hash (
    infoHash,
    resetHash,
    findHash,
    getHash,
    addHash
) where

import Data.Int (Int64)
import Foreign.StablePtr
import BitRepresenation (Move)

foreign import ccall "clib.h info_hash"   infoHash :: IO ()
foreign import ccall "clib.h reset_hash" resetHash :: IO ()
foreign import ccall "clib.h find_hash"   findHash :: Int64 -> Int -> IO Bool
foreign import ccall "clib.h get_hash"  c_getHash  :: Int64 -> IO (StablePtr (Move, Int))
foreign import ccall "clib.h add_hash"  c_addHash  :: Int64 -> Int -> StablePtr (Move, Int) -> IO ()

addHash :: Int64 -- ^ hash
        -> Int   -- ^ depth
        -> (Move, Int) -- ^ (PV, value)
        -> IO ()
addHash h d best = do
        isNotEmpty <- findHash h 0
        if isNotEmpty
            then do
                ptr' <- c_getHash h
                freeStablePtr ptr'
            else
                return ()

        ptr <- newStablePtr best
        c_addHash h d ptr
        return ()

getHash :: Int64 -> IO (Move, Int)
getHash h = do
        bestPtr <- c_getHash h
        best <- deRefStablePtr bestPtr
        return best
