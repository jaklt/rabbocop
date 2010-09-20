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
import BitRepresenation (DMove, Player, playerToInt)

foreign import ccall "clib.h info_hash"   infoHash :: IO ()
foreign import ccall "clib.h reset_hash" resetHash :: Int -> IO ()
foreign import ccall "clib.h find_hash" c_findHash :: Int64 -> Int -> Int -> IO Bool
foreign import ccall "clib.h get_hash"  c_getHash  :: Int64 -> Int -> IO (StablePtr (DMove, Int))
foreign import ccall "clib.h add_hash"  c_addHash  :: Int64 -> Int -> Int -> StablePtr (DMove, Int) -> IO ()

addHash :: Int64 -- ^ hash
        -> Int   -- ^ depth
        -> Player -- ^ active player
        -> (DMove, Int) -- ^ (PV, value)
        -> IO ()
addHash h d pl best = do
        let plNum = playerToInt pl
        isNotEmpty <- c_findHash h 0 plNum
        if isNotEmpty
            then do
                ptr' <- c_getHash h plNum
                freeStablePtr ptr'
            else
                return ()

        ptr <- newStablePtr best
        c_addHash h d plNum ptr
        return ()

getHash :: Int64 -> Player -> IO (DMove, Int)
getHash h pl = do
        let plNum = playerToInt pl
        bestPtr <- c_getHash h plNum
        deRefStablePtr bestPtr

findHash :: Int64 -> Int -> Player -> IO Bool
findHash h d pl = c_findHash h d plNum
        where plNum = playerToInt pl
