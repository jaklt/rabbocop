{-# OPTIONS -fno-warn-deprecated-flags #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Hash (
    resetHash,
    findHash,
    getHash,
    addHash,

    -- testing Storable Hash implementation
    jajxAdd,
    jajxGet,
    jajxArr,
    jajxFind,
) where

import Data.Int (Int64)
import Data.Array.Storable
import qualified Foreign.Storable as FS
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr (nullPtr)
import Foreign.StablePtr
import BitRepresenation (Move)

#include "clib.h"

foreign import ccall "clib.h reset_hash" resetHash :: IO ()
foreign import ccall "clib.h find_hash" findHash :: Int64 -> Int -> IO Bool
foreign import ccall "clib.h get_hash"   getHash :: Int64 -> IO Int
foreign import ccall "clib.h add_hash"   addHash :: Int64 -> Int -> Int -> IO ()

data Record = Record { hash  :: Int64
                     , value :: Int
                     , depth :: Int
                     , pv    :: (StablePtr Move)
                     , used  :: Bool }

-- | To be able to do fast mutable array, which don't reallocate after
--   any changes.
--
-- inspiration from: http://therning.org/magnus/archives/315
instance FS.Storable Record where
    sizeOf _ = (#size CRecord)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        hash' <- (#peek CRecord, hash) ptr
        value' <- (#peek CRecord, value) ptr
        depth' <- (#peek CRecord, depth) ptr
        pv' <- (#peek CRecord, pv) ptr
        used' <- (#peek CRecord, used) ptr
        return Record { hash=hash', value=value', depth=depth', pv=pv', used=used' }
    poke ptr (Record hash' value' depth' pv' used') = do
        (#poke CRecord, hash) ptr hash'
        (#poke CRecord, value) ptr value'
        (#poke CRecord, depth) ptr depth'
        (#poke CRecord, pv) ptr pv'
        (#poke CRecord, used) ptr used'

hASH_SIZE :: Int
hASH_SIZE = 131072

computeIndex :: Int64 -> Int
computeIndex i = fromIntegral $ (fromIntegral i) `mod` hASH_SIZE

nullStablePtr :: StablePtr a
nullStablePtr = castPtrToStablePtr nullPtr

jajxArr :: IO (StorableArray Int Record)
jajxArr = newArray (0,hASH_SIZE-1)
            (Record { hash = 0, value = 0, depth = 0
                    , pv = nullStablePtr, used = False})

jajxAdd :: StorableArray Int Record
        -> Int64 -- ^ hash
        -> Int   -- ^ depth
        -> Int   -- ^ value
        -> Move  -- ^ PV to save
        -> IO ()
jajxAdd arr h d v m = do
        -- TODO freeStablePtr
        ptr <- newStablePtr m
        writeArray arr (computeIndex h)
            (Record { hash = h, value = v, depth = d, pv = ptr, used = True})
        return ()

jajxGet :: StorableArray Int Record -> Int64 -> IO (Move, Int)
jajxGet arr h = do
        rec <- readArray arr (computeIndex h)
        mov <- deRefStablePtr $ pv rec
        return (mov, value rec)

jajxFind :: StorableArray Int Record
         -> Int64  -- ^ hash
         -> Int    -- ^ depth
         -> IO Bool
jajxFind arr h d = do
        rec <- readArray arr (computeIndex h)
        return $ used rec && depth rec <= d && hash rec == h
