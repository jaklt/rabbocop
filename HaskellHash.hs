{-# LANGUAGE BangPatterns #-}
module HaskellHash (
    infoHash,
    resetHash,
    findHash,
    getHash,
    addHash
) where

import Data.Int (Int64)
import BitRepresenation (DMove, Player, playerToInt)
import Control.Monad (when)

import Data.Int (Int32)
import Data.Bits
import System.IO.Unsafe
import Control.Concurrent
import qualified Data.HashTable as H

tABLEsIZE :: Num a => a
-- tABLEsIZE = 23625
tABLEsIZE = 23625000

data HObject = HO { hash   :: Int64
                  , best   :: (DMove, Int)
                  , depth  :: Int
                  , player :: Player
                  -- TODO add depthFromTopModedByFour
                  } deriving (Eq)

mkOnceIO :: IO a -> IO (IO a)
mkOnceIO io = do
    mv <- newEmptyMVar
    demand <- newEmptyMVar
    forkIO (takeMVar demand >> io >>= putMVar mv)
    return (tryPutMVar demand () >> readMVar mv)

hTable :: IO (H.HashTable Int32 HObject)
hTable = unsafePerformIO $ mkOnceIO $ H.new (==) (`mod` tABLEsIZE)
{-# NOINLINE hTable #-}

toInt :: Int64 -> Player -> Int32
toInt h pl = fromIntegral $ (h `xor` fromIntegral (playerToInt pl)) `mod` tABLEsIZE

addHash :: Int64 -- ^ hash
        -> Int   -- ^ depth
        -> Player -- ^ active player
        -> (DMove, Int) -- ^ (PV, value)
        -> IO ()
addHash h d pl b = do
        t <- hTable
        let key = toInt h pl
        inside <- findHash h d pl
        when (not inside) (do
            _ <- H.update t key (HO { hash = h
                                    , best = justNeeded b
                                    , depth = d
                                    , player = pl
                                    })
            return ())
    where
        justNeeded (!a:(!f):(!c):(!e):_,val) = ([a,f,c,e],val)
        justNeeded (!a:(!f):(!c):[],val)     = ([a,f,c],val)
        justNeeded (!a:(!f):[],val)          = ([a,f],val)
        justNeeded (!a:[],val)               = ([a],val)
        justNeeded ([],val)                  = ([],val)

findHash :: Int64 -> Int -> Player -> IO Bool
findHash h d pl = do
        t <- hTable
        let key = toInt h pl
        val <- H.lookup t key
        case val of
            Nothing -> return False
            Just v  ->
                return $ player v == pl && depth v >= d && hash v == h

getHash :: Int64 -> Player -> IO (DMove, Int)
getHash h pl = do
    t <- hTable
    val <- H.lookup t (toInt h pl)
    case val of
        Nothing -> return ([], -1)
        Just v  -> return $ best v

resetHash :: Int -> IO ()
resetHash size = return ()

infoHash :: IO ()
infoHash = return ()
