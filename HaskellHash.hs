{-# LANGUAGE BangPatterns #-}
module HaskellHash (
    infoHash,
    resetHash,
    findHash,
    getHash,
    addHash
) where

import BitRepresentation (DMove, Player, playerToInt, MovePhase)
import Data.Int (Int64)
import Data.Int (Int32)
import Data.Bits
import System.IO.Unsafe
import Control.Concurrent
import qualified Data.HashTable as H

tABLEsIZE :: Num a => a
tABLEsIZE = 236250

-- TODO try bangs
data HObject = HO { hash   :: Int64
                  , best   :: (DMove, Int, Int)
                  , depth  :: Int
                  , phase  :: MovePhase
                  } deriving (Eq)

mkOnceIO :: IO a -> IO (IO a)
mkOnceIO io = do
    mv <- newEmptyMVar
    demand <- newEmptyMVar
    _ <- forkIO (takeMVar demand >> io >>= putMVar mv)
    return (tryPutMVar demand () >> readMVar mv)

hTable :: IO (H.HashTable Int32 HObject)
hTable = unsafePerformIO $ mkOnceIO $ H.new (==) (`mod` tABLEsIZE)
{-# NOINLINE hTable #-}

toInt :: Int64 -> MovePhase -> Int32
toInt h (pl,s) = fromIntegral . (`mod` tABLEsIZE) $
    h `xor` fromIntegral (playerToInt pl) -- `xor` (fromIntegral s `shift` 4)

-- TODO consider wheather is better always rewrite HT entry, or it is
--      necessary to check and compare depths
addHash :: Int64     -- ^ hash
        -> Int       -- ^ depth
        -> MovePhase -- ^ active player and steps in move
        -> (DMove, Int, Int) -- ^ (PV, lower bound, upper bound)
        -> IO ()
addHash h d mp b = do
        t <- hTable
        _ <- H.update t (toInt h mp)
                (HO { hash = h
                    , best = justNeeded b
                    , depth = d
                    , phase = mp
                    })
        return ()
    where
        justNeeded (!a:(!f):(!c):(!e):_,x,y) = ([a,f,c,e],x,y)
        justNeeded (!a:(!f):(!c):[],x,y)     = ([a,f,c],x,y)
        justNeeded (!a:(!f):[],x,y)          = ([a,f],x,y)
        justNeeded (!a:[],x,y)               = ([a],x,y)
        justNeeded ([],x,y)                  = ([],x,y)

findHash :: Int64 -> Int -> MovePhase -> IO Bool
findHash h d mp = do
        t <- hTable
        val <- H.lookup t $ toInt h mp
        case val of
            Nothing -> return False
            Just v  ->
                return $ phase v == mp
                      && depth v >= d &&  hash v == h

getHash :: Int64 -> Player -> IO (DMove, Int, Int)
getHash h pl = do
    t <- hTable
    val <- H.lookup t $ toInt h (pl,0)
    case val of
        Nothing -> return ([], -1, -1)
        Just v  -> return $ best v

resetHash :: Int -> IO ()
resetHash _ {- size -} = return ()

infoHash :: IO ()
infoHash = return ()
