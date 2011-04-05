{-# LANGUAGE BangPatterns #-}
module HaskellHash (
    TTable(..),
    findHash,
    getHash,
    addHash
) where

import Data.Int (Int32)
import qualified Data.HashTable as H


data TTable e o i
    = TT { table     :: H.HashTable Int32 o
         , getEntry  :: o -> e
         , isValid   :: o -> i -> Bool
         , key       :: i -> Int32
         , saveEntry :: e -> i -> o
         , empty     :: e
         }

-- TODO consider wheather is better always rewrite HT entry, or it is
--      necessary to check and compare depths
addHash :: TTable e o i
        -> i
        -> e
        -> IO ()
addHash tt i e = do
        _ <- H.update (table tt) (key tt i) (saveEntry tt e i)
        return ()

findHash :: TTable e o i -> i -> IO Bool
findHash tt i = do
        val <- H.lookup (table tt) $ key tt i
        case val of
            Nothing -> return False
            Just v  -> return $ isValid tt v i

getHash :: TTable e o i -> i -> IO e
getHash tt i = do
    val <- H.lookup (table tt) $ key tt i
    case val of
        Nothing -> return $ empty tt
        Just v  -> return $ getEntry tt v
