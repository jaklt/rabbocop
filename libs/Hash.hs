{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Hash
    ( TTable(..)
    , HTable
    , findHash -- :: TTable e o i -> i -> IO Bool
    , getHash  -- :: TTable e o i -> i -> IO e
    , addHash  -- :: TTable e o i -> i -> e -> IO ()
    , newHT    -- :: (Int32 -> Int32) -> IO (HTable o)
    ) where

import Data.Int (Int32)

#ifdef JUDY
#include "Hash/JudyHash.hs"
#else
#include "Hash/HaskellHash.hs"
#endif

data TTable e o i
    = TT { table     :: HTable o
         , getEntry  :: o -> e
         , isValid   :: o -> i -> Bool
         , key       :: i -> Int32
         , saveEntry :: e -> i -> o
         , empty     :: e
         }
