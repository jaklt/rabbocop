{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Hash
    ( HTable(..)
    , HTable'
    , getHash  -- :: HTable e o i -> i -> IO (Maybe e)
    , addHash  -- :: HTable e o i -> i -> e -> IO ()
    , newHT    -- :: (Int32 -> Int32) -> IO (HTable' o)
    ) where

import Data.Int (Int32)

#ifdef JUDY
#include "Hash/JudyHash.hs"
#elif HASKELL_HASH
#include "Hash/HaskellHash.hs"
#else
#include "Hash/IntMapHash.hs"
#endif

data HTable e o i
    = HT { table     :: HTable' o
         , getEntry  :: o -> e
         , isValid   :: i -> o -> Bool
         , key       :: i -> Int32
         , saveEntry :: e -> i -> o
         }

validate :: HTable e o i -> i -> o -> Maybe e
validate tt i v = if isValid tt i v
                    then Just $ getEntry tt v
                    else Nothing


-- TODO add stg like: shouldReplaceWith? :: o -> o -> Bool
