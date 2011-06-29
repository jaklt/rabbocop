{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Hash
    ( TTable(..)
    , HTable
    , getHash  -- :: TTable e o i -> i -> IO (Maybe e)
    , addHash  -- :: TTable e o i -> i -> e -> IO ()
    , newHT    -- :: (Int32 -> Int32) -> IO (HTable o)
    ) where

import Data.Int (Int32)

#ifdef JUDY
#include "Hash/JudyHash.hs"
#elif HASKELL_HASH
#include "Hash/HaskellHash.hs"
#else
#include "Hash/IntMapHash.hs"
#endif

data TTable e o i
    = TT { table     :: HTable o
         , getEntry  :: o -> e
         , isValid   :: i -> o -> Bool
         , key       :: i -> Int32
         , saveEntry :: e -> i -> o
         }

validate :: TTable e o i -> i -> o -> Maybe e
validate tt i v = if isValid tt i v
                    then Just $ getEntry tt v
                    else Nothing


-- TODO add stg like: shouldReplaceWith? :: o -> o -> Bool

-- TODO try rewrite validate to point-free style
