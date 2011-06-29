import Data.Judy as J
import GHC.Ptr
import GHC.Base
import GHC.Word
import Foreign.StablePtr
-- import System.Mem (performGC)

-------------------------------------------------------------------------
--  Current implementation of this library is not useful (too slow),   --
--  but JudyHash uses less memory (about half).                        --
-------------------------------------------------------------------------

-- From Judy documentation:
-- instance JE HObject where ...
--  -- strict bytestrings may be stored.
--  --
--  -- TODO: Quite a bit slower than using an IntMap ( see C.hs , D.hs )
--  --


newtype JudyElement e = E { getJudyElement :: e }
type HTable o = (J.JudyL (JudyElement o), Int32 -> Int32)

instance J.JE (JudyElement e) where
    toWord b = do
        p <- newStablePtr b
        case castStablePtrToPtr p of
             Ptr a# -> return $! W# (int2Word# (addr2Int# a#))

    fromWord w = do
        case fromIntegral w of
             I# i# -> case int2Addr# i# of
                     a# -> deRefStablePtr (castPtrToStablePtr (Ptr a#))
    {-# INLINE toWord   #-}
    {-# INLINE fromWord #-}

-- TODO consider wheather is better always rewrite HT entry, or it is
--      necessary to check and compare depths
addHash :: TTable e o i
        -> i
        -> e
        -> IO ()
addHash tt i e = do
        _ <- J.insert (jkey tt i) (E $ saveEntry tt e i) (jtable tt)
        -- performGC
        return ()

getHash :: TTable e o i -> i -> IO (Maybe e)
getHash tt i = do
    val <- J.lookup (jkey tt i) (jtable tt)
    return $ val >>= (validate tt i . getJudyElement)

newHT :: (Int32 -> Int32) -> IO (HTable o)
newHT fun = do
        j <- J.new
        return (j,fun)

jtable :: TTable e o i -> JudyL (JudyElement o)
jtable = fst . table

jkey :: TTable e o i -> i -> Key
jkey tt = fromIntegral . (snd $ table tt) . (key tt)
