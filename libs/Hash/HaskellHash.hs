import qualified Data.HashTable as H

type HTable o = H.HashTable Int32 o

-- TODO consider wheather is better always rewrite HT entry, or it is
--      necessary to check and compare depths
addHash :: TTable e o i
        -> i
        -> e
        -> IO ()
addHash tt i e = do
        _ <- H.update (table tt) (key tt i) (saveEntry tt e i)
        return ()

getHash :: TTable e o i -> i -> IO (Maybe e)
getHash tt i = do
    val <- H.lookup (table tt) $ key tt i
    return $ val >>= validate tt i

newHT :: (Int32 -> Int32) -> IO (HTable o)
newHT = H.new (==)
