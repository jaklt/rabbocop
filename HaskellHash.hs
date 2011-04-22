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

newHT :: (Int32 -> Int32) -> IO (HTable o)
newHT = H.new (==)
