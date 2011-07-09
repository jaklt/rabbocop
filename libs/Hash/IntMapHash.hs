import qualified Data.IntMap as I
import Control.Concurrent.MVar

type HTable' o = MVar (I.IntMap o, Int32 -> Int)


addHash :: HTable e o i
        -> i
        -> e
        -> IO ()
addHash tt i e = do
        (im,f) <- readMVar (table tt)
        let !im' = I.insert (f $ key tt i) (saveEntry tt e i) im
        modifyMVar_ (table tt) (return . const (im',f))

getHash :: HTable e o i -> i -> IO (Maybe e)
getHash tt i = do
        (im,f) <- readMVar (table tt)
        let val = I.lookup (f $ key tt i) im
        return $ val >>= validate tt i

newHT :: (Int32 -> Int32) -> IO (HTable' o)
newHT f = newMVar (I.empty, fromIntegral . f)

