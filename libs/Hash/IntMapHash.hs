import qualified Data.IntMap as I
import Data.IORef

type HTable' o = IORef (I.IntMap o, Int32 -> Int)


addHash :: HTable e o i
        -> i
        -> e
        -> IO ()
addHash tt i e =
        atomicModifyIORef (table tt) $ \(im,f) ->
            ((I.insert (f $ key tt i) (saveEntry tt e i) im, f), ())


getHash :: HTable e o i -> i -> IO (Maybe e)
getHash tt i = do
        (im,f) <- readIORef (table tt)
        let val = I.lookup (f $ key tt i) im
        return $ val >>= validate tt i

newHT :: (Int32 -> Int32) -> IO (HTable' o)
newHT f = newIORef (I.empty, fromIntegral . f)
