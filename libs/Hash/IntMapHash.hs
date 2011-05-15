import qualified Data.IntMap as I
import Control.Concurrent.MVar

type HTable o = MVar (I.IntMap o, Int32 -> Int)


addHash :: TTable e o i
        -> i
        -> e
        -> IO ()
addHash tt i e = do
        (im,f) <- takeMVar (table tt)
        let im' = I.insert (f $ key tt i) (saveEntry tt e i) im
        putMVar (table tt) (im',f)

findHash :: TTable e o i -> i -> IO Bool
findHash tt i = do
        (im,f) <- readMVar (table tt)
        let val = I.lookup (f $ key tt i) im
        case val of
            Nothing -> return False
            Just v  -> return $ isValid tt v i

getHash :: TTable e o i -> i -> IO e
getHash tt i = do
        (im,f) <- readMVar (table tt)
        let val = I.lookup (f $ key tt i) im
        case val of
            Nothing -> return $ empty tt
            Just v  -> return $ getEntry tt v

newHT :: (Int32 -> Int32) -> IO (HTable o)
newHT f = newMVar (I.empty, fromIntegral . f)

