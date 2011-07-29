{-# LANGUAGE CPP          #-}
{-
 - | Depending on is CORES value we prepare computation for runnning
 - parallel or sequentially.
 -}
module Computation
    ( Stoplight
    , ComputationToken
    , startComputation
    , stopComputation
#if CORES > 1
    , module Control.Concurrent
#endif
    ) where


import Control.Concurrent.MVar
import Control.Concurrent

#if CORES > 1
type Stoplight = MVar ()
type ComputationToken = MVar [ThreadId]
#else
type Stoplight = ThreadId
type ComputationToken = ()
#endif


-- | Starts computation depending on number of CORES. If there is more than
-- one core computations called action could run in parallel.
startComputation :: (a -> MVar b -> IO ComputationToken) -> a -> MVar b
                 -> IO Stoplight

-- | Stops parallel or sequential computation.
stopComputation :: Stoplight -> IO ()


#if CORES > 1
startComputation comp par var = do
        ts <- comp par var
        stopL <- newEmptyMVar
        _ <- forkIO $ do
                takeMVar stopL
                mapM_ killThread =<< takeMVar ts
        return stopL

stopComputation t = putMVar t ()
#else
startComputation comp par var = forkIO $ comp par var
stopComputation = killThread
#endif
