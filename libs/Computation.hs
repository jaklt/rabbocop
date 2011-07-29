{-# LANGUAGE CPP          #-}
module Computation
    ( Stoplight
    , ComputationToken
    , startComputation
    , stopComputation
#if CORES > 0
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


startComputation :: (a -> MVar b -> IO ComputationToken) -> a -> MVar b
                 -> IO Stoplight
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
