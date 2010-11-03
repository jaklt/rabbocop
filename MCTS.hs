module MCTS (search) where

import Control.Concurrent
import System.IO
import BitRepresenation
import BitEval

search :: Board -> Player -> MVar (DMove, Int) -> IO ()
search _ _ _ = return ()
