module MCTS (search) where

import Control.Concurrent
-- import System.IO
import System.Random
import BitRepresenation
import BitEval

-- TODO consider active player, step count, zdvojeni tahu -> hloubka
--      empty DMove (win for oponent)

-- | Mini-Max Tree representation
data MMTree = MT { board     :: Board
                 , movePhase :: MovePhase
                 , treeNode  :: TreeNode
                 , step :: (Step, Step)
                 }

type MovePhase = (Player, Int) -- ^ (active player, number of steps played in move)
data TreeNode = Leaf
              | Node { children :: [MMTree] -- ^ possible steps from this
                     , value    :: Int      -- ^ actual value of this node
                     , number   :: Int      -- ^ how many times this node has been visited
                     }


stepCount :: MMTree -> Int
stepCount = snd . movePhase

player :: MMTree -> Player
player = fst . movePhase

search :: Board             -- ^ starting position
       -> MVar (DMove, Int) -- ^ best results to store here
       -> IO ()
search b = search' MT { board = b
                      , movePhase = (mySide b, 0)
                      , treeNode = Leaf
                      , step = (Pass, Pass)
                      }

search' :: MMTree -> MVar (DMove, Int) -> IO ()
search' mt mvar = do
        (mt',_) <- improveTree mt
        _ <- aj `seq` swapMVar mvar (aj, 0)
        search' mt' mvar
    where
        aj = constructMove mt 4

constructMove :: MMTree -> Int -> DMove
constructMove _ 0 = []
constructMove mt n = step mt' : constructMove mt' (n-1)
    where
        mt' = fst $ findAndRemoveBest (children $ treeNode mt)

improveTree :: MMTree -> IO (MMTree, Int)
improveTree mt =
    case treeNode mt of
        Leaf -> do
            val <- getValueByMC (board mt) (movePhase mt)
            return (createNode mt val, val)
        root -> do
            let (node, rest) = findAndRemoveBest (children $ treeNode mt)
            (nodeNew, improved) <- improveTree node
            let improved' = (if stepCount mt == 3 then -1 else 1) * improved

            return ( mt { treeNode = Node
                            { value    = value root + improved'
                            , number   = number root + 1
                            , children = nodeNew : rest
                            }
                        }
                   , improved')

-- TODO co kdyz je vstupni seznam prazdny?
findAndRemoveBest :: [MMTree] -> (MMTree, [MMTree])
findAndRemoveBest [] = error "Empty children list"
findAndRemoveBest (r:rs) = go (r, rs) (descendByUCB1 r)
    where
        go :: (MMTree, [MMTree]) -> Double -> (MMTree, [MMTree])
        go (_, []) _ = error "Empty children list"
        go (best, t:ts) bestValue =
                if bestValue < tValue
                    then addToSnd best $ go (   t, ts) tValue
                    else addToSnd    t $ go (best, ts) bestValue
            where
                tValue = descendByUCB1 t

addToSnd :: a -> (b, [a]) -> (b, [a])
addToSnd a (b, c) = (b, a:c)

createNode :: MMTree -> Int -> MMTree
createNode mt val =
        mt { treeNode = Node { children = map leafFromStep steps
                             , value = val
                             , number = 1
                             }
           }
    where
        b = board mt

        leafFromStep s@(s1,s2) =
            MT { board = fst $ makeMove b [s1,s2]
               , movePhase = stepInMove (movePhase mt) s2
               , treeNode = Leaf
               , step = s
               }

        steps = generateSteps b (player mt) (stepCount mt < 2)

stepInMove :: MovePhase -> Step -> MovePhase
stepInMove (pl,steps) s = (pl',steps')
    where
        steps' = steps + (if s == Pass then 1 else 2) `mod` 4

        pl' = if steps' == 0 then oponent pl
                                 else pl

-- TODO meaningful magic constant (even 0s)
--      first 0 is wrong
descendByUCB1 :: MMTree -> Double
descendByUCB1 mt = case treeNode mt of Leaf -> 42
                                       _    -> val
    where
        childrenNodes = children $ treeNode mt
        (val,count) = foldr f (0,0) childrenNodes

        f :: MMTree -> (Double, Int) -> (Double, Int)
        f node (bestVal,s) | bestVal < nodeVal = (nodeVal,s+1)
                           | otherwise         = (bestVal,s+1)
            where
                nodeVal = case tn of
                            Leaf -> 42
                            _ -> - (vl / nb) + sqrt (2 * log cn / nb)
                tn = treeNode node
                [vl,nb,cn] = map fromIntegral [value tn, number tn, count]

depth, simulations :: Int
depth = 100       -- ^ simulation depth
simulations = 100 -- ^ number of simulations

getValueByMC :: Board -> MovePhase -> IO Int
getValueByMC b mp = do
    s <- mapM (randomSimulation mp depth) $ replicate simulations b
    return $ sum s

randomSimulation :: MovePhase -> Int -> Board -> IO Int
randomSimulation (pl,_) 0 b = eval b pl
randomSimulation mp@(pl,sc) d b = do
    (s1,s2) <- chooseRandomly $ generateSteps b pl (sc < 2)
    randomSimulation (stepInMove mp s2) (d-1) (fst $ makeMove b [s1,s2])

chooseRandomly :: [a] -> IO a
chooseRandomly xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

-- TODO makeMove could be rewriten by makeStep
