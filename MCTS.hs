module MCTS (search) where

import Control.Concurrent
-- import System.IO
import System.Random
import BitRepresenation
import BitEval

-- TODO consider active player, step count, zdvojeni tahu -> hloubka
--      empty DMove (win for oponent)

-- | Mini-Max Tree representation
data MMTree = MMTree Board MovePhase TreeNode

type MovePhase = (Player, Int)
data TreeNode = Leaf
              | Node { children :: [MMTree] -- ^ possible steps from this
                     , value    :: Int      -- ^ actual value of this node
                     , number   :: Int      -- ^ how many times this node has been visited
                     }


search :: Board             -- ^ starting position
       -> MVar (DMove, Int) -- ^ best results to store here
       -> IO ()
search board mvar = return ()

improveTree :: MMTree -> IO (MMTree, Int)
improveTree (MMTree b mp@(_,stepCount) root) =
    case root of
        Leaf -> do
            val <- getValueByMC b mp
            return (createNode b val mp, val)
        _ -> do
            let (node, rest) = findAndRemoveBest (children root)
            (nodeNew, improved) <- improveTree node
            let improved' = (if stepCount == 3 then -1 else 1) * improved

            return ( MMTree b mp $
                        root { value    = (value root) + improved'
                             , number   = (number root) + 1
                             , children = nodeNew : rest }
                   , improved')

-- TODO co kdyz je vstupni seznam prazdny?
findAndRemoveBest :: [MMTree] -> (MMTree, [MMTree])
findAndRemoveBest [] = error "Empty children list"
findAndRemoveBest (r:rs) = go (r, rs) (descendByUCB1 r)
    where
        go :: (MMTree, [MMTree]) -> Double -> (MMTree, [MMTree])
        go (_, []) _ = error "Empty children list"
        go (best, (t:ts)) bestValue =
                if bestValue < tValue
                    then addToSnd best $ go (   t, ts) tValue
                    else addToSnd    t $ go (best, ts) bestValue
            where
                tValue = descendByUCB1 t

addToSnd :: a -> (b, [a]) -> (b, [a])
addToSnd a (b, c) = (b, a:c)

createNode :: Board -> Int -> MovePhase -> MMTree
createNode b val mp@(pl,stepCount) = MMTree b mp $
        Node { children = map leafFromStep (generateSteps b pl (stepCount < 2))
             , value = val
             , number = 1 }
    where
        leafFromStep (s1,s2) =
            MMTree (fst $ makeMove b [s1,s2]) (stepInMove mp s2) Leaf

stepInMove :: MovePhase -> Step -> MovePhase
stepInMove (pl,stepCount) s = (pl',stepCount')
    where
        stepCount' = stepCount + (if s == Pass then 1 else 2) `mod` 4

        pl' = if stepCount' == 0 then oponent pl
                                 else pl

-- TODO meaningful magic constant (even 0s)
--      first 0 is wrong
descendByUCB1 :: MMTree -> Double
descendByUCB1 (MMTree _ _ Leaf) = 42
descendByUCB1 (MMTree _ _ root) = val
    where
        childrenNodes = children root
        (_,val,count) = foldr f (MMTree EmptyBoard (Gold,0) Leaf,0,0) childrenNodes

        f :: MMTree -> (MMTree, Double, Int) -> (MMTree, Double, Int)
        f node (best,bestVal,s) | bestVal < nodeVal = (node,nodeVal,s+1)
                                | otherwise         = (best,bestVal,s+1)
            where
                nodeVal = case node of
                            MMTree _ _ Leaf -> 42
                            _ -> - (vl / nb) + (sqrt (2 * (log cn) / nb))
                tn = treeNode node
                [vl,nb,cn] = map fromIntegral [value tn, number tn, count]

treeNode :: MMTree -> TreeNode
treeNode (MMTree _ _ n) = n

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
