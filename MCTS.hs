module MCTS (search) where

import Control.Concurrent
-- import System.IO
-- import System.Random
import BitRepresenation
-- import BitEval

-- TODO consider active player, step count, zdvojeni tahu -> hloubka
--      empty DMove (win for oponent)

-- | Mini-Max Tree representation
data MMTree = MMTree Board Player Int TreeNode

data TreeNode = Leaf
              | Node { children :: [MMTree] -- ^ possible steps from this
                     , value    :: Int      -- ^ actual value of this node
                     , number   :: Int      -- ^ how many times this node has been visited
                     }


search :: Board -> Player -> MVar (DMove, Int) -> IO ()
search board pl mvar = return ()

improveTree :: MMTree -> IO (MMTree, Int)
improveTree (MMTree b pl stepCount root) =
    case root of
        Leaf -> do
            val <- getValueByMC b
            return (createNode b val pl stepCount, val)
        _ -> do
            let (node, rest) = findAndRemoveBest (children root)
            (nodeNew, improved) <- improveTree node
            let improved' = (if stepCount == 3 then -1 else 1) * improved

            return ( MMTree b pl stepCount $
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

createNode :: Board -> Int -> Player -> Int -> MMTree
createNode b val pl stepCount = MMTree b pl stepCount $
        Node { children = map leafFromStep (generateSteps b pl (stepCount < 3))
             , value = val
             , number = 1 }
    where
        leafFromStep (s1,s2) =
                MMTree (fst $ makeMove b [s1,s2]) pl' stepCount' Leaf

        stepCount' = stepCount + 1 `mod` 4

        pl' = if stepCount' == 0 then oponent pl
                                 else pl

-- TODO meaningful magic constant (even 0s)
--      first 0 is wrong
descendByUCB1 :: MMTree -> Double
descendByUCB1 (MMTree _ _ _ Leaf) = 42
descendByUCB1 (MMTree _ _ _ root) = val
    where
        childrenNodes = children root
        (_,val,count) = foldr f (MMTree EmptyBoard Gold 0 Leaf,0,0) childrenNodes

        f :: MMTree -> (MMTree, Double, Int) -> (MMTree, Double, Int)
        f node (best,bestVal,s) | bestVal < nodeVal = (node,nodeVal,s+1)
                                | otherwise         = (best,bestVal,s+1)
            where
                nodeVal = case node of
                            MMTree _ _ _ Leaf -> 42
                            _ -> - (vl / nb) + (sqrt (2 * (log cn) / nb))
                tn = treeNode node
                [vl,nb,cn] = map fromIntegral [value tn, number tn, count]


treeNode :: MMTree -> TreeNode
treeNode (MMTree _ _ _ n) = n

getValueByMC :: Board -> IO Int
getValueByMC _ = return 1
