module MCTS (search) where

import Control.Concurrent
-- import System.IO
-- import System.Random
import BitRepresenation
-- import BitEval

-- TODO consider active player, step count, zdvojeni tahu -> hloubka
--      empty DMove (win for oponent)

-- | Mini-Max Tree representation
data MMTree = Leaf Board
            | Node { boardState :: Board  -- ^ board state
                   , children :: [MMTree] -- ^ possible steps from this
                   , value  :: Int        -- ^ actual value of this node
                   , number :: Int        -- ^ how many times this node has been visited
                   }

search :: Board -> Player -> MVar (DMove, Int) -> IO ()
search board pl mvar = return ()

emptyLeaf :: MMTree
emptyLeaf = Leaf EmptyBoard

improveTree :: MMTree -> IO (MMTree, Int)
improveTree (Leaf b) = createNode b
improveTree rootNode = do
    let (node, rest) = findAndRemoveBest (children rootNode)
    (nodeNew, improved) <- improveTree node

    return ( rootNode{ value = (value rootNode) + improved
                     , number = (number rootNode) + 1
                     , children = nodeNew : children rootNode}
           , -improved)

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

createNode :: Board -> IO (MMTree, Int)
createNode b = do
    val <- getValueByMC b
    return ( Node { boardState = b
                  , children = map (leafFromStep b) (generateSteps b Gold True)
                  , value = val, number = 1}
           , -val)

leafFromStep :: Board -> (Step,Step) -> MMTree
leafFromStep b (s1,s2) = Leaf $ fst $ makeMove b [s1,s2]

-- TODO meaningful magic constant (even 0s)
--      first 0 is wrong
descendByUCB1 :: MMTree -> Double
descendByUCB1 (Leaf _) = 42
descendByUCB1 node0 = val
    where
        childrenNodes = children node0
        (_,val,count) = foldr f (emptyLeaf,0,0) childrenNodes

        f :: MMTree -> (MMTree, Double, Int) -> (MMTree, Double, Int)
        f node (best,bestVal,s) | bestVal < nodeVal = (node,nodeVal,s+1)
                                | otherwise         = (best,bestVal,s+1)
            where
                nodeVal = case node of
                            (Leaf _) -> 42
                            _ -> - (vl / nb) + (sqrt (2 * (log cn) / nb))
                [vl,nb,cn] = map fromIntegral [value node, number node, count]

getValueByMC :: Board -> IO Int
getValueByMC _ = return 1
