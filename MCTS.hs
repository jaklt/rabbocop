{-# LANGUAGE BangPatterns #-}
module MCTS (
    MMTree(..),
    TreeNode(..),
    search,
    improveTree,
    findAndRemoveBest,
    createNode,
    descendByUCB1,
) where

import Control.Concurrent
import BitRepresenation
import BitEval (iNFINITY)
import MonteCarloEval

-- TODO consider active player, step count, zdvojeni tahu -> hloubka

-- | Mini-Max Tree representation
data MMTree = MT { board     :: !Board
                 , movePhase :: !MovePhase
                 , treeNode  :: !TreeNode
                 , step :: (Step, Step)
                 } deriving (Show)

data TreeNode = Leaf
              | Node { children :: [MMTree] -- ^ possible steps from this
                     , value    :: !Int     -- ^ actual value of this node
                     , number   :: !Int     -- ^ how many times this node has been visited
                     } deriving (Show)

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
search' !mt mvar = do
        (mt',score) <- improveTree mt
        _ <- move `seq` swapMVar mvar (move,score)
        search' mt' mvar
    where
        move = constructMove mt 4

constructMove :: MMTree -> Int -> DMove
constructMove _ 0 = []
constructMove (MT { treeNode = Leaf }) _ = []
constructMove !mt !n = (s `seq` subTreeMove) `seq` s : subTreeMove
    where
        mt' = fst $ findAndRemoveBest (children $ treeNode mt)
        s = step mt'
        subTreeMove = constructMove mt' (n-1)

improveTree :: MMTree -> IO (MMTree, Int)
improveTree mt =
    case treeNode mt of
        Leaf -> do
            val <- getValueByMC (board mt) (movePhase mt)
            return (createNode mt (val * (player mt <#> Gold)), val)
        root -> do
            let (node, rest) = findAndRemoveBest (children $ treeNode mt)
            (nodeNew, improvement) <- improveTree node
            -- TODO asi problem - vytvari se stromecek dobre?
            let improvement' = player mt <#> Gold * improvement

            return ( mt { treeNode = Node
                            { value    = value root + improvement'
                            , number   = number root + 1
                            , children = nodeNew : rest
                            }
                        }
                   , improvement)

-- TODO co kdyz je vstupni seznam prazdny?
findAndRemoveBest :: [MMTree] -> (MMTree, [MMTree])
findAndRemoveBest [] = error "Empty children list"
findAndRemoveBest (r:rs) = go (r, rs) (descendByUCB1 r)
    where
        go :: (MMTree, [MMTree]) -> Double -> (MMTree, [MMTree])
        go (best, []) _ = (best, [])
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
        mt { treeNode =
                Node { children = map (leafFromStep mt) steps
                     , value = val
                     , number = 1
                     }
           }
    where
        steps = generateSteps (board mt) (player mt) (stepCount mt < 2)

leafFromStep :: MMTree -> (Step, Step) -> MMTree
leafFromStep mt s@(s1,s2) =
    MT { board = fst $ makeMove (board mt) [s1,s2]
       , movePhase = stepInMove (movePhase mt) s2
       , treeNode = Leaf
       , step = s
       }

-- TODO first 0 is wrong
--      +/- iNFINITYs discussion
--      improve speed (compute in one walk)
descendByUCB1 :: MMTree -> Double
descendByUCB1 mt = case treeNode mt of Leaf -> iNFINITY
                                       _    -> val
    where
        childrenNodes = children $ treeNode mt
        count = length childrenNodes
        val = foldr (accumNodes count) 0 childrenNodes

accumNodes :: Int -> MMTree -> Double -> Double
accumNodes count node bestVal
        | bestVal < nodeVal = nodeVal
        | otherwise         = bestVal
    where
        tn = treeNode node
        nodeVal = case tn of
                    Leaf -> iNFINITY
                    _ -> - (vl / nb) + sqrt (2 * log cn / nb)
        [vl,nb,cn] = map fromIntegral [value tn, number tn, count]
