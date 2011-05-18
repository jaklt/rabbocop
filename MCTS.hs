{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
#ifdef ENGINE
module Main (main) where
import AEI
#else
module MCTS
    ( MMTree(..)
    , TreeNode(..)
    , newSearch
    , constructMove
    , improveTree
    , descendByUCB1
    , valueUCB
    , createNode
    , nodeValue
    , nodeVisitCount
    , nodeTreeNode
    , newTT
    ) where
#endif

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar
import Control.Monad (foldM)
import Bits.BitRepresentation
import Eval.BitEval
import Eval.MonteCarloEval

import Data.Bits
import Data.Int (Int32)
import Hash


#ifdef ENGINE
main :: IO ()
main = runAEIInterface newSearch
#endif

-- | Mini-Max Tree representation
data MMTree = MT { board     :: !Board
                 , movePhase :: !MovePhase
                 , treeNode  ::  MVar TreeNode
                 , step      :: (Step, Step)
                 }

data TreeNode = Leaf
              | Node { children   :: [MMTree]  -- ^ steps from this
                     , value      :: MVar Double -- ^ actual value of node
                     , visitCount :: MVar Int
                     }


iNFINITY' :: Num a => a
iNFINITY' = iNFINITY * iNFINITY

newSearch :: Int -> IO (Board -> MVar (DMove, Int) -> IO ())
newSearch tableSize = search <$> newTT tableSize

search :: MCTSTable
       -> Board             -- ^ starting position
       -> MVar (DMove, Int) -- ^ best results to store here
       -> IO ()
search tt b mv = do
        newLeaf <- newMVar Leaf
        search' MT { board = b
                   , movePhase = (mySide b, 0)
                   , treeNode = newLeaf
                   , step = (Pass, Pass)
                   }
                mv tt

search' :: MMTree -> MVar (DMove, Int) -> MCTSTable -> IO ()
search' mt mvar tt = do
        score <- improveTree mt tt 0
        move  <- constructMove mt 4
        changeMVar mvar (const (move,0))
        -- putStrLn $ "info actual " ++ show (move,score)
        search' mt mvar tt

constructMove :: MMTree -> Int -> IO DMove
constructMove _ 0 = return []
constructMove mt n = do
        tn <- nodeTreeNode mt
        case tn of
            Leaf -> return []
            _    -> do
                    best <- descendByUCB1 mt
                    ((step best) :) <$> constructMove best (n-1)

improveTree :: MMTree -> MCTSTable -> Int -> IO Double
improveTree mt tt !depth = do
        tn <- nodeTreeNode mt

        case tn of
            Leaf -> do
                val <- normaliseValue . (* (mySide (board mt) <#> Gold))
                       <$> getValueByMC (board mt) (movePhase mt)
                createNode mt val tt depth
                return val
            root -> do
                if null $ children root
                    -- immobilization
                    then do
                        inf <- normaliseValue
                               <$> evalImmobilised (board mt) (player mt)
                        changeMVar' (value root) (const inf)
                        changeMVar' (visitCount root) (+1)
                        return inf
                        -- TODO test +/-

                    else do
                        node <- descendByUCB1 mt
                        improvement <- improveTree node tt (depth + 1)
                        changeMVar' (value root) (+ improvement)
                        changeMVar' (visitCount root) (+1)
                        return improvement

createNode :: MMTree -> Double -> MCTSTable -> Int -> IO ()
createNode mt val tt depth = do
        inTranspositionTable <- findHash tt index

        if inTranspositionTable
            then do
                tn <- getHash tt index
                changeMVar' (visitCount tn) (+1)
                changeMVar' (value tn)    (+val)
                changeMVar  (treeNode mt) (const tn)
            else do
                chls <- mapM (leafFromStep mt) steps
                newVal <- newMVar val
                newVisitCount <- newMVar 1

                let tn = Node { children   = chls
                              , value      = newVal
                              , visitCount = newVisitCount
                              }
                changeMVar (treeNode mt) (const tn)
                addHash tt index tn
    where
        steps = generateSteps (board mt) (player mt) (stepCount mt < 3)
        index = (board mt, depth, movePhase mt)

leafFromStep :: MMTree -> (Step, Step) -> IO MMTree
leafFromStep mt s@(s1,s2) = do
    newLeaf <- newMVar Leaf
    return $ MT
        { board = fst $ makeMove (board mt) [s1,s2]
        , movePhase = stepInMove (movePhase mt) s2
        , treeNode = newLeaf
        , step = s
        }

-- TODO speedup
-- | if immobilised returns first_argument
descendByUCB1 :: MMTree -> IO MMTree
descendByUCB1 mt = do
        tn <- nodeTreeNode mt
        let chs = children tn

        case chs of
            [] -> return mt -- immobilization
            _  -> do
                vc <- readMVar $ visitCount tn
                descendByUCB1' chs vc

-- | first argument have to be not empty
descendByUCB1' :: [MMTree] -> Int -> IO MMTree
descendByUCB1' mms nb = do
        valHMms <- valueUCB hMms nb
        fst <$> foldM (accumUCB nb) (hMms, valHMms) (tail mms)
    where
        hMms = head mms

accumUCB :: Int -> (MMTree, Double) -> MMTree -> IO (MMTree, Double)
accumUCB count (best, bestValue) mt = do
        nodeVal <- valueUCB mt count

        if nodeVal > bestValue
            then return (mt, nodeVal)
            else return (best, bestValue)

valueUCB :: MMTree -> Int -> IO Double
valueUCB mt count = do
        tn <- nodeTreeNode mt

        case tn of
            Leaf -> return iNFINITY'
            Node { children = [] } -> nodeValue mt
            _ -> do
                nb <- fromIntegral <$> readMVar (visitCount tn)
                vl <- readMVar (value tn)
                return $ (vl / nb) + 0.01 * sqrt (log cn / nb)
    where
        cn = fromIntegral count

normaliseValue :: Int -> Double
normaliseValue v = 2 / (1 + exp (-0.0003 * fromIntegral v)) - 1

stepCount :: MMTree -> Int
stepCount = snd . movePhase

player :: MMTree -> Player
player = fst . movePhase

nodeValue :: MMTree -> IO Double
nodeValue mt = readMVar (treeNode mt) >>= readMVar . value

nodeVisitCount :: MMTree -> IO Int
nodeVisitCount mt = readMVar (treeNode mt) >>= readMVar . visitCount

nodeTreeNode :: MMTree -> IO TreeNode
nodeTreeNode = readMVar . treeNode

changeMVar :: MVar a -> (a -> a) -> IO ()
changeMVar mv f = modifyMVar_ mv $ return . f

changeMVar' :: MVar a -> (a -> a) -> IO ()
changeMVar' mv f = modifyMVar_ mv $ (seq <$> id <*> return) . f


type MCTSTable = TTable TreeNode HObject (Board, Int, MovePhase)

data HObject = HO { board0    :: Board
                  , treeNode0 :: TreeNode
                  , depth0    :: Int
                  , phase     :: MovePhase
                  }

newTT :: Int -> IO MCTSTable
newTT tableSize = do
    ht <- newHT id
    return TT
       { table     = ht
       , getEntry  = treeNode0
       , isValid   = isValid'
       , key       = key' ts
       , saveEntry = saveEntry'
       , empty     = Leaf
       }
    where
        ts = (fromIntegral tableSize) * (500000 `div` 200)


isValid' :: HObject -> (Board, Int, MovePhase) -> Bool
isValid' e (b,d,mp) =  phase e == mp
                   && depth0 e == d
                   && hash (board0 e) == hash b
                   && board0 e == b

key' :: Int32 -> (Board, Int, MovePhase) -> Int32
key' tableSize (b, de, (pl,s)) = fromIntegral . (`mod` tableSize) $
        fromIntegral (hash b) `xor` fromIntegral (playerToInt pl)
        `xor` (fromIntegral s `shift` 4) `xor` (fromIntegral de `shift` 6)

saveEntry' :: TreeNode -> (Board, Int, MovePhase) -> HObject
saveEntry' tn (b, d, mp) =
        HO { board0    = b
           , treeNode0 = tn
           , depth0    = d
           , phase     = mp
           }
