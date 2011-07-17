{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
#ifdef ENGINE
module Main (main) where
import AEI
#else
module MCTS
    ( MMTree(..)
    , MCTSTables
    , TreeNode(..)
    , newSearch       -- :: Int -> IO SearchEngine
    , constructMove   -- :: MCTSTables -> MMTree -> Int -> IO DMove
    , improveTree     -- :: MCTSTables -> MMTree -> Int -> IO Double
    , descendByUCB1   -- :: MCTSTables -> MMTree -> Int -> IO MMTree
    , createNode      -- :: MCTSTables -> MMTree -> Double -> Int -> IO ()
    , nodeValue       -- :: MMTree -> IO Double
    , nodeVisitCount  -- :: MMTree -> IO Int
    , nodeTreeNode    -- :: MMTree -> IO TreeNode
    , newHTables      -- :: Int -> IO MCTSTables
    , leaf            -- :: TreeNode
    , isLeaf          -- :: Int -> TreeNode -> Bool
    ) where

import AEI (SearchEngine)
#endif

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent.MVar
import Control.Monad (foldM, void)
import Data.Bits
import Data.Int (Int32)
import Data.Maybe (fromMaybe)

import Bits.BitRepresentation
import Eval.BitEval
import Eval.MonteCarloEval
import Hash

#ifdef VERBOSE
import System.IO (hFlush, stdout)
import Control.Monad (when)
#endif


#ifdef ENGINE
main :: IO ()
main = runAEIInterface newSearch
#endif

-- | Mini-Max Tree representation
data MMTree = MT { board     :: !Board
                 , movePhase :: !MovePhase
                 , treeNode  ::  MVar TreeNode
                 , step      :: DStep
                 }

data TreeNode = Node { children   :: ![MMTree] -- ^ steps from this
                     , value      :: !Double   -- ^ actual value of node
                     , visitCount :: !Int
                     }


iNFINITY' :: Double
iNFINITY' = 0.9

thresholdMaternity :: Int
thresholdMaternity = 5

newSearch :: Int -> IO SearchEngine
newSearch = return . search

search :: Int                  -- ^ table size
       -> Board                -- ^ starting position
       -> MVar (DMove, String) -- ^ best results to store here
       -> IO ()
search tableSize b mv = do
        newLeaf <- newMVar leaf
        tables <- newHTables tableSize
        search' MT { board = b
                   , movePhase = (mySide b, 0)
                   , treeNode = newLeaf
                   , step = (Pass, Pass)
                   }
                tables mv 1

search' :: MMTree -> MCTSTables -> MVar (DMove, String) -> Int -> IO ()
search' mt tables mvar !count = do
        void $ improveTree tables mt 0
        move  <- constructMove tables mt 0
        score <- (/) <$> nodeValue mt <*> (fromIntegral <$> nodeVisitCount mt)
        changeMVar mvar (const (move, show score ++ " " ++ show count))
#ifdef VERBOSE
        when (count `mod` VERBOSE == 0) $
            putStrLn ("info actual " ++ show (move,score))
            >> hFlush stdout
#endif
        search' mt tables mvar (count+1)

constructMove :: MCTSTables -> MMTree -> Int -> IO DMove
constructMove _ _ 4 = return []
constructMove tables mt n = do
        tn <- nodeTreeNode mt

        if player mt /= mySide (board mt) || isLeaf n tn
            then return []
            else do
                best <- descendByUCB1 tables mt n
                ((step best) :) <$> constructMove tables best (n+1)

leaf :: TreeNode
leaf = Node { children   = []
            , value      = 0
            , visitCount = 0
            }

isLeaf :: Int -> TreeNode -> Bool
isLeaf depth (Node { visitCount = vc }) = vc < thresholdMaternity + depth

isMature :: Int -> TreeNode -> Bool
isMature depth tn = not $ isLeaf (depth-1) tn

improveTree :: MCTSTables -> MMTree -> Int -> IO Double
improveTree tables mt !depth = do
        tn <- nodeTreeNode mt

        if isLeaf depth tn
            then do
                val <- normaliseValue
                   <$> getValueByMC (board mt) (movePhase mt)

                if isMature depth tn
                    then modifyMVar_ (treeNode mt)
                       $ createNode tables mt val depth
                    else changeMVar (treeNode mt)
                       $ improveTreeNode val

                improveStep tables (step mt) val
                return val
            else do
                if null $ children tn
                    -- immobilization
                    then
                        return $ value tn

                    else do
                        node <- descendByUCB1 tables mt depth
                        impr <- improveTree tables node (depth + 1)

                        improveStep tables (step mt) impr
                        changeMVar (treeNode mt) $ improveTreeNode impr
                        return impr

improveTreeNode :: Double -> TreeNode -> TreeNode
improveTreeNode impr tn@(Node { value = val, visitCount = vc }) =
        tn { value      = val + impr
           , visitCount = vc  + 1
           }

createNode :: MCTSTables -> MMTree -> Double -> Int -> TreeNode
           -> IO TreeNode
createNode tables mt impr depth (Node { value = val, visitCount = vc }) = do
        -- generate steps
        chls <- mapM (leafFromStep tables brd mp depth) steps

        newVal <- if null steps -- is immobilised?
                    then normaliseValue <$> evalImmobilised (board mt) pl
                    else return $ val + impr

        return Node { children   = chls
                    , value      = newVal
                    , visitCount = vc + 1
                    }
    where
        steps = generateSteps brd pl (canPushOrPull mp)
        brd   = board mt
        mp@(pl,_) = movePhase mt

leafFromStep :: MCTSTables -> Board -> MovePhase -> Int -> DStep
             -> IO MMTree
leafFromStep tables brd mp depth s@(_,s2) = do
        fromTT <- getHash tt index

        tn <- case fromTT of
                Just tn -> return tn
                Nothing -> do
                    -- insert new item to TT
                    tn <- newMVar leaf
                    addHash tt index tn
                    return tn

        return $ MT
            { board = brd'
            , movePhase = mp'
            , treeNode = tn
            , step = s
            }
    where
        index@(brd', _, mp') =
                (makeDStep' brd s, depth, stepInMove mp s2)
        tt = ttTable tables

-- U C T   f o r m u l a

type ParentToken = (MCTSTables, Int, Int, Int, MovePhase, DStep, Board)

-- | Immobilised position cause fail.
descendByUCB1 :: MCTSTables -> MMTree -> Int -> IO MMTree
descendByUCB1 tables mt depth = do
        tn <- nodeTreeNode mt
        let chs   = children tn
            quant = player mt <#> Gold
            mp@(_,sc) = movePhase mt
            token = ( tables
                    , visitCount tn
                    , quant
                    , depth+1
                    , mp
                    , if sc == 0 then (Pass,Pass)
                                 else step mt
                    , board mt
                    )

        descendByUCB1' token chs

-- | first argument have to be not empty
descendByUCB1' :: ParentToken -> [MMTree] -> IO MMTree
descendByUCB1' token (hMms:mms) = do
        valHMms <- valueUCB token hMms
        fst <$> foldM (accumUCB token) (hMms, valHMms) mms
descendByUCB1' _ _ = error "Given list is empty in descendByUCB1'."

accumUCB :: ParentToken -> (MMTree, Double) -> MMTree
         -> IO (MMTree, Double)
accumUCB token (best, bestValue) mt = do
        nodeVal <- valueUCB token mt

        if nodeVal > bestValue
            then return (mt, nodeVal)
            else return (best, bestValue)

valueUCB :: ParentToken -> MMTree -> IO Double
valueUCB (tables,count,quant,depth,mp,st,board) mt = do
        tn <- nodeTreeNode mt
        let nb = fromIntegral $ visitCount tn
        let vl = value tn
        let stepVal = evalStep board mp st (step mt)
#if HH
        histValPair <- getHash (hhTable tables) $ step mt
        let histVal = fromMaybe 0 $ uncurry (/) <$> histValPair
#endif

        case tn of
            Node { visitCount = 0 } ->
                    return $ iNFINITY' + stepVal/(nb+1)
            _ | null (children tn) && not (isLeaf depth tn) ->
                    nodeValue mt -- immobilised
            _ -> return $ (quant' * vl / nb) + 0.01 * sqrt (log cn / nb)
                        + stepVal / nb
#if HH
                        + (quant' * histVal) / nb
#endif
    where
        cn = fromIntegral count
        quant' = fromIntegral quant

normaliseValue :: Int -> Double
normaliseValue v = 2 / (1 + exp (-0.0003 * fromIntegral v)) - 1

stepCount :: MMTree -> Int
stepCount = snd . movePhase

player :: MMTree -> Player
player = fst . movePhase

nodeValue :: MMTree -> IO Double
nodeValue mt = value <$> readMVar (treeNode mt)

nodeVisitCount :: MMTree -> IO Int
nodeVisitCount mt = visitCount <$> readMVar (treeNode mt)

nodeTreeNode :: MMTree -> IO TreeNode
nodeTreeNode = readMVar . treeNode

changeMVar :: MVar a -> (a -> a) -> IO ()
changeMVar mv f = modifyMVar_ mv $ return . f

changeMVar' :: MVar a -> (a -> a) -> IO ()
changeMVar' mv f = modifyMVar_ mv $ (seq <$> id <*> return) . f


-- Implementation of Transposition table

type TTable = HTable (MVar TreeNode) TTObject (Board, Int, MovePhase)

data TTObject = TTO { board0    :: Board
                    , treeNode0 :: MVar TreeNode
                    , depth0    :: Int
                    , phase     :: MovePhase
                    }

newTT :: Int -> IO TTable
newTT tableSize = do
    ht <- newHT id
    return HT
       { table     = ht
       , getEntry  = treeNode0
       , isValid   = ttIsValid
       , key       = ttKey ts
       , saveEntry = ttSaveEntry
       }
    where
        ts = (fromIntegral tableSize) * (500000 `div` 200)


ttIsValid :: (Board, Int, MovePhase) -> TTObject -> Bool
ttIsValid (b,d,mp) e =  phase e == mp
                     && depth0 e == d
                     && hash (board0 e) == hash b
                     && board0 e == b

ttKey :: Int32 -> (Board, Int, MovePhase) -> Int32
ttKey tableSize (b, de, (pl,s)) = fromIntegral . (`mod` tableSize) $
        fromIntegral (hash b) `xor` fromIntegral (playerToInt pl)
        `xor` (fromIntegral s `shift` 4) `xor` (fromIntegral de `shift` 6)

ttSaveEntry :: (MVar TreeNode) -> (Board, Int, MovePhase) -> TTObject
ttSaveEntry tn (b, d, mp) =
        TTO { board0    = b
            , treeNode0 = tn
            , depth0    = d
            , phase     = mp
            }

-- Implementation of History heuristics table

type HHTable = HTable (Double,Double) HHObject DStep

data HHObject = HHO { hhValue :: (Double, Double)
                    , step0   :: DStep
                    }

newHH :: IO HHTable
newHH = do
    ht <- newHT id
    return HT
       { table     = ht
       , getEntry  = hhValue
       , isValid   = \_ _ -> True
       , key       = dStepToInt
       , saveEntry = hhSaveEntry
       }

hhSaveEntry :: (Double, Double) -> DStep -> HHObject
hhSaveEntry val st = HHO { hhValue = val
                         , step0   = st
                         }

improveStep :: MCTSTables -> DStep -> Double -> IO ()
#if HH
improveStep tables ss val = do
        ho <- getHash hhT ss
        addHash hhT ss $
            case ho of
                Nothing  -> (val, 1)
                Just hho -> (+ val) *** (+ 1) $ hho
    where
        hhT = hhTable tables
#else
improveStep _ _ _ = return ()
#endif

-- Structure of Hash Tables

data MCTSTables = MCTSTables { ttTable :: TTable
                             , hhTable :: HHTable
                             }

newHTables :: Int -> IO MCTSTables
newHTables size = do
        tt <- newTT size
        hh <- newHH
        return MCTSTables { ttTable = tt, hhTable = hh }
