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
    , constructMove   -- :: MMTree -> Int -> IO DMove
    , improveTree     -- :: MCTSTables -> MMTree -> Int -> IO Double
    , descendByUCB1   -- :: MCTSTables -> MMTree -> IO MMTree
    , valueUCB        -- :: MCTSTables -> MMTree -> Int -> Int -> IO Double
    , createNode      -- :: MCTSTables -> MMTree -> Double -> Int -> IO ()
    , nodeValue       -- :: MMTree -> IO Double
    , nodeVisitCount  -- :: MMTree -> IO Int
    , nodeTreeNode    -- :: MMTree -> IO TreeNode
    , newHTables      -- :: Int -> IO MCTSTables
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
              | Node { children   :: [MMTree]    -- ^ steps from this
                     , value      :: MVar Double -- ^ actual value of node
                     , visitCount :: MVar Int
                     }


iNFINITY' :: Num a => a
iNFINITY' = iNFINITY * iNFINITY

newSearch :: Int -> IO SearchEngine
newSearch = return . search

search :: Int                  -- ^ table size
       -> Board                -- ^ starting position
       -> MVar (DMove, String) -- ^ best results to store here
       -> IO ()
search tableSize b mv = do
        newLeaf <- newMVar Leaf
        tables <- newHTables tableSize
        search' MT { board = b
                   , movePhase = (mySide b, 0)
                   , treeNode = newLeaf
                   , step = (Pass, Pass)
                   }
                tables mv

search' :: MMTree -> MCTSTables -> MVar (DMove, String) -> IO ()
search' mt tables mvar = do
        void $ improveTree tables mt 0
        move  <- constructMove tables mt 4
        score <- (/) <$> nodeValue mt <*> (fromIntegral <$> nodeVisitCount mt)
        changeMVar mvar (const (move, show score))
        -- putStrLn $ "info actual " ++ show (move,score)
        search' mt tables mvar

constructMove :: MCTSTables -> MMTree -> Int -> IO DMove
constructMove _ _ 0 = return []
constructMove tables mt n = do
        tn <- nodeTreeNode mt
        case tn of
            _ | player mt /= mySide (board mt) -> return []
            Leaf -> return []
            _    -> do
                    best <- descendByUCB1 tables mt
                    ((step best) :) <$> constructMove tables best (n-1)

improveTree :: MCTSTables -> MMTree -> Int -> IO Double
improveTree tables mt !depth = do
        tn <- nodeTreeNode mt

        case tn of
            Leaf -> do
                val <- normaliseValue <$> getValueByMC (board mt) (movePhase mt)
                createNode tables mt val depth
                improveStep tables (step mt) val
                return val
            root -> do
                if null $ children root
                    -- immobilization
                    then
                        readMVar $ value root

                    else do
                        node <- descendByUCB1 tables mt
                        improvement <- improveTree tables node (depth + 1)

                        improveStep tables (step mt) improvement
                        changeMVar' (value root) (+ improvement)
                        changeMVar' (visitCount root) (+1)
                        return improvement

createNode :: MCTSTables -> MMTree -> Double -> Int -> IO ()
createNode tables mt val depth = do
        fromTT <- getHash tt index

        case fromTT of
            Just tn -> do
                -- update value of the item in TT
                changeMVar' (visitCount tn) (+1)
                changeMVar' (value tn)    (+val)
                changeMVar  (treeNode mt) (const tn)
            Nothing -> do
                -- insert new item to TT with right value
                chls <- mapM (leafFromStep brd mp) steps
                newVisitCount <- newMVar 1
                newVal <- newMVar =<<
                            if null chls -- is immobilised?
                                then normaliseValue
                                     <$> evalImmobilised (board mt) (player mt)
                                else return val

                let tn = Node { children   = chls
                              , value      = newVal
                              , visitCount = newVisitCount
                              }
                changeMVar (treeNode mt) (const tn)
                addHash tt index tn
    where
        steps = generateSteps brd pl (canPushOrPull mp)
        index = (brd, depth, mp)
        brd   = board mt
        mp@(pl,_) = movePhase mt
        tt    = ttTable tables

leafFromStep :: Board -> MovePhase -> (Step, Step) -> IO MMTree
leafFromStep brd mp s@(s1,s2) = do
    newLeaf <- newMVar Leaf
    return $ MT
        { board = fst $ makeMove brd [s1,s2]
        , movePhase = stepInMove mp s2
        , treeNode = newLeaf
        , step = s
        }

-- | if immobilised returns first_argument
descendByUCB1 :: MCTSTables -> MMTree -> IO MMTree
descendByUCB1 tables mt = do
        tn <- nodeTreeNode mt
        let chs = children tn
        let quant = player mt <#> Gold

        case chs of
            [] -> return mt -- immobilization
            _  -> do
                vc <- readMVar $ visitCount tn
                descendByUCB1' tables chs vc quant

-- | first argument have to be not empty
descendByUCB1' :: MCTSTables -> [MMTree] -> Int -> Int -> IO MMTree
descendByUCB1' tables mms nb quant = do
        valHMms <- valueUCB tables hMms nb quant
        fst <$> foldM (accumUCB tables nb quant) (hMms, valHMms) (tail mms)
    where
        hMms = head mms

accumUCB :: MCTSTables -> Int -> Int -> (MMTree, Double) -> MMTree
         -> IO (MMTree, Double)
accumUCB tables count quant (best, bestValue) mt = do
        nodeVal <- valueUCB tables mt count quant

        if nodeVal > bestValue
            then return (mt, nodeVal)
            else return (best, bestValue)

valueUCB :: MCTSTables -> MMTree -> Int -> Int -> IO Double
valueUCB tables mt count quant = do
        tn <- nodeTreeNode mt
#if HH
        histValPair <- getHash (hhTable tables) $ step mt
        let histVal = fromMaybe 0 $ uncurry (/) <$> histValPair
#endif

        case tn of
            Leaf -> return 0.9 -- INFINITY'
            Node { children = [] } -> nodeValue mt
            _ -> do
                nb <- fromIntegral <$> readMVar (visitCount tn)
                vl <- readMVar (value tn)
                return $ (quant' * vl / nb) + 0.01 * sqrt (log cn / nb)
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
nodeValue mt = readMVar (treeNode mt) >>= readMVar . value

nodeVisitCount :: MMTree -> IO Int
nodeVisitCount mt = readMVar (treeNode mt) >>= readMVar . visitCount

nodeTreeNode :: MMTree -> IO TreeNode
nodeTreeNode = readMVar . treeNode

changeMVar :: MVar a -> (a -> a) -> IO ()
changeMVar mv f = modifyMVar_ mv $ return . f

changeMVar' :: MVar a -> (a -> a) -> IO ()
changeMVar' mv f = modifyMVar_ mv $ (seq <$> id <*> return) . f


-- Implementation of Transposition table

type TTable = HTable TreeNode TTObject (Board, Int, MovePhase)

data TTObject = TTO { board0    :: Board
                    , treeNode0 :: TreeNode
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

ttSaveEntry :: TreeNode -> (Board, Int, MovePhase) -> TTObject
ttSaveEntry tn (b, d, mp) =
        TTO { board0    = b
            , treeNode0 = tn
            , depth0    = d
            , phase     = mp
            }

-- Implementation of History heuristics table

type HHTable = HTable (Double,Double) HHObject (Step, Step)

data HHObject = HHO { hhValue :: (Double, Double)
                    , step0   :: (Step, Step)
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

hhSaveEntry :: (Double, Double) -> (Step, Step) -> HHObject
hhSaveEntry val st = HHO { hhValue = val
                         , step0   = st
                         }

improveStep :: MCTSTables -> (Step, Step) -> Double -> IO ()
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
