{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module AlphaBeta
    ( ABTables
    , alphaBeta
#if CORES > 1
    , alphaBetaParallel
#endif
    , newHTables
    ) where

import Bits.BitRepresentation
import Eval.BitEval
import Hash
import Control.Applicative ((<$>),(<*>))
import Data.Bits
import Data.Int (Int64, Int32)

#if CORES > 1
import Control.Monad (when)
import Helpers (changeMVar)
import Computation
#endif
#ifdef abHH
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
#endif

type KMoves = (DMove, DMove) -- ^ Last two killer moves


alphaBeta :: Board
          -> ABTables
          -> (Int, Int)  -- ^ (alpha, beta)
          -> Int         -- ^ maximal depth
          -> MovePhase
          -> IO (DMove, Int) -- ^ (steps to go, best value)
alphaBeta brd tables' bounds' maxDepth mp =
        proj <$> alphaBeta' T { board = brd
                              , tables = tables'
                              , pvMoves = []
                              , killerMoves = emptyKM
                              , bounds = bounds'
                              , actDepth = 1
                              , remDepth = maxDepth
                              , movePhase = mp
                              , nullPossible = False
                              }
    where
        proj (a,b,_) = (a,b)

#if CORES > 1
-- Perform alphaBeta in parallel with divide and conquer strategy
alphaBetaParallel :: Board
                  -> ABTables
                  -> (Int, Int)  -- ^ (alpha, beta)
                  -> Int         -- ^ maximal depth
                  -> MovePhase
                  -> MVar (DMove, Int) -- ^ (best value, score)
                  -> MVar [ThreadId]   -- ^ pools of working threads
                  -> MVar ()           -- ^ to wake up worker
                  -> IO ()
alphaBetaParallel brd tables' bounds' maxDepth mp mv threads workerMV = do
        -- channel to generated steps from root position
        stepsMV <- newEmptyMVar :: IO (MVar DStep)

        -- thread which will be filling stepsMV
        fillingThread <- forkIO $ fillWithSteps stepsMV brd mp

        -- mvar for shared token and best score so far
        tokenMV <- newMVar (token,-iNFINITY * Gold <#> (fst mp))

        -- for each CORES create thread with eager consuming of stepsMV
        -- and updating tokens killerMoves and bounds
        modifyMVar_ threads $ const $
             mapM (\_ -> forkIO $ oneComp tokenMV fillingThread stepsMV
                                          mv workerMV mp)
                  [1 .. cores :: Int]
    where
        cores = CORES
        token = T { board = brd
                  , tables = tables'
                  , pvMoves = []
                  , killerMoves = emptyKM
                  , bounds = bounds'
                  , actDepth = 1
                  , remDepth = maxDepth
                  , movePhase = mp
                  , nullPossible = True
                  }

fillWithSteps :: MVar DStep -> Board -> MovePhase -> IO ()
fillWithSteps mv brd mp = go steps
    where
        steps = generateSteps brd mp

        go []     = putMVar mv dPass -- all steps were consumed
        go (s:ss) = putMVar mv s >> go ss

oneComp :: MVar (ABToken,Int)   -- ^ shared token and best score
        -> ThreadId             -- ^ fillers thread id
        -> MVar DStep           -- ^ channel
        -> MVar (DMove, Int)    -- ^ to store best value
        -> MVar ()              -- ^ to inform worker about
        -> MovePhase
        -> IO ()
oneComp tokenMV fillerId dsMV bestMV workerMV mp = do
        -- take new step
        ds <- takeMVar dsMV

        case ds of
            -- it's end, so return back dPass as end of computation marker
            (Pass,Pass) -> putMVar dsMV dPass >> putMVar workerMV ()

            s -> do
                -- start search with last version of the token
                (token0,best0) <- readMVar tokenMV
                res@(pv,score,_) <- findBest token0 ([],best0) [s]

                -- The same procedure as in findBest to update bounds, killer
                -- move and best value.
                -- TODO prevent repeating code
                modifyMVar_ tokenMV $ \(token1,best1) -> do
                    let (a,b) = bounds token1 -- old bounds

                    -- updated bounds, value and killerMove
                    let !bounds' | isMaxNode = (max a score, b)
                                 | otherwise = (a, min b score)
                    let bestValue' = cmp best1 score
                    let (_,_,km') = repairKM (killerMoves token1) res

                    -- if we have improved the value, store the result
                    when (best1 /= bestValue')
                        $ changeMVar bestMV $ const (pv, score)

                    when (not $ boundsOK bounds') endComp
                    return ( token1 { bounds = bounds', killerMoves = km' }
                           , bestValue')

                oneComp tokenMV fillerId dsMV bestMV workerMV mp
    where
        isMaxNode = Gold == fst mp
        cmp = if isMaxNode then max else min

        -- when thread decides to prune, it will also kill fillingThread
        endComp = do
            killThread fillerId
            _ <- takeMVar dsMV -- create mark for other threads to stop
            putMVar dsMV dPass

#endif

data ABToken = T
    { board        :: !Board
    , tables       :: !ABTables
    , pvMoves      :: !DMove         -- ^ best Principal Variation (PV)
    , killerMoves  :: !KMoves        -- ^ killer moves
    , bounds       :: !(Int, Int)    -- ^ (alpha, beta)
    , actDepth     :: !Int           -- ^ actual depth
    , remDepth     :: !Int           -- ^ depth remaining
    , movePhase    :: !MovePhase
    , nullPossible :: !Bool          -- ^ true if is null move possible
    }

-- | Returns (steps to go, best value, killer moves)
alphaBeta' :: ABToken -> IO (DMove, Int, KMoves)

alphaBeta' token@T { board=brd, remDepth=rd
                   , bounds=aB@(alpha,beta), movePhase=mp@(pl,_) }
    = do
        -- Search Transposition tables for entry.
        fromTT <- getHash (ttTable (tables token)) ((hash brd),mp)
        let (ttBounds@(al', bet'), maybeBest, ttMove) =
                case fromTT of
                    -- If entry was found, use it's values.
                    -- But if found search result is shallow, don't use it's
                    -- bounds.
                    Just (remD, ttMove', _, _) | remD < rd ->
                        ( aB, Nothing, ttMove')
                    Just (_, ttMove', lowerBound, upperBound) ->
                        ( (lowerBound, upperBound)
                        , maybeResult lowerBound upperBound ttMove'
                        , ttMove'
                        )
                    -- Otherwise use default.
                    Nothing ->
                        (aB, Nothing,[])
        let newAB = (max alpha al', min beta bet')
        forbidden <- isForbidden brd mp

        case maybeBest of
            -- To omit repetitions.
            _ | forbidden ->
                return ([], mySide brd <#> Silver * iNFINITY, emptyKM)

            -- What we have found in TT is enought to end computation
            -- for this node.
            Just bestResult -> return bestResult

            Nothing -> do
                -- Prioritise killer moves and previous successful PV.
                let (headPV,tailPV) = maybeSwapPV ttMove (pvMoves token)
                    prioritised = nullMove ++ headPV ++ headKM'
                    generSteps steps' = prioritised
                                     ++ filter (`notElem` prioritised) steps'

                res <- if rd <= 0 || isEnd brd
                        then do
                            -- First check if game has ended or we have
                            -- reached the leaf, if so run evaluation.
                            e <- eval brd mp
                            return ([], e, [])
                        else do
                            -- If History heuristics (HH) is set for alpha
                            -- beta we order generated steps in first few
                            -- levels.
#ifdef abHH
                            sortedSteps <-
                                    if actDepth token < 4
                                        then do
                                            -- Sort steps by HH values.
                                            evalSteps <- mapM evalS steps
                                            return $ map fst (sortBy cmpHH evalSteps)
                                        else return steps
                            let steps' = generSteps sortedSteps
#else
                            -- Otherwise don't change their order.
                            let steps' = generSteps steps
#endif
                            -- Search in children nodes.
                            findBest token { pvMoves = tailPV
                                           , killerMoves = tailKM
                                           , bounds = newAB }
                                     ([], negInf) steps'

                -- Save value given by children to transposition table.
                addHash (ttTable (tables token)) ((hash brd),mp)
                        (changeTTBounds res rd ttBounds newAB)

                -- Propagate up new killer moves.
                return $ repairKM (killerMoves token) res
    where
        negInf = -iNFINITY * Gold <#> pl
        steps = generateSteps brd mp
        (headKM,tailKM) = headTailKM $ killerMoves token

        -- Use Killer move only if it is valid on given position.
        headKM' = filter ((&&) <$> canMakeDStep brd <*> isStepBy pl) headKM


        -- Check if found result from TT is satisfying.
        maybeResult low upp mv
               | low >= beta  = Just (mv, low, emptyKM)
               | upp <= alpha = Just (mv, upp, emptyKM)
               | otherwise    = Nothing

#ifdef abHH
        cmpHH x y = compare (snd y) (snd x)

        -- From given step create pair (step, its value).
        evalS ds = do
            maybeE <- getHash (hhTable (tables token)) ds
            return (ds, fromMaybe 0 maybeE)
#endif
#ifdef NULL_MOVE
        nullMove = [(Pass,Pass) | nullPossible token, snd mp == 0]
#else
        nullMove = []
#endif

        -- If needed use PV found in TT
        maybeSwapPV ttM [] = firstAsList ttM
        maybeSwapPV  _   pv = firstAsList pv

        firstAsList x = case x of
                            (h:t) -> ([h],t)
                            _     -> ([],[])


emptyKM :: KMoves
emptyKM = ([],[])

headTailKM :: KMoves -> ([DStep], KMoves)
headTailKM km = case km of
        ([],[])      -> ([],    emptyKM)
        (a:as, [])   -> ([a],   (as,[]))
        (a:as, b:bs) -> ([a,b], (as,bs))
        _            -> ([],    emptyKM)

repairKM :: KMoves -> (DMove, Int, DMove) -> (DMove, Int, KMoves)
repairKM (km1,km2) (dm,sc,km') = (dm,sc,kms)
    where
        kms | km' == [] || km1 == km' = (km1,km2)
            | otherwise               = (km',km1)

-- Prepaire recursively computed or eval result to be stored to TT.
-- Returns foursome (depth, PV move, alpha, beta).
changeTTBounds :: (DMove, Int, DMove) -- ^ (PV, value, killer move)
               -> Int          -- ^ depth
               -> (Int, Int)   -- ^ old bounds
               -> (Int, Int)   -- ^ used alphaBeta window
               -> (Int, DMove, Int, Int)
changeTTBounds (mv, score, _) d (ttAlpha, ttBeta) (alpha,beta)
    | score <= alpha = (d, mv, ttAlpha, score)  -- change upper bound in TT
    | score >= beta  = (d, mv, score,  ttBeta)  -- change lower bound in TT
    | alpha < score && score < beta = (d, mv, score, score)
    | otherwise = (d, mv, ttAlpha, ttBeta)


-- | Walk through nodes children and either prune or improve bounds and/or
-- killer moves.
findBest :: ABToken
         -> (DMove, Int) -- ^ actual best result
         -> DMove        -- ^ next steps to try
         -> IO (DMove, Int, DMove) -- ^ (PV, score, killer move)
findBest _ bestResult [] =
        -- TODO update HH information?
        return $ makeTriple bestResult []
findBest token@T { board=brd, bounds=(!a,!b), movePhase=mp }
         best0@(!_, !bestValue) (s@(!_,!s2):ss)
    = do
        -- Make step and search from this position.
        (!childPV, !childValue, childKM) <-
            alphaBeta' token { board     = makeDStep' brd s
                             , remDepth  = remDepth token - ch
                             , movePhase = stepInMove mp s
                             , nullPossible = s /= (Pass,Pass)
                             , actDepth  = actDepth token + ch
                             }

        -- Considering previous result, improve bounds
        let bestValue' = cmp bestValue childValue
        let !bounds' | isMaxNode = (max a childValue, b)
                     | otherwise = (a, min b childValue)
        -- and value.
        let !best' | bestValue /= bestValue' = (s:childPV, childValue)
                   | otherwise               = best0

        if boundsOK bounds' then findBest token { pvMoves = []
                                                , killerMoves = childKM
                                                , bounds = bounds' }
                                          best' ss
                            -- Cut off
                            else do
#ifdef abHH
                                -- Increase value in HH table for given
                                -- step.
                                let acD = actDepth token
                                improveStep (tables token) s (acD*acD)
                                -- TODO compare perormance with 2^acD
#endif
                                return $ makeTriple best' (s:fst childKM)
    where
        isMaxNode = Gold == fst mp
        cmp = if isMaxNode then max else min

        ch = if s2    == Pass then 1 else 2  -- step count change
#ifdef NULL_MOVE
           + if fst s == Pass then 0 else 2
           -- TODO this used alone increases speed, but it's not known if
           -- results are OK
#endif

makeTriple :: (a,b) -> c -> (a,b,c)
makeTriple (a,b) c = (a,b,c)

boundsOK :: (Int, Int) -> Bool
boundsOK (!alpha, !beta) = alpha < beta

-- Implementation of Transpositions table

type TTable = HTable (Int, DMove, Int, Int) TTObject (Int64, MovePhase)

{-|
 - Implementing object for HTable, where we match type parameters as:
 -  e ~ (Int, DMove, Int, Int) { for (depth, PV, lower bound, upper bound) }
 -  o ~ TTObject
 -  i ~ (Int64, Int, MovePhase)  { for hash, depth, move phase }
|-}
data TTObject = TTO { hash0 :: Int64
                    , best  :: (Int, DMove, Int, Int)
                    , phase :: MovePhase
                    }

newTT :: Int -> IO TTable
newTT tableSize = do
        ht <- newHT (`mod` ts)
        return HT
           { table     = ht
           , getEntry  = best
           , isValid   = isValid'
           , key       = key' ts
           , saveEntry = saveEntry'
           }
    where
        -- TableSize - one entry in table has:
        --   * cover = 12B
        --   * one value of information = 12B
        --     + cover for each composite entry
        --   * 4 steps = 4*12 + 4*12B
        -- total: 12 + (12 + (4*12 + 4*12) + 2*12) + 12 + (12 + 2*12) B
        --        = 192B
        ts = (fromIntegral tableSize) * (500000 `div` 200)

isValid' :: (Int64, MovePhase) -> TTObject -> Bool
isValid' (h,mp) e = phase e == mp && hash0 e == h

key' :: Int32 -> (Int64, MovePhase) -> Int32
key' tableSize (h, (pl,s)) = fromIntegral . (`mod` tableSize) $
    fromIntegral h `xor` fromIntegral (playerToInt pl)
    `xor` (fromIntegral s `shift` 4)

saveEntry' :: (Int,DMove, Int, Int) -> (Int64, MovePhase) -> TTObject
saveEntry' b (h,mp) =
        TTO { hash0 = h
            , best  = justNeeded b
            , phase = mp
            }
    where
        justNeeded (d,!a:(!f):(!c):(!e):_,x,y) = (d,[a,f,c,e],x,y)
        justNeeded (d,!a:(!f):(!c):[],x,y)     = (d,[a,f,c],x,y)
        justNeeded (d,!a:(!f):[],x,y)          = (d,[a,f],x,y)
        justNeeded (d,!a:[],x,y)               = (d,[a],x,y)
        justNeeded (d,[],x,y)                  = (d,[],x,y)


-- Implementation of History heuristics table
#ifdef abHH

type HHTable = HTable Int HHObject DStep

data HHObject = HHO { hhValue :: !Int
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

hhSaveEntry :: Int -> DStep -> HHObject
hhSaveEntry val st = HHO { hhValue = val
                         , step0   = st
                         }

-- | Updates statistics for given DStep.
improveStep :: ABTables -> DStep -> Int -> IO ()
improveStep tables' ss val = do
        ho <- getHash hhT ss
        addHash hhT ss $ fromMaybe 0 ho + val
    where
        hhT = hhTable tables'
#endif


-- Structure of Hash Tables

data ABTables = ABTables { ttTable :: TTable
#ifdef abHH
                         , hhTable :: HHTable
#endif
                         }

newHTables :: Int -> IO ABTables
newHTables size = do
        tt <- newTT size
#ifdef abHH
        hh <- newHH
        return ABTables { ttTable = tt, hhTable = hh }
#else
        return ABTables { ttTable = tt }
#endif
