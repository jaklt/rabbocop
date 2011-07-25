{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module AlphaBeta
    ( ABTables
    , alphaBeta
    , newHTables
    ) where

import Bits.BitRepresentation
import Eval.BitEval
import Hash
import Control.Applicative ((<$>),(<*>))
import Data.Bits
import Data.Int (Int64, Int32)

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
                              , nullPossible = True
                              }
    where
        proj (a,b,_) = (a,b)

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
                    prioritised = headPV ++ nullMove ++ headKM'
                    generSteps steps' = prioritised
                                     ++ filter (`notElem` prioritised) steps'

                res <- if rd <= 0 || isEnd brd
                        then do
                            -- First check if game has ended or we have
                            -- reached the leaf, if so run evaluation.
                            e <- eval brd pl
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
                                           , nullPossible = True
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
findBest _ bestResult [] = return $ makeTriple bestResult []
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
#endif
                                return $ makeTriple best' (s:fst childKM)
    where
        boundsOK (!alpha, !beta) = alpha < beta
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
