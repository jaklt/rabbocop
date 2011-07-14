module AEI
    ( SearchEngine
    , runAEIInterface
    ) where

import Control.Concurrent
import Control.Monad (unless)
import System.IO (hFlush, stdout)
import System.Mem (performGC)

import Eval.BitEval (forbidBoard)
import Bits.BitRepresentation
import Helpers

type SearchEngine = Board -> MVar (DMove, String) -> IO ()

data Game = Game { timePerMove :: Int
                 , percentUnusedToReserve :: Int
                 , startingReserve :: Int
                 , maxLenghtOfGame :: Int
                 , maxReserve  :: Int
                 , maxTurns    :: Int
                 , maxTurnTime :: Int
                 , quit        :: Bool
                 , board       :: Board
                 , engine      :: SearchEngine
                 , newEngine   :: Int -> IO (SearchEngine)
                 }


getValue :: Read a => String -> a
getValue str = case firstWord str of
                    ("value", rest) -> read rest
                    _ -> read str

aeiSetposition :: Game -> String -> Game
aeiSetposition game flatBoard = game { board = newBoard }
    where
        (col, flatBoard') = firstWord flatBoard
        plCol = playerFromChar $ head col
        newBoard = parseFlatBoard plCol $ ltrim flatBoard'

aeiMakemove :: Game -> String -> Game
aeiMakemove game move
        | board game == EmptyBoard = game { board = parseBoard Silver move }
        | (hash.board) game == 0   = game { board = parseBoard   Gold move }
        | whole (board game) Silver == 0 = game { board = fst board2 }
        | otherwise =  game { board = fst board1 }
    where
        board1 = makeMove (board game) $ filter notTrapping
               $ map parseStep $ words move
        board2 = makeMove (board game) $ map (positionToStep.parsePosition)
               $ words move

        notTrapping (Step _ _ _ to) = to /= 0
        notTrapping _ = True

-- TODO zmenit startovni pozici
startSilver, startGold :: String
startSilver = "ra8 db8 rc8 cd8 ce8 rf8 dg8 rh8 ra7 hb7 rc7 ed7 me7 rf7 hg7 rh7 "
startGold   = "Ra1 Db1 Rc1 Cd1 Ce1 Rf1 Dg1 Rh1 Ra2 Hb2 Rc2 Md2 Ee2 Rf2 Hg2 Rh2 "

aeiGo :: Game -> IO Game
aeiGo game | board game == EmptyBoard  = do
                putStrLn ("bestmove " ++ startGold)
                return game { board = parseBoard Gold "" }
           | whole (board game) Silver == 0 = do
                putStrLn ("bestmove " ++ startSilver)
                return game
           | otherwise = do
                mvar <- newMVar ([],"Nothing computed")
                forbidBoard $ board game
                thread <- forkIO $ engine game (board game) mvar
                threadDelay (3000000 * timePerMove game `div` 4)
                (pv, val) <- takeMVar mvar
                killThread thread

                putStrLn $ "info bestscore " ++ val
                putStrLn $ "bestmove "
                            ++ unwords (map show $ justOneMove (board game) pv)
                return game

action :: String -> String -> Game -> IO Game
action str line game = case str of
    "aei" -> putStrLn "protocol-version 1\nid name Rabbocop\nid author JackeLee\naeiok"
             >> return game
    "isready" -> putStrLn "readyok"
                 >> return game
    "setoption" ->
        case firstWord line of
            ("name", line') ->
                case firstWord line' of
                    ("tcmove", time)    ->
                        return game { timePerMove = getValue time }
                    ("tcreserve", time) ->
                        return game { startingReserve = getValue time}
                    ("tcpercent", time) ->
                        return game { percentUnusedToReserve = getValue time }
                    ("tcmax", time)     ->
                        return game { maxReserve = getValue time }
                    ("tctotal", time)   ->
                        return game { maxLenghtOfGame = getValue time }
                    ("tcturns", turns)  ->
                        return game { maxTurns = getValue turns }
                    ("tcturntime", time)->
                        return game { maxTurnTime = getValue time }

                    ("hash", size) -> do
                            search <- newEngine game $ getValue size
                            return game { engine = search }
                    {-
                    ("greserve",_) -> return game
                    ("sreserve",_) -> return game
                    ("gused",_) -> return game
                    ("sused",_) -> return game
                    ("lastmoveused",_) -> return game
                    ("moveused",_) -> return game
                    ("opponent",_) -> return game
                    ("opponent_rating",_) -> return game
                    ("rating",_) -> return game
                    ("rated",_) -> return game
                    ("event",_) -> return game
                    ("depth",_) -> return game
                    -}
                    _ -> {- putStrLn "log Warning: unsupported setoption" >> -}
                         return game
            _ -> putStrLn "log Error: corrupted 'setoption name <id> [value <x>]' command"
                 >> return game
    "newgame"     -> return game { board = EmptyBoard }
    "setposition" -> return $ aeiSetposition game line
    "makemove"    -> return $ aeiMakemove game line
    "go" -> if (fst.firstWord) line == "ponder"
                then do
                    _ <- forkIO performGC
                    return game
                else aeiGo game
    -- "stop" -> -- jak?
    "debug" -> case firstWord line of
                ("board",kind) -> do
                    putStrLn $ unlines $ map ("info board "++) $ lines
                             $ displayBoard (board game) (kind /= "flat")
                    return game
                _ -> return game
    "quit" -> return $ game { quit = True }
    _ -> putStrLn "log Error: unknown command" >> return game

communicate :: Game -> IO ()
communicate game = game `seq` do
    hFlush stdout
    performGC
    l <- getLine
    (str, line) <- return $ firstWord l
    game' <- action str line game
    unless (quit game') $ communicate game'

runAEIInterface :: (Int -> IO (SearchEngine)) -> IO ()
runAEIInterface newSearch = do
    search <- newSearch 1000
    communicate
        Game { timePerMove = 20
             , startingReserve = 20
             , percentUnusedToReserve = 100
             , maxReserve = 10
             , maxLenghtOfGame = -1
             , maxTurns = -1
             , maxTurnTime = -1
             , quit = False
             , board = EmptyBoard
             , engine = search
             , newEngine = newSearch
             }
