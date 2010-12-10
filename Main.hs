module Main (main) where

import Control.Concurrent
import Data.Array ((!))
import System.IO
import System.Mem
import BitRepresenation
import Hash (resetHash)
import MTDf

ltrim :: String -> String
ltrim = dropWhile (== ' ')

firstWord :: String -> (String, String)
firstWord str = f [] $ ltrim str
    where
        f word [] = (word, [])
        f word (s:ss) | s == ' '  = (word, ss)
                      | otherwise = f (word ++ [s]) ss

getValue :: Read a => String -> a
getValue str = case firstWord str of
                    ("value", rest) -> read rest
                    _ -> read str

data Game = Game { timePerMove :: Int, startingReserve :: Int
                 , percentUnusedToReserve :: Int, maxReserve :: Int
                 , maxLenghtOfGame :: Int, maxTurns :: Int, maxTurnTime :: Int
                 , quit :: Bool, board :: Board, playerColor :: Player
                 , hashSize :: Int}
                 deriving (Show)


aeiSetposition :: Game -> String -> Game
aeiSetposition game flatBoard = game { playerColor = playerFromChar $ head col
                                      , board = newBoard }
    where
        (col, flatBoard') = firstWord flatBoard
        newBoard = parseFlatBoard $ ltrim flatBoard'

aeiMakemove :: Game -> String -> Game
aeiMakemove game move
        | (hash.board) game == 0 = game { board = parseBoard move }
        | (whole (board game)) ! Silver == 0 = game { board = fst board2 }
        | otherwise =  game { board = fst board1 }
    where
        board1 = makeMove (board game) $ filter notTrapping $ map parseStep $ words move
        board2 = makeMove (board game) $ map (positionToStep.parsePosition) $ words move

        notTrapping (Step _ _ _ to) = to /= 0
        notTrapping _ = True

startSilver, startGold :: String
startSilver = "ra8 db8 rc8 cd8 ce8 rf8 dg8 rh8 ra7 hb7 rc7 ed7 me7 rf7 hg7 rh7 "
startGold   = "Ra1 Db1 Rc1 Cd1 Ce1 Rf1 Dg1 Rh1 Ra2 Hb2 Rc2 Md2 Ee2 Rf2 Hg2 Rh2 "

aeiGo :: Game -> IO Game
aeiGo game | hash (board game) == 0 = do
                putStrLn ("bestmove " ++ startGold)
                return game
           | (whole (board game)) ! Silver == 0 = do
                putStrLn ("bestmove " ++ startSilver)
                return game { playerColor = Silver }
           | otherwise = do
                mvar <- newMVar ([],0)
                thread <- forkOS $ search (board game) (playerColor game) mvar
                threadDelay (3000000 * (timePerMove game) `div` 4)
                (pv, val) <- takeMVar mvar
                killThread thread

                putStrLn $ "info bestscore " ++ show val
                putStrLn $ "bestmove " ++ (unwords $ map show $ justOneMove pv)
                return game
    where
        justOneMove :: DMove -> Move
        justOneMove pv = snd $ makeMove (board game) $ justOneMove' pv 4

        justOneMove' :: DMove -> Int -> Move
        justOneMove' [] _ = []
        justOneMove' (s:ss) n
            | n <= 0 = []
            | otherwise = case s of
                 (s1, Pass) -> s1 : (justOneMove' ss (n-1))
                 (s1@(Step pie1 pl1 _ _), s2@(Step pie2 _ _ _)) ->
                    if (pl1 == pl && pie1 > pie2) || (pl1 /= pl && pie1 < pie2)
                        then [s1,s2] ++ justOneMove' ss (n-2)
                        else []
                 _ -> error "Inner error in aeiGo"

        pl = playerColor game

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
                    ("tcmove", time)    -> return game { timePerMove = getValue time }
                    ("tcreserve", time) -> return game { startingReserve = getValue time}
                    ("tcpercent", time) -> return game { percentUnusedToReserve = getValue time }
                    ("tcmax", time)     -> return game { maxReserve = getValue time }
                    ("tctotal", time)   -> return game { maxLenghtOfGame = getValue time }
                    ("tcturns", turns)  -> return game { maxTurns = getValue turns }
                    ("tcturntime", time)-> return game { maxTurnTime = getValue time }

                    ("hash",size) -> do
                            let size' = getValue size
                            resetHash (size' `div` 5)
                            return game { hashSize = size' }
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
                    _ -> {- putStrLn "log Warning: unsupported setoption" >> -} return game
            _ -> putStrLn "log Error: corrupted 'setoption name <id> [value <x>]' command"
                 >> return game
    "newgame"     -> return game { board = parseBoard "", playerColor = Gold }
    "setposition" -> return $ aeiSetposition game line
    "makemove"    -> return $ aeiMakemove game line
    "go" -> if (fst.firstWord) line == "ponder"
                then do
                    performGC
                    return game
                else aeiGo game
    -- "stop" -> -- jak?
    "debug" -> case firstWord line of
                ("board",kind) -> do
                    putStrLn $ unlines $ map ("info board "++)
                             $ lines $ displayBoard (board game) (kind /= "flat")
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
    if quit game'
        then return ()
        else communicate game'

main :: IO ()
main = do
    resetHash 0
    communicate game
    where
        game = Game { timePerMove = 20, startingReserve = 20
                    , percentUnusedToReserve = 100, maxReserve = 10
                    , maxLenghtOfGame = -1, maxTurns = -1, maxTurnTime = -1
                    , quit = False, board = parseBoard "", playerColor = Gold
                    , hashSize = 100 }
