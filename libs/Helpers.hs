module Helpers
    ( ltrim             -- :: String -> String
    , firstWord         -- :: String -> (String, String)
    , justOneMove       -- :: Board -> DMove -> Move
    , manageJustOneMove -- :: Board -> DMove -> (Board, Move)
    , showHeader        -- :: String -> IO ()
    , (~=~)             -- :: Eq a => [a] -> [a] -> Bool
    , printBoard        -- :: Board -> IO ()
    , boolToNum         -- :: Num a => Bool -> a
    , changeMVar        -- :: MVar a -> (a -> a) -> IO ()
    , changeMVar'       -- :: MVar a -> (a -> a) -> IO ()
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Bits.BitRepresentation


ltrim :: String -> String
ltrim = dropWhile (== ' ')

firstWord :: String -> (String, String)
firstWord str = f [] $ ltrim str
    where
        f word [] = (word, [])
        f word (s:ss) | s == ' '  = (word, ss)
                      | otherwise = f (word ++ [s]) ss

justOneMove :: Board -> DMove -> Move
justOneMove b = snd . manageJustOneMove b

manageJustOneMove :: Board -> DMove -> (Board, Move)
manageJustOneMove b pv = makeMove b $ justOneMove' pv 4
    where
        justOneMove' :: DMove -> Int -> Move
        justOneMove' [] _ = []
        justOneMove' (s:ss) n
            | n <= 0 = []
            | otherwise = case s of
                 (s1@(Step _ pl1 _ _), Pass)
                    | pl1 == pl -> s1 : justOneMove' ss (n-1)
                    | otherwise -> []
                 (s1@(Step pie1 pl1 _ _), s2@(Step pie2 _ _ _)) ->
                    if (pl1 == pl && pie1 > pie2) || (pl1 /= pl && pie1 < pie2)
                        then [s1,s2] ++ justOneMove' ss (n-2)
                        else []
                 (Pass, Pass) -> []
                 _ -> error "Inner error found in manageJustOneMove"

        pl = mySide b

showHeader :: String -> IO ()
showHeader h = do
        putStrLn $ '\n' : h
        putStrLn $ map (\_ -> '-') h  -- underline

-- | Are those sets made of lists equal?
(~=~) :: Eq a => [a] -> [a] -> Bool
l1 ~=~ l2 = and $ [length l1 == length l2] ++ map (`elem` l1) l2

printBoard :: Board -> IO ()
printBoard b = putStrLn $ displayBoard b True

boolToNum :: Num a => Bool -> a
boolToNum True = 1
boolToNum _    = 0

changeMVar :: MVar a -> (a -> a) -> IO ()
changeMVar mv f = modifyMVar_ mv $ return . f

changeMVar' :: MVar a -> (a -> a) -> IO ()
changeMVar' mv f = modifyMVar_ mv $ (seq <$> id <*> return) . f
