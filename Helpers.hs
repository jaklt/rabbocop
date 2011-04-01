module Helpers (
    ltrim,
    firstWord,
    justOneMove
) where

import BitRepresentation

ltrim :: String -> String
ltrim = dropWhile (== ' ')

firstWord :: String -> (String, String)
firstWord str = f [] $ ltrim str
    where
        f word [] = (word, [])
        f word (s:ss) | s == ' '  = (word, ss)
                      | otherwise = f (word ++ [s]) ss

justOneMove :: Board -> DMove -> Move
justOneMove b pv = snd $ makeMove b $ justOneMove' pv 4
    where
        justOneMove' :: DMove -> Int -> Move
        justOneMove' [] _ = []
        justOneMove' (s:ss) n
            | n <= 0 = []
            | otherwise = case s of
                 (s1, Pass) -> s1 : justOneMove' ss (n-1)
                 (s1@(Step pie1 pl1 _ _), s2@(Step pie2 _ _ _)) ->
                    if (pl1 == pl && pie1 > pie2) || (pl1 /= pl && pie1 < pie2)
                        then [s1,s2] ++ justOneMove' ss (n-2)
                        else []
                 _ -> error "Inner error in aeiGo"

        pl = mySide b
