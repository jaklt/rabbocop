module Main (main) where

import Data.Word
import Data.Bits
import System.Environment
import Data.Char
import Data.List hiding (find)
import Numeric

tab :: String
tab = "    "

trim :: String -> String
trim = filter (`notElem` [' ', '\n', '\t'])

split :: Eq a => a -> [a] -> [[a]]
split c ys = go ys []
    where
        go [] [] = []
        go [] a  = [reverse a]
        go (x:xs) a | x == c = reverse a : go xs []
                    | otherwise = go xs (x:a)

indent :: [String] -> [String]
indent = map (tab ++)

transform :: String -> String
transform = unlines . map parseDefinition . blockDefinitions . lines

blockDefinitions :: [String] -> [[String]]
blockDefinitions [] = []
blockDefinitions ss =  [take 12 start] ++ blockDefinitions (drop 12 start)
    where
        start = dropWhile (null . trim) ss
        
parseDefinition :: [String] -> String
parseDefinition definition@(name:defs:_:table) =
        tab ++ "/*" ++ unlines (indent definition) ++ tab ++ "*/" ++ code 
    where
        code = unlines . indent $ generatedTables ++ [generatedSums]
        generatedTables = map (makeTable table defs . toLower) $ filter (`elem` ['A'..'Z']) name
        generatedSums   = makeSums name
parseDefinition _ = error "Parse error"

makeTable :: [String] -> String -> Char -> String
makeTable table defs c = ("\n" ++) $ unlines $ reverse $ addSemicolon $ reverse $ indent res
    where
        flatT = filter (`notElem` [' ', '\t', '-', '|', '+']) $ concat table
        bits = makeBits flatT
        parsedD = makeDefs defs
        comb = combine parsedD c bits

        addSemicolon s = (head s ++ ";") : tail s

        res :: [String]
        res | null comb = []
            | otherwise =
                ("tmp = " ++ head comb) : map ("    + " ++) (tail comb)

combine :: [(Char, Double)] -> Char -> [(Char, Word64)] -> [String]
combine defs ch = foldr cmb []
    where
        cmb (c,mask) s =
            s ++ [num c ++ " * bit_count("++[ch]++" & 0x" ++ (showHex mask "LLU)")]

        num c = let f = find c in (if f < 0 then "" else " ") ++ (show (find c))
        find c = foldr (\(a,b) r -> if c == a then b else r) 0 defs

makeDefs :: String -> [(Char, Double)]
makeDefs s = map (make . split '=') $ split ',' s
    where
        make [symb, koef] = (head $ trim symb, read koef)
        make _ = error "Parse error"

makeBits :: String -> [(Char, Word64)]
makeBits s = snd $ foldr accum (0,[]) s
    where
        accum :: Char -> (Int, [(Char, Word64)]) -> (Int, [(Char, Word64)])
        accum c (n,res) = (n+1, repRes c n res)

repRes :: Char -> Int -> [(Char, Word64)] -> [(Char, Word64)]
repRes c n res
    | null res = [(c, bit n)]
    | c == fst (head res) = (c, snd (head res) .|. bit n) : tail res
    | otherwise = head res : repRes c n (tail res)

makeSums :: String -> String
makeSums name = "sum += tmp * " ++ weights
    where
        ns = filter (not.null) $ map (filter isAlpha) $ words name
        weights = concat $ 
                    [ "(" 
                    , concat $ intersperse " + " $ map weightTable ns
                    , ");"
                    ]

        weightTable "Rabbit" = "rabbit_weight"
        weightTable n        = "weight_table[" ++ map toUpper n ++ "]"

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn . transform
