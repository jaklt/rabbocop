module Main (main) where

import Data.Word
import Data.Bits
import System.Environment
import Data.Char
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

transform :: Bool -> String -> String
transform rev = unlines . map (parseDefinition rev) . blockDefinitions . lines

blockDefinitions :: [String] -> [[String]]
blockDefinitions [] = []
blockDefinitions ss = [take 12 start | length start >= 12]
                      ++ blockDefinitions (drop 12 start)
    where
        start = dropWhile (null . trim) ss

parseDefinition :: Bool -> [String] -> String
parseDefinition rev (name:defs:_:table) =
        tab ++ "/*" ++ unlines (indent (name:defs:table')) ++ tab ++ "*/" ++ code
    where
        code = unlines . indent $ generatedTables
        generatedTables = map (makeTable table' defs) name'
        table' = (if rev then reverse else id) table
        name' = filter (not.null) $ map (filter isAlpha) $ words name
parseDefinition _ _ = error "Parse error"

makeTable :: [String] -> String -> String -> String
makeTable table defs name =
        ("\n" ++).(++ assign) $ unlines $ reverse $ addSemicolon $ reverse
                              $ indent res
    where
        flatT = filter (`notElem` [' ', '\t', '-', '|', '+']) $ concat table
        bits = makeBits flatT
        parsedD = makeDefs defs
        (c:_) = filter (`elem` ['A'..'Z']) name
        comb = combine parsedD c bits
        assign = tab ++ makeSum name ++ ";"

        addSemicolon s = (head s ++ ";") : tail s

        res :: [String]
        res | null comb = []
            | otherwise =
                ("tmp = " ++ head comb) : map ("    + " ++) (tail comb)

combine :: [(Char, Double)] -> Char -> [(Char, Word64)] -> [String]
combine defs ch = foldr cmb []
    where
        cmb (c,mask) s =
            s ++ [num c ++ " * bit_count("++[toLower ch]
                        ++" & 0x" ++ (long $ showHex mask "") ++ "LLU)"]

        num c = let f = find c in (if f < 0 then "" else " ") ++ (show (find c))
        find c = foldr (\(a,b) r -> if c == a then b else r) 0 defs
        long m = replicate (16 - length m) '0' ++ m

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

makeSum :: String -> String
makeSum name = "sum += tmp * " ++ weightTable name
    where
        weightTable "Rabbit" = "rabbit_weight"
        weightTable n        = "weight_table[" ++ map toUpper n ++ "]"

main :: IO ()
main = do
    (file:revStr) <- getArgs
    let rev = case revStr of ["REVERSE"] -> True; _ -> False

    readFile file >>= putStrLn . (transform rev)
