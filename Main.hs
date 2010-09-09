module Main where

import BitRepresenation

-- search :: Board -> Int -> Move
-- search b 0 = eval b
-- search b n = findBest steps
--          where
--              -- | Must differentiate if this is gold or silver step
--              -- | Must containt beta-cutof
--              findBest :: [Step] -> Move
--              findBest xs = foldl findBest' b xs -- radsi foldr !!
--              findBest' b x = ...
--              steps = generateSteps b
-- eval :: Board -> Int

main :: IO ()
main = do
    putStrLn "none"
    putStrLn $ show $ [Elephant .. Horse]
