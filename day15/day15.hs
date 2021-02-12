import Data.List
import Data.Maybe

input :: [Int]
input = [20, 0, 1, 11, 6, 3]

input' :: [Int]
input' = [1, 2, 3]

limit :: Int
limit = 2020

main :: IO ()
main = putStrLn (show (process (reverse input) (limit - length input)))

process :: [Int] -> Int -> Int
process lst 0 = head lst 
process lst@(x : xs) lim = 
  if x `elem` xs 
    then 
      let len = length lst 
          idx = fromJust (findIndex (==x) xs) + 1 
      in 
      process (idx : lst) (lim - 1)
    else process (0 : lst) (lim - 1)