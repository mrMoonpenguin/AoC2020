import Data.List.Split
import Data.List.Utils (replace)

main :: IO ()
main = do
  contents <- readFile "inputDay2.txt"
  let input = lines contents
  print $ check $ parseInput input

readInt :: String -> Int
readInt = read

parseInput :: [String] -> [[String]]
parseInput [] = []
parseInput (s : ss) = [splitOn " " s2] ++ parseInput ss
  where
    s1 = [x | x <- s, x /= ':']
    s2 = replace "-" " " s1

check :: [[String]] -> Int
check lst = length $ filter f lst

f :: [String] -> Bool
f l = n >= mi && n <= ma
  where
    mi = readInt (head l)
    ma = readInt (l !! 1)
    c = head (l !! 2)
    s = l !! 3
    n = length (filter (== c) s)

-- check :: [[String]] -> Int
-- check lst = length $ filter f lst
