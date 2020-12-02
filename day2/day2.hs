import Data.List.Split
import Data.List.Utils (replace)

main :: IO ()
main = do
  contents <- readFile "inputDay2.txt"
  let input = lines contents
  print $ check2 $ parseInput input

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

check2 :: [[String]] -> Int
check2 lst = length $ filter g lst

g :: [String] -> Bool
g l = n < length s && m < length s && xor (s !! n == c) (s !! m == c)
  where
    n = readInt (head l) - 1
    m = readInt (l !! 1) - 1
    c = head (l !! 2)
    s = l !! 3

xor :: Bool -> Bool -> Bool
xor a b = (a && (not b)) || ((not a) && b)

-- check :: [[String]] -> Int
-- check lst = length $ filter f lst
