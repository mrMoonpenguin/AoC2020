import Data.List.Utils (replace)
import Data.Maybe
import Text.Regex

main :: IO ()
main = do
  contents <- readFile "inputDay2.txt"
  print $ check2 $ parseInput $ lines contents

readInt :: String -> Int
readInt = read

strToChar :: String -> Char
strToChar [x] = x

parseInput :: [String] -> [[String]]
parseInput = map (fromJust . matchRegex (mkRegex "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)"))

check :: [[String]] -> Int
check lst = length $ filter f lst

f :: [String] -> Bool
f l = n >= mi && n <= ma
  where
    mi = readInt (head l)
    ma = readInt (l !! 1)
    c = strToChar (l !! 2)
    s = l !! 3
    n = length (filter (== c) s)

check2 :: [[String]] -> Int
check2 lst = length $ filter g lst

g :: [String] -> Bool
g l = n < length s && m < length s && xor (s !! n == c) (s !! m == c)
  where
    n = readInt (head l) - 1
    m = readInt (l !! 1) - 1
    c = strToChar (l !! 2)
    s = l !! 3

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)
