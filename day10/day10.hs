import Data.List

main :: IO ()
main = do
  contents <- readFile "inputDay10.txt"
  let lst = sort . map readInt . lines $ contents
  let lst' = zip (lst ++ [last lst + 3]) ((0 : lst) ++ [last lst + 3])
  print $ joltage lst'

readInt :: String -> Int
readInt = read

joltage :: [(Int, Int)] -> Int
joltage lst = length (filter (\(x, y) -> x - y == 3) lst) * length (filter (\(x, y) -> x - y == 1) lst)