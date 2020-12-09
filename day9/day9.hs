import Data.Maybe

main :: IO ()
main = do
  contents <- readFile "inputDay9.txt"
  let lst = map readInt . lines $ contents
  let outlier = findOutlier 0 lst
  print $ findContiguousSet lst outlier

readInt :: String -> Int
readInt = read

findOutlier :: Int -> [Int] -> Int
findOutlier pos lst
  | pos < 25 = findOutlier (pos + 1) lst
  | twoSum (drop (pos - 25) (take pos lst)) (lst !! pos) = findOutlier (pos + 1) lst
  | otherwise = lst !! pos

twoSum :: [Int] -> Int -> Bool
twoSum lst n = not . null $ [i + j | i <- lst, j <- lst, i /= j, i + j == n]

findContiguousSet :: [Int] -> Int -> Int
findContiguousSet (x : xs) n =
  if isNothing res
    then findContiguousSet xs n
    else maximum (fromJust res) + minimum (fromJust res)
  where
    res = findContiguousSet' xs n [x]

findContiguousSet' :: [Int] -> Int -> [Int] -> Maybe [Int]
findContiguousSet' (x : xs) n acc
  | acc' == n = Just (x : acc)
  | acc' < n = findContiguousSet' xs n (x : acc)
  | acc' > n = Nothing
  where
    acc' = x + sum acc
