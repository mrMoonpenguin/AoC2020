main :: IO ()
main = do
  contents <- readFile "inputDay9.txt"
  let lst = map readInt . lines $ contents
  let outlier = findOutlier 0 lst
  print outlier
  print $ findContiguousSet lst outlier 0 0

readInt :: String -> Int
readInt = read

findOutlier :: Int -> [Int] -> Int
findOutlier pos lst
  | pos < 25 = findOutlier (pos + 1) lst
  | twoSum (drop (pos - 25) (take pos lst)) (lst !! pos) = findOutlier (pos + 1) lst
  | otherwise = lst !! pos

twoSum :: [Int] -> Int -> Bool
twoSum lst n = not . null $ [i + j | i <- lst, j <- lst, i /= j, i + j == n]

findContiguousSet :: [Int] -> Int -> Int -> Int -> Int
findContiguousSet lst n lo hi
  | sum window == n = minimum window + maximum window
  | sum window > n = findContiguousSet lst n (lo + 1) hi
  | sum window < n = findContiguousSet lst n lo (hi + 1)
  where
    window = drop lo (take hi lst)
