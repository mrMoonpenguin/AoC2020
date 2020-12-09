main :: IO ()
main = do
  contents <- readFile "inputDay9.txt"
  print . findOutlier 0 . map readInt . lines $ contents

readInt :: String -> Int
readInt = read

findOutlier :: Int -> [Int] -> Int
findOutlier pos lst
  | pos < 25 = findOutlier (pos + 1) lst
  | twoSum (drop (pos - 25) (take pos lst)) (lst !! pos) = findOutlier (pos + 1) lst
  | otherwise = lst !! pos

twoSum :: [Int] -> Int -> Bool
twoSum lst n = not . null $ [i + j | i <- lst, j <- lst, i /= j, i + j == n]