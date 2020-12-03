main :: IO ()
main =
  do
    contents <- readFile "inputDay3.txt"
    let input = lines contents
    print ((countTrees input 0 1 0) * (countTrees input 0 3 0) * (countTrees input 0 5 0) * (countTrees input 0 7 0) * (countTrees' input 0 1 0))

-- delta_y = 1
countTrees :: [String] -> Int -> Int -> Int -> Int
countTrees [] _ _ acc = acc
countTrees (s : ss) x d acc
  | s !! x == '#' = countTrees ss x' d (acc + 1)
  | otherwise = countTrees ss x' d acc
  where
    x' = mod (x + d) (length s)

-- delta_y = 2
countTrees' :: [String] -> Int -> Int -> Int -> Int
countTrees' [] _ _ acc = acc
countTrees' (s : []) _ _ acc = acc
countTrees' (s1 : s2 : ss) x d acc
  | s1 !! x == '#' = countTrees' ss x' d (acc + 1)
  | otherwise = countTrees' ss x' d acc
  where
    x' = mod (x + d) (length s1)