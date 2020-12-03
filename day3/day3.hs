-- -- part 1
-- main :: IO ()
-- main =
--   do
--     contents <- readFile "inputDay3.txt"
--     let input = lines contents
--     print $ countTrees input 0 0 3 1

-- part 2
main :: IO ()
main =
  do
    contents <- readFile "inputDay3.txt"
    let input = lines contents
    print $ countTrees input 0 0 1 1 * countTrees input 0 0 3 1 * countTrees input 0 0 5 1 * countTrees input 0 0 7 1 * countTrees input 0 0 1 2

-- general solution
countTrees :: [String] -> Int -> Int -> Int -> Int -> Int
countTrees lst x y dx dy
  | y >= l = 0
  | otherwise = n + countTrees lst x' y' dx dy
  where
    l = length lst
    x' = mod (x + dx) (length (head lst))
    y' = y + dy
    n = if (lst !! y) !! x == '#' then 1 else 0
