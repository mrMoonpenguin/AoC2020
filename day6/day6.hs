import Data.List
import Data.List.Split
import Data.List.Utils (replace)

-- -- part 1
-- main :: IO ()
-- main =
--   do
--     contents <- readFile "inputDay6.txt"
--     print . foldr ((+) . length . nub . replace "\n" "") 0 . splitOn "\n\n" $ contents

-- part 2
main :: IO ()
main =
  do
    contents <- readFile "inputDay6.txt"
    print . foldr ((+) . length . foldr1 intersect . words) 0 . splitOn "\n\n" $ contents