import Data.List
import Data.List.Split
import Data.List.Utils (replace)

-- -- part 1
-- main :: IO ()
-- main = readFile "inputDay6.txt" >>= print . foldr ((+) . length . nub . replace "\n" "") 0 . splitOn "\n\n"

-- part 2
main :: IO ()
main = readFile "inputDay6.txt" >>= print . foldr ((+) . length . foldr1 intersect . words) 0 . splitOn "\n\n"