import Data.List
import Data.List.Split
import Data.List.Utils (replace)

main :: IO ()
main =
  do
    contents <- readFile "inputDay6.txt"
    print . foldr ((\x y -> length x + y) . nub . replace "\n" "") 0 . splitOn "\n\n" $ contents