import System.IO
import Control.Monad

main :: IO ()
main = do
        contents <- readFile "inputDay1.txt"
        print . findSum2 . map readInt . words $ contents

readInt :: String -> Int
readInt = read

findSum :: [Int] -> Int
findSum lst = head [x * y | x <- lst, y <- lst, x + y == 2020]

findSum2 :: [Int] -> Int
findSum2 lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z == 2020]
