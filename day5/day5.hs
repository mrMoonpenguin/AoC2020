import Data.Bits

main :: IO ()
main =
  do
    contents <- readFile "inputDay5.txt"
    print . findSeat . parseInput . lines $ contents

parseInput :: [String] -> [Int]
parseInput lst = [r * 8 + c | s <- lst, let r = binSearch (take 7 s) 0 127, let c = binSearch (drop 7 s) 0 7]

binSearch :: String -> Int -> Int -> Int
binSearch [] m n = m
binSearch (s : ss) m n
  | s == 'F' || s == 'L' = binSearch ss m (n - x)
  | s == 'B' || s == 'R' = binSearch ss (m + x) n
  where
    x = div (n - m + 1) 2

findSeat :: [Int] -> Int
findSeat lst = (+ 1) $ head $ filter (\x -> notElem (x + 1) lst && elem (x + 2) lst) lst
