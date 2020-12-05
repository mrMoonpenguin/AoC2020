main :: IO ()
main =
  do
    contents <- readFile "inputDay5.txt"
    print . maximum . parseInput . lines $ contents

parseInput :: [String] -> [Int]
parseInput lst = [r * 8 + c | s <- lst, let r = binSearch (take 7 s) 0 127, let c = binSearch (drop 7 s) 0 7]

binSearch :: String -> Int -> Int -> Int
binSearch [] m n = m
binSearch ('F' : ss) m n = binSearch ss m (n - x)
  where
    x = div (n - m + 1) 2
binSearch ('B' : ss) m n = binSearch ss (m + x) n
  where
    x = div (n - m + 1) 2
binSearch ('L' : ss) m n = binSearch ss m (n - x)
  where
    x = div (n - m + 1) 2
binSearch ('R' : ss) m n = binSearch ss (m + x) n
  where
    x = div (n - m + 1) 2
