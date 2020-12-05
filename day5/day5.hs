main :: IO ()
main =
  do
    contents <- readFile "inputDay5.txt"
    print . findSeat . parseInput . lines $ contents

parseInput :: [String] -> [Int]
parseInput lst = [r * 8 + c | s <- lst, let s' = map (\x -> if x == 'B' || x == 'R' then 1 else 0) (reverse s), let r = binToDec $ drop 3 s', let c = binToDec $ take 3 s']

binToDec :: [Int] -> Int
binToDec = foldr (\x y -> x + 2 * y) 0

findSeat :: [Int] -> Int
findSeat lst = (+ 1) $ head $ filter (\x -> notElem (x + 1) lst && elem (x + 2) lst) lst
