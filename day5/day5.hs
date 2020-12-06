main :: IO ()
main =
  do
    contents <- readFile "inputDay5.txt"
    print . findSeat . parseInput . lines $ contents

-- directly converting inputs into binary immediately gives the seatID, no need to calculate rows and columns separately
parseInput :: [String] -> [Int]
parseInput lst = [binToDec s' | s <- lst, let s' = map (\x -> if x == 'B' || x == 'R' then 1 else 0) (reverse s)]

binToDec :: [Int] -> Int
binToDec = foldr (\x y -> x + 2 * y) 0

findSeat :: [Int] -> Int
findSeat lst = (+ 1) $ head $ filter (\x -> notElem (x + 1) lst && elem (x + 2) lst) lst
