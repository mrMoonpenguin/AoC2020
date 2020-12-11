main :: IO ()
main = do
  contents <- readFile "inputDay11.txt"
  let seats = lines contents
  print . countOccupiedSeats . repeatRules $ seats

-- returns neighbours of seat + seat itself
getNeighbours :: Int -> Int -> [String] -> [Char]
getNeighbours x y seats = [(seats !! y') !! x' | i <- [-1 .. 1], j <- [-1 .. 1], let (x', y') = (x + i, y + j), x' >= 0, y' >= 0, x' < length (head seats), y' < length seats]

replace' :: Int -> a -> [a] -> [a]
replace' n el lst = take n lst ++ [el] ++ drop (n + 1) lst

repeatRules :: [String] -> [String]
repeatRules seats
  | not chd1 && not chd2 = seats2
  | otherwise = repeatRules seats2
  where
    (chd1, seats1) = rule1 seats
    (chd2, seats2) = rule2 seats1

countOccupiedSeats :: [String] -> Int
countOccupiedSeats seats = sum $ map (length . filter (== '#')) seats

rule1 :: [String] -> (Bool, [String])
rule1 seats = rule1' 0 0 False seats seats

rule1' :: Int -> Int -> Bool -> [String] -> [String] -> (Bool, [String])
rule1' x y changed acc seats
  | y >= length seats = (changed, acc)
  | x >= length (seats !! y) = rule1' 0 (y + 1) changed acc seats
  | (seats !! y) !! x /= 'L' = rule1' (x + 1) y changed acc seats
  | occupied == 0 = rule1' (x + 1) y True (replace' y (replace' x '#' (acc !! y)) acc) seats
  | otherwise = rule1' (x + 1) y changed acc seats
  where
    occupied = length . filter (== '#') $ getNeighbours x y seats

rule2 :: [String] -> (Bool, [String])
rule2 seats = rule2' 0 0 False seats seats

rule2' :: Int -> Int -> Bool -> [String] -> [String] -> (Bool, [String])
rule2' x y changed acc seats
  | y >= length seats = (changed, acc)
  | x >= length (head seats) = rule2' 0 (y + 1) changed acc seats
  | (seats !! y) !! x /= '#' = rule2' (x + 1) y changed acc seats
  | occupied > 4 = rule2' (x + 1) y True (replace' y (replace' x 'L' (acc !! y)) acc) seats
  | otherwise = rule2' (x + 1) y changed acc seats
  where
    occupied = length . filter (== '#') $ getNeighbours x y seats