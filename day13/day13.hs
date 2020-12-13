import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- readFile "inputDay13.txt"
  let input = lines contents
  let start = readInt . head $ input
  let busses = map readInt $ filter (/= "x") (splitOn "," (input !! 1))
  print . (\(x, y) -> (x - start) * y) $ findBus start busses

readInt :: String -> Int
readInt = read

findBus :: Int -> [Int] -> (Int, Int)
findBus time busses
  | length busses' > 0 = (time, head busses')
  | otherwise = findBus (time + 1) busses
  where
    busses' = filter (\x -> time `mod` x == 0) busses