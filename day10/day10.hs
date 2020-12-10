import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

-- -- part 1
-- main :: IO ()
-- main = do
--   contents <- readFile "inputDay10.txt"
--   let lst = sort . map readInt . lines $ contents
--   let lst' = zip (lst ++ [last lst + 3]) ((0 : lst) ++ [last lst + 3])
--   print $ joltage lst'

-- part 2
main :: IO ()
main = do
  contents <- readFile "inputDay10.txt"
  let lst = sort . map readInt . lines $ contents
  let lst' = lst ++ [last lst + 3]
  print $ combinations lst' (Map.singleton 0 1)

readInt :: String -> Int
readInt = read

-- hardcoded the gap, oh well
joltage :: [(Int, Int)] -> Int
joltage lst = length (filter (\(x, y) -> x - y == 3) lst) * length (filter (\(x, y) -> x - y == 1) lst)

(!!!) :: Map.Map Int Int -> Int -> Int
mp !!! n
  | isNothing val = 0
  | otherwise = fromJust val
  where
    val = Map.lookup n mp

combinations :: [Int] -> Map.Map Int Int -> Int
combinations [] mp = snd (Map.findMax mp)
combinations (x : xs) mp = combinations xs (Map.insert x val mp)
  where
    val = mp !!! (x - 1) + mp !!! (x - 2) + mp !!! (x - 3)