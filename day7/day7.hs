import Data.List
import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.Maybe
import Data.Tuple

type Bag = String

data Rule = MkRule {bag :: Bag, bagContent :: [(Int, Bag)]} deriving (Show)

type Rules = [Rule]

-- part 1
main :: IO ()
main = do
  contents <- readFile "inputDay7.txt"
  let rules = parseInput . lines $ contents
  print . length . filter (checkGold rules) $ rules

-- -- part 2
-- main :: IO ()
-- main = do
--   contents <- readFile "inputDay7.txt"
--   let rules = parseInput . lines $ contents
--   print . ((-1) +) $ countBags (fromJust $ findBag rules "shinygold") rules

readInt :: String -> Int
readInt = read

parseInput :: [String] -> Rules
parseInput [] = []
parseInput (s : ss)
  | s' !! 1 == "" = MkRule (head s') [] : parseInput ss
  | otherwise = MkRule (head s') [(x, y) | bag <- lst, let (x', y) = splitAt 1 bag, let x = readInt x'] : parseInput ss
  where
    s' = splitOn "contain" . replace " " "" . replace "bag" "" . replace "bags" "" . replace "." "" . replace "no other bags" "" $ s
    lst = splitOn "," (s' !! 1)

contains :: [(Int, Bag)] -> Bag -> Bool
contains [] _ = False
contains (x : xs) b
  | b == snd x = True
  | otherwise = contains xs b

findBag :: Rules -> Bag -> Maybe Rule
findBag [] _ = Nothing
findBag (r : rs) b
  | b == bag r = Just r
  | otherwise = findBag rs b

checkGold :: Rules -> Rule -> Bool
checkGold rules r
  | contains (bagContent r) "shinygold" = True
  | otherwise = foldr (\x y -> (checkGold rules . fromJust . findBag rules . snd $ x) || y) False (bagContent r)

countBags :: Rule -> Rules -> Int
countBags r rules = foldr (\x y -> fst x * countBags (fromJust $ findBag rules (snd x)) rules + y) 1 (bagContent r)