import System.IO
import Control.Monad
import Data.HashSet as HashSet

readInt :: String -> Int
readInt = read

findSum :: IO ()
findSum = do
            contents <- readFile "inputDay1.txt"
            print . findSum' . Prelude.map readInt . words $ contents

findSum' :: [Int] -> Int
findSum' lst = head [x * y | x <- lst, y <- lst, x + y == 2020]

findSum2 :: IO ()
findSum2 = do
            contents <- readFile "inputDay1.txt"
            print . findSum2' . Prelude.map readInt . words $ contents

findSum2' :: [Int] -> Int
findSum2' lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z == 2020]

effFindSum :: IO ()
effFindSum = do
                contents <- readFile "inputDay1.txt"
                let input = Prelude.map readInt . words $ contents
                print $ effFindSum' input 2020 (HashSet.fromList input) 

effFindSum' :: [Int] -> Int -> HashSet Int -> Maybe Int
effFindSum' [] _ _ =  Nothing
effFindSum' (x : xs) t hs 
    | HashSet.member (t - x) hs = Just (x * (t - x))
    | otherwise = effFindSum' xs t hs
