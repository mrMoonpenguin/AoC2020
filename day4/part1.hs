import Data.List.Split

main :: IO ()
main =
  do
    contents <- readFile "inputDay4.txt"
    print $ length $ filter checkPass $ map words $ splitOn "\n\n" contents

checkPass :: [String] -> Bool
checkPass s = checkPass' s'
  where
    s' = map (take 3) s

checkPass' :: [String] -> Bool
checkPass' s =
  elem "byr" s
    && elem "iyr" s
    && elem "eyr" s
    && elem "hgt" s
    && elem "hcl" s
    && elem "ecl" s
    && elem "pid" s