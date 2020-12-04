import Data.List.Split
import Text.Regex

main :: IO ()
main =
  do
    contents <- readFile "inputDay4.txt"
    print $ length $ filter checkPass2 $ filter checkPass $ map words $ splitOn "\n\n" contents

readInt :: String -> Int
readInt = read

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

checkPass2 :: [String] -> Bool
checkPass2 = foldr (\x y -> checkField (take 3 x) (drop 4 x) && y) True

checkField :: String -> String -> Bool
checkField "byr" byr = readInt byr >= 1920 && readInt byr <= 2002
checkField "iyr" iyr = readInt iyr >= 2010 && readInt iyr <= 2020
checkField "eyr" eyr = readInt eyr >= 2020 && readInt eyr <= 2030
checkField "hgt" hgt
  | unit == "cm" = hgt' >= 150 && hgt' <= 193
  | unit == "in" = hgt' >= 59 && hgt' <= 76
  | otherwise = False
  where
    unit = drop (len - 2) hgt
    hgt' = readInt $ take (len - 2) hgt
    len = length hgt
checkField "hcl" hcl = length hcl == 7 && matchRegex (mkRegex "#([0-9a-f]{6})") hcl /= Nothing
checkField "ecl" ecl = ecl == "amb" || ecl == "blu" || ecl == "brn" || ecl == "gry" || ecl == "grn" || ecl == "hzl" || ecl == "oth"
checkField "pid" pid = length pid == 9 && matchRegex (mkRegex "[0-9]{9}") pid /= Nothing
checkField "cid" _ = True