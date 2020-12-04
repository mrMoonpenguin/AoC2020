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
checkPass2 [] = True
checkPass2 (s : ss)
  | h == "byr" = checkByr t && checkPass2 ss
  | h == "iyr" = checkIyr t && checkPass2 ss
  | h == "eyr" = checkEyr t && checkPass2 ss
  | h == "hgt" = checkHgt t && checkPass2 ss
  | h == "hcl" = checkHcl t && checkPass2 ss
  | h == "ecl" = checkEcl t && checkPass2 ss
  | h == "pid" = checkPid t && checkPass2 ss
  | h == "cid" = checkPass2 ss
  where
    h = take 3 s
    t = drop 4 s

checkByr :: String -> Bool
checkByr byr = byr' >= 1920 && byr' <= 2002
  where
    byr' = readInt byr

checkIyr :: String -> Bool
checkIyr iyr = iyr' >= 2010 && iyr' <= 2020
  where
    iyr' = readInt iyr

checkEyr :: String -> Bool
checkEyr eyr = eyr' >= 2020 && eyr' <= 2030
  where
    eyr' = readInt eyr

checkHgt :: String -> Bool
checkHgt hgt
  | unit == "cm" = hgt' >= 150 && hgt' <= 193
  | unit == "in" = hgt' >= 59 && hgt' <= 76
  | otherwise = False
  where
    unit = drop (len - 2) hgt
    hgt' = readInt $ take (len - 2) hgt
    len = length hgt

checkHcl :: String -> Bool
checkHcl s = length s == 7 && matchRegex (mkRegex "#([0-9a-f]{6})") s /= Nothing

checkEcl :: String -> Bool
checkEcl s = s == "amb" || s == "blu" || s == "brn" || s == "gry" || s == "grn" || s == "hzl" || s == "oth"

checkPid :: String -> Bool
checkPid s = length s == 9 && matchRegex (mkRegex "[0-9]{9}") s /= Nothing