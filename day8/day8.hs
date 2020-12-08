import Data.List.Utils (replace)
import Data.Set (Set, empty, insert, member)

data Operation = Op {fn :: String, arg :: Int} deriving (Show)

main :: IO ()
main = do
  contents <- readFile "inputDay8.txt"
  print . run 0 0 empty . findBadLine 0 empty . map ((\x -> Op (x !! 0) (readInt $ x !! 1)) . words) . lines . replace "+" "" $ contents

readInt :: String -> Int
readInt = read

run :: Int -> Int -> Set Int -> [Operation] -> Int
run acc _ _ [] = acc
run acc pos set ops
  | pos >= length ops = acc
  | member pos set = acc
  | otherwise = case instr of
    "acc" -> run (acc + arg op) (pos + 1) (insert pos set) ops
    "jmp" -> run acc (pos + arg op) (insert pos set) ops
    "nop" -> run acc (pos + 1) (insert pos set) ops
  where
    op = ops !! pos
    instr = fn op

findBadLine :: Int -> Set Int -> [Operation] -> [Operation]
findBadLine pos set ops = case instr of
  "acc" -> findBadLine (pos + 1) (insert pos set) ops
  "jmp" ->
    if member (pos + arg op) set
      then take pos ops ++ [Op "nop" (arg op)] ++ drop (pos + 1) ops
      else findBadLine (pos + arg op) (insert pos set) ops
  "nop" ->
    if member (pos + 1) set
      then take pos ops ++ [Op "jmp" (arg op)] ++ drop (pos + 1) ops
      else findBadLine (pos + 1) (insert pos set) ops
  where
    op = ops !! pos
    instr = fn op