import Data.List.Utils (replace)
import Data.Maybe
import Data.Set (Set, empty, insert, member)

data Operation = Op {fn :: String, arg :: Int} deriving (Show)

main :: IO ()
main = do
  contents <- readFile "inputDay8.txt"
  print . run 0 0 empty . map ((\x -> Op (x !! 0) (readInt $ x !! 1)) . words) . lines . replace "+" "" $ contents
  print . replaceBadLine 0 . map ((\x -> Op (x !! 0) (readInt $ x !! 1)) . words) . lines . replace "+" "" $ contents

readInt :: String -> Int
readInt = read

-- run for part 1
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

run' :: Int -> Int -> Set Int -> [Operation] -> Maybe Int
run' acc _ _ [] = Just acc
run' acc pos set ops
  | pos >= length ops = Just acc
  | member pos set = Nothing
  | otherwise = case instr of
    "acc" -> run' (acc + arg op) (pos + 1) (insert pos set) ops
    "jmp" -> run' acc (pos + arg op) (insert pos set) ops
    "nop" -> run' acc (pos + 1) (insert pos set) ops
  where
    op = ops !! pos
    instr = fn op

replaceBadLine :: Int -> [Operation] -> Int
replaceBadLine pos ops = case instr of
  "acc" -> replaceBadLine (pos + 1) ops
  "jmp" ->
    fromMaybe (replaceBadLine (pos + 1) ops) res1
    where
      res1 = run' 0 0 empty (take pos ops ++ [Op "nop" (arg op)] ++ drop (pos + 1) ops)
  "nop" ->
    fromMaybe (replaceBadLine (pos + 1) ops) res2
    where
      res2 = run' 0 0 empty (take pos ops ++ [Op "jmp" (arg op)] ++ drop (pos + 1) ops)
  where
    op = ops !! pos
    instr = fn op