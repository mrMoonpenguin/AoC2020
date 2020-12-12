type Action = (Char, Double)

type Position = (Double, Double)

type Angle = Double

main :: IO ()
main = do
  contents <- readFile "inputDay12.txt"
  print . move (0, 0) 0 . map mkAction . lines $ contents

readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble = read

act :: Action -> Char
act (a, _) = a

val :: Action -> Double
val (_, v) = v

mkAction :: String -> (Char, Double)
mkAction s = (head s, readDouble (tail s))

move :: Position -> Angle -> [Action] -> Position
move pos _ [] = pos
move pos@(x, y) alpha (act : acts) = case fst act of
  'N' -> move (x, y - snd act) alpha acts
  'S' -> move (x, y + snd act) alpha acts
  'E' -> move (x + snd act, y) alpha acts
  'W' -> move (x - snd act, y) alpha acts
  'L' -> move pos (alpha - snd act) acts
  'R' -> move pos (alpha + snd act) acts
  'F' -> move (x + snd act * sin (alpha * 2 * pi / 360), y + snd act * cos (alpha * 2 * pi / 360)) alpha acts