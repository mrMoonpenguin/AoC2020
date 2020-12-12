type Action = (Char, Double)

type Position = (Double, Double)

type Angle = Double

main :: IO ()
main = do
  contents <- readFile "inputDay12.txt"
  print . move (0, 0) (10, 1) . map mkAction . lines $ contents

readDouble :: String -> Double
readDouble = read

mkAction :: String -> (Char, Double)
mkAction s = (head s, readDouble (tail s))

move :: Position -> Position -> [Action] -> Position
move posS _ [] = posS
move posS@(x1, y1) posW@(x2, y2) (act : acts) = case fst act of
  'N' -> move posS (x2, y2 + snd act) acts
  'S' -> move posS (x2, y2 - snd act) acts
  'E' -> move posS (x2 + snd act, y2) acts
  'W' -> move posS (x2 - snd act, y2) acts
  'L' ->
    move
      posS
      (x2 * cos (2 * pi * snd act / 360) - y2 * sin (2 * pi * snd act / 360), y2 * cos (2 * pi * snd act / 360) + x2 * sin (2 * pi * snd act / 360))
      acts
  'R' ->
    move
      posS
      (x2 * cos (- 2 * pi * snd act / 360) - y2 * sin (- 2 * pi * snd act / 360), y2 * cos (- 2 * pi * snd act / 360) + x2 * sin (- 2 * pi * snd act / 360))
      acts
  'F' -> move (x1 + snd act * x2, y1 + snd act * y2) posW acts