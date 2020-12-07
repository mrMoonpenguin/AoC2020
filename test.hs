import Data.List

data Sample = MkSample Int deriving Show

reverse' = sortBy (\_ _ -> GT)
