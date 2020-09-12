data ThereYet =
  There Float Int Bool deriving (Eq, Show)
-- who needs a "builder pattern"?

nope :: Float -> Int -> Bool -> ThereYet
nope = There
notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5
notQuite :: Bool -> ThereYet
notQuite = notYet 10
yusssss :: ThereYet
yusssss = notQuite False

