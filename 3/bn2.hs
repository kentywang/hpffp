-- bn2.hs
module BuildingFunctions2 where

a :: [Char] -> [Char]
a s = s ++ "!"

b :: [Char] -> [Char]
b s = drop 4 (take 5 s)
b2 s = 4 `drop` (5 `take` s)

c s = drop 9 s