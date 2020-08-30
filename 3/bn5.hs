-- bn5.hs
module BuildingFunctions5 where

rvrs :: [Char] -> [Char]
rvrs x = concat [drop 9 x, take 4 (drop 5 x), take 5 x]  