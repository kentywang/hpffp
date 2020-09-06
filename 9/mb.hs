import Data.Bool

itIsMystery :: [Char] -> [Bool]
itIsMystery = map $ (flip elem) "aeiou"

inv3rt :: (Eq a, Num a) => [a] -> [a]
inv3rt = map (\x -> bool x (-x) (x == 3))

main :: IO ()
main =
  print $ inv3rt [1..10]