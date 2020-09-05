module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "nope"

digits :: Int -> [Int]
digits n = go n []
  where go z result
          | z < 10 = [z] ++ result
          | otherwise =
            go (div z 10) $ [mod z 10] ++ result

wordNumber :: Int -> String
wordNumber = concat . (intersperse "-") . (map digitToWord) . digits
--           String   [String]            [String]            [Int]
