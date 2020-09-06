module PoemLines where

-- Originally just for problem 1, now abstracted
-- to be useful for 2 also. And apparently that
-- was accomplished what problem 3 was looking for.
groupStrings :: Char -> [Char] -> [[Char]]
groupStrings by s
  | next s == [] = [curr s]
  | otherwise = [curr s] ++ (groupStrings by . tail . next) s
  where next = dropWhile (/= by)
        curr = takeWhile (/= by)

myWords :: [] Char -> [] ([] Char)
myWords = groupStrings ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String]
myLines = groupStrings '\n'

-- What we want 'myLines sentences'
-- to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = 
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)