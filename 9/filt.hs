threes :: (Integral a) => [a] -> [a]
threes = filter $ (== 0) . flip rem 3

threesLength :: (Integral a) => [a] -> Int
threesLength = length . threes

myFilter :: [Char] -> [[Char]]
myFilter = filter outArticles . words
  where outArticles w
          | w == "a" = False
          | w == "an" = False
          | w == "the" = False
          | otherwise = True

main :: IO ()
main =
  let one = threes [1..30]
      two = threesLength [1..30]
      three = myFilter "the brown dog was a goof"
  in do
    print one
    print two
    print three