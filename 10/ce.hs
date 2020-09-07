-- 1
stops = "pbtdkg"
vowels = "aeiou"
-- a
stopVowelStop = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
-- b
startsWithP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']
-- c
nVn = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]
  where nouns = ["corgi", "pug"]
        verbs = ["bite", "kiss"]

-- 3
seekritFunc :: (Fractional a) => [Char] -> a
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))

main :: IO ()
main = do
  -- print stopVowelStop
  -- print startsWithP
  print nVn
  print $ seekritFunc "four score and seven years ago"