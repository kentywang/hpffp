import Data.Char ( isAlpha ) 

newtype Word' =
  Word' String deriving (Eq, Show)

type VowelCount = Integer
type ConsonantCount = Integer

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = mkWord' s $ vowCons s

mkWord' :: String -> (VowelCount, ConsonantCount) -> Maybe Word'
mkWord' s (a, b)
  | a > b     = Nothing
  | otherwise = Just $ Word' s

vowCons :: String -> (VowelCount, ConsonantCount)
vowCons = go (0, 0)
  where go vc "" = vc
        go (a, b) (x:xs)
          | elem x vowels == True = go (a + 1, b) xs
          | isAlpha x == True     = go (a, b + 1) xs
          | otherwise             = go (a, b) xs