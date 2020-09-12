import Data.Char ( toLower )

-- example GHCi session
-- above the functions

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . unwrapMaybe' . map notThe . words

unwrapMaybe' :: [Maybe String] -> [String]
unwrapMaybe' [] = []
unwrapMaybe' (Just str:xs) = str : unwrapMaybe' xs
unwrapMaybe' (Nothing:xs) = "a" : unwrapMaybe' xs


-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow" -- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel =
  snd .
  foldr countTheBeforeVowel' (Nothing, 0) .
  words

countTheBeforeVowel' :: String
                     -> (Maybe String, Integer)
                     -> (Maybe String, Integer)
countTheBeforeVowel' "the" (Just t, a)
  | startsWithVowel t == True = (Nothing, a + 1)  -- [1]
  | otherwise                 = (Nothing, a)
countTheBeforeVowel' s (_, a)
  | startsWithVowel s == True = (Just s, a)
  | otherwise                 = (Nothing, a)  -- [2]

{-
  [1]: It needn't be creating a tuple with Nothing here,
       since "the" is obviously not going to be a vowel
       for the next word. But using Nothing is a signal,
       I guess.
  [2]: Likewise.
-}

startsWithVowel :: [Char] -> Bool
startsWithVowel "" = False
startsWithVowel (x:_) = isVowel x

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak" -- 4
countVowels :: String -> Integer
countVowels = fromIntegral . length . getVowels

getVowels :: String -> String
getVowels = filter isVowel

isVowel :: Char -> Bool
isVowel x =
  case (toLower x) of
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    _   -> False