-- {-# LANGUAGE FlexibleInstances #-}

import Data.List ( elemIndex, find )
import Data.Char ( toLower, ord, isAlpha )

-- "hello" -> [(4,2),(3,2),(5,3),(5,3),(0,3)]
-- Using list rather than record
-- because I dunno how to iterate through record's
-- fields.
type DaPhone = [Key]

data Sequence = 
  Value String | Caps
  deriving Show

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data Key =
  Key Digit Sequence
  deriving Show

phone :: DaPhone
phone = [ Key '1' $ Value "1"
        , Key '2' $ Value "abc2"  -- Could leave numbers
        , Key '3' $ Value "def3"  -- out, but would make
        , Key '4' $ Value "ghi4"  -- logic more complex.
        , Key '5' $ Value "jkl5"
        , Key '6' $ Value "mno6"
        , Key '7' $ Value "pqrs7"
        , Key '8' $ Value "tuv8"
        , Key '9' $ Value "wxyz9"
        -- , Key '*' Caps
        , Key '0' $ Value " 0"
        , Key '#' $ Value ".," ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

-- | God, this function is ugly.
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps [] _ = error "Untextable character!"
reverseTaps p c
  | isUpper c = go [('*', 1)] p (toLower c) -- hard-coded caps location
  | otherwise = go [] p c
  where go :: [(Digit, Presses)] -> DaPhone -> Char -> [(Digit, Presses)]
        go _ [] c = error "Untextable character!"
        go acc ((Key d (Value s)):ps) c  -- TODO: Need pattern for Caps
          | elemIndex c s == Nothing = go acc ps c
          | otherwise = helper acc d (elemIndex c s)
          where helper j l (Just x) = j ++ [(l, x + 1)]
                helper _ _ Nothing = error "Shouldn't be possible."

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ "" = []
cellPhonesDead p (x:xs) =
  reverseTaps p x ++ cellPhonesDead p xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) q -> p + q) 0

mostPopularLetter :: String -> Char
mostPopularLetter =
  mostPopularItem .
  letterFreq

mostPopularItem :: [(a, Int)] -> a
mostPopularItem = fst . foldr1 cmpCts
  where cmpCts :: (a, Int)
               -> (a, Int)
               -> (a, Int)
        cmpCts a@(_,i) b@(_,j)
          | i > j = a
          | otherwise = b

-- Something is buggy about this...
-- instance Ord (a, Int) where
--   (_,i) > (_,j) = i > j

{-|
  Abstracted out to be usable for both letters and words.
  It was so easy too!
-}
freqList :: Eq a => [a] -> [(a, Int)]
freqList [] = error "No list provided!"
freqList xs = go [] xs
  where go :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
        go counts [] = counts
        go counts (x:xs) = go (upCt counts x) xs

        upCt :: Eq a => [(a, Int)] -> a -> [(a, Int)]
        upCt [] c = (c,1) : []
        upCt (t@(a, b):xs) c
          | a == c = (a, b + 1) : xs
          | otherwise = t : upCt xs c

letterFreq :: [Char] -> [(Char, Int)]
letterFreq = freqList . map toLower . filter isAlpha

wordFreq :: [String] -> [(String, Int)]
wordFreq = freqList

mostFreqLetterTapCost :: DaPhone -> [Char] -> Presses
mostFreqLetterTapCost phone msg = 
  ct * fingerTaps (reverseTaps phone c)
    where ct = letterCount c msg
          c = mostPopularLetter msg

letterCount :: Char -> [Char] -> Int
letterCount char = maybeOpener . find predicate . letterFreq
  where maybeOpener :: Maybe (Char, Int) -> Int
        maybeOpener Nothing = 0
        maybeOpener (Just (_, b)) = b

        predicate :: (Char, Int) -> Bool
        predicate (a, _)
          | a == char = True
          | otherwise = False

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat 

coolestWord :: [String] -> String
coolestWord =
  mostPopularItem .
  wordFreq .
  getWords

getWords :: [String] -> [String]
getWords = concat . map f
  where f str = words $ filter g str
        g = (/= '.')

-- Actually, this exists.
isUpper :: Char -> Bool
isUpper c =
  ord c >= ord 'A' && ord c <= ord 'Z'

main :: IO ()
main = do
  putStr "Finger taps:\n\t"
  print $ map f convo
  putStr "Most frequent char:\n\t"
  print $ map mostPopularLetter convo
  putStr "Char count:\n\t"
  print $ map g convo
  putStr "Char tap cost:\n\t"
  print $ map h convo
  putStr "Coolest letter:\n\t"
  print $ coolestLtr convo
  putStr "Coolest word:\n\t"
  print $ coolestWord convo
    where f = fingerTaps . cellPhonesDead phone
          g str = letterCount (mostPopularLetter str) str
          h = mostFreqLetterTapCost phone