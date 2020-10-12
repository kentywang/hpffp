{-# LANGUAGE TypeApplications #-}

module Two where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "parseDigit"

base10Integer :: Parser Integer
-- Easy way:
-- base10Integer = read <$> some parseDigit <?> "integer"
base10Integer = fromIntegerListToInteger . (fmap charToInteger) <$> some parseDigit <?> "integer"

charToInteger n =
  case n of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _ -> error "bad"

fromIntegerListToInteger :: Integral a => [a] -> a
fromIntegerListToInteger (x:xs) = go x xs
  where go acc [] = acc
        go acc (y:ys) = go (acc * 10 + y) ys
-- Better: foldl1 (\acc a -> acc * 10 + a)

base10Integer' :: Parser Integer
base10Integer' = base10Integer <|> negativeBase10Integer
  where negativeBase10Integer = char '-' *> fmap negate base10Integer
 
main :: IO ()
main = do
  print $ parseString parseDigit mempty "123" -- Success '1'
  print $ parseString parseDigit mempty "abc" -- Failure
  print $ parseString base10Integer mempty "123abc" -- Success 123
  print $ parseString base10Integer mempty "abc" -- Failure
  print $ parseString base10Integer' mempty "-123abc" -- Success (-123)