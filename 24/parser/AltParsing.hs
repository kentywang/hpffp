{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative 
import Text.Trifecta
import Text.RawString.QQ
import Data.Ratio ((%)) 

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)

eitherOr :: String
eitherOr = [r|
123
abc
456
12haha
def|]

main = do
  let p f i =
        parseString f mempty i
  -- print $ p (some letter) a
  -- print $ p integer b
  -- print $ p parseNos a
  -- print $ p parseNos b
  -- print $ p (many parseNos) c
  -- print $ p (some parseNos) c
  print $ p (some parseNos) eitherOr

type DecimalOrFraction = Either Double Rational

parseDof :: Parser DecimalOrFraction
parseDof = do
  skipMany (oneOf "\n")
  -- Gee, I hope the book explains why I needed try
  -- here but not in parseNos. I'm guessing it has
  -- something to do with the fact that a sequence of
  -- digits can be either the start of a decimal number
  -- or a fractional, so when it reaches the '/' char,
  -- it might be in the middle of the decimal parsing,
  -- whereas in parseNos, you can tell from the first
  -- character which parser to use.
  v <- try (Left <$> double) <|> (Right <$> parseFraction)
  skipMany (oneOf "\n")
  return v

-- | This is virtuousFraction from earlier.
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal 
  char '/'
  denominator <- decimal 
  case denominator of
    0 -> fail "Denominator cannot be zero" 
    _ -> return (numerator % denominator)

-- Apparently Text.Parser.Token.double needs a decimal point,
-- meaning "12345" would fail.
myText :: String
myText = [r|
6.789
9/10
11.12/13
abc
14/15.16
17/0
|]

tryTry :: IO ()
tryTry = print $
  parseString (some parseDof) mempty myText

p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return (read i)
