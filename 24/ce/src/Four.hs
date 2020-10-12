{-# LANGUAGE TypeApplications #-}

module Four where

import Control.Applicative
import Text.Trifecta
-- Just to test if it's exchangeable with my charToInteger.
import Data.Char (digitToInt)
import Two (fromIntegerListToInteger)

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber 
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  try variant1 <|> variant2 <|> variant3
-- Cleanest solution, but not optimal.

variant1 = do
  digits <- count 10 digit
  
  let xs = [ take 3 digits
           , take 3 (drop 3 digits)
           , take 4 (drop 6 digits) ] 

  let [npa', ex', ln'] = fmap fromIntegerListToInteger ((fmap . fmap) digitToInt xs)

  pure $ PhoneNumber 
    (npa' :: NumberingPlanArea) 
    (ex' :: Exchange)
    (ln' :: LineNumber)

variant2 = do
  -- So if `try` fails, instead of exiting this, because it
  -- behaves as if we didn't consume input, `skipOptional`
  -- doesn't fail.
  skipOptional $ try (char '1' *> char '-')
  npa <- count 3 digit
  char '-'
  ex <- count 3 digit
  char '-'
  ln <- count 4 digit

  let [npa', ex', ln'] = 
        fmap fromIntegerListToInteger ((fmap . fmap) digitToInt [npa, ex, ln])

  pure $ PhoneNumber 
    (npa' :: NumberingPlanArea) 
    (ex' :: Exchange)
    (ln' :: LineNumber)

variant3 = do
  npa <- parens $ count 3 digit
  -- `parens` already removes whitespace.
  -- If we want to consider multiple whitespaces invalid, we need
  -- to not use `parens`.
  ex <- count 3 digit
  char '-'
  ln <- count 4 digit

  let [npa', ex', ln'] = 
        fmap fromIntegerListToInteger ((fmap . fmap) digitToInt [npa, ex, ln])

  pure $ PhoneNumber 
    (npa' :: NumberingPlanArea) 
    (ex' :: Exchange)
    (ln' :: LineNumber)

main = do
  print $ parseString parsePhone mempty "1234567890" -- Success (PhoneNumber 123 456 7890)
  print $ parseString parsePhone mempty "123-456-7890" -- Success (PhoneNumber 123 456 7890)
  print $ parseString parsePhone mempty "(123) 456-7890" -- Success (PhoneNumber 123 456 7890)
  print $ parseString parsePhone mempty "1-123-456-7890" -- Success (PhoneNumber 123 456 7890)