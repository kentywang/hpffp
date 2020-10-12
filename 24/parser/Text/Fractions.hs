{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative 
import Data.Ratio ((%)) 
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal 
  char '/'
  denominator <- decimal 
  case denominator of
    0 -> fail "Denominator cannot be zero" 
    _ -> return (numerator % denominator)

yourFuncHere :: Parser Integer
yourFuncHere =
  -- integer >>= \n ->
  --   eof >>= const (pure n)
  do
    n <- integer
    eof
    return n

main :: IO ()
main = do
  -- let parseFraction' =
  --       parseString virtuousFraction mempty
  -- print $ parseFraction' shouldWork
  -- print $ parseFraction' shouldAlsoWork
  -- print $ parseFraction' alsoBad
  -- print $ parseFraction' badFraction

  print $ parseString (yourFuncHere) mempty "123" -- Success "123"
  print $ parseString (yourFuncHere) mempty "123abc" -- Failure