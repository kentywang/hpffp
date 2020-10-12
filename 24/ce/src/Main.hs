{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Text.Trifecta
import Data.Char (isAlpha)

-- Relevant to precedence/ordering, 
-- cannot sort numbers like strings.
data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving Show

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- parseNonleadingZeroNumber
  char '.'
  minor <- parseNonleadingZeroNumber
  char '.'
  patch <- parseNonleadingZeroNumber
  release <- parseRelease <|> pure [] -- option [] parseRelease works too
  metadata <- parseMetadata <|> pure []
  return $ SemVer major minor patch release metadata -- metadata
  where parseRelease :: Parser [NumberOrString]
        parseRelease = do
          char '-'
          -- How do I parse something interleaved with dots, but no dot at end?
          -- A: Book didn't mention this, but sepBy1 does the trick. 
          sepBy1 parseIdentifier $ char '.'

        -- Interpreting SemVer spec that metadata can allow leading zeroes to
        -- mean that metadata is always string, not numeric.
        parseMetadata :: Parser [NumberOrString]
        parseMetadata = do
          char '+'
          sepBy1 parseAlphaNumDashes $ char '.'

        parseIdentifier :: Parser NumberOrString
        parseIdentifier =
              (try (NOSI <$> parseNonleadingZeroNumber))
          <|> parseAlphaNumDashes

        -- Assistance from 
        -- https://github.com/yamad/haskellbook-hpfp/blob/master/Ch24_SemVer.hs
        parseNonleadingZeroNumber :: Parser Integer
        parseNonleadingZeroNumber =
          read <$> 
              ((string "0" <* notFollowedBy digit) <|> -- Just a '0'
               (oneOf "123456789" >>= \x -> -- Any number that doesn't start with '0'
                 many digit >>= \xs ->
                   pure $ x : xs))

        parseAlphaNumDashes :: Parser NumberOrString
        parseAlphaNumDashes = NOSS <$> (some $ alphaNum <|> char '-')

instance Eq SemVer where
  (==) (SemVer a b c d _)
       (SemVer v w x y _) =
         a == b &&
         b == w &&
         c == x &&
         (null d == null y)
{-- 
I like this person's soln:
  mconcat
    [ compare major major'
    , compare minor minor'
    , compare patch patch'
    , compare rel rel'
    ]
--}
instance Ord SemVer where
  compare 
    (SemVer a b c d _)
    (SemVer v w x y _)
    | a > v = GT
    | a < v = LT
    -- Below this means major versions equal.
    | b > w = GT
    | b < w = LT
    -- Below this means minor versions equal.
    | c > x = GT
    | c < x = LT   
    -- Below this means patch versions equal.
    -- Here (unlike Github person I saw) I assume
    -- release versions are comparable only for existence
    -- but not for content.
    | null d && not (null y) = GT
    | not (null d) && null y = LT
    | otherwise = EQ

main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1" -- Success (SemVer 2 1 1 [] [])

  print $ parseString parseSemVer mempty "1.0.0-alpha"
  print $ parseString parseSemVer mempty "1.0.0-alpha.1"
  print $ parseString parseSemVer mempty "1.0.0-0.3.7"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  -- Success (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
  print $ parseString parseSemVer mempty "1.0.0-x-y-z.corgi"
  print $ parseString parseSemVer mempty "1.0.0-007" -- Doesn't pass
  print $ parseString parseSemVer mempty "1.01.0" -- Should fail


  print $ parseString parseSemVer mempty "1.0.0-alpha+001"
  print $ parseString parseSemVer mempty "1.0.0+20130313144700"
  print $ parseString parseSemVer mempty "1.0.0-beta+exp.sha.5114f85"
  print $ parseString parseSemVer mempty "1.0.0+21AF26D3-117B344092BD"

  print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] -- True

  print $ SemVer 1 2 3 [] [] == SemVer 1 2 3 [] [] -- True
  print $ compare
    (SemVer 1 2 3 [] [])
    (SemVer 1 2 3 [] []) -- EQ
  print $ compare
    (SemVer 1 1 1 [] [])
    (SemVer 1 1 2 [] []) -- LT
  print $ compare
    (SemVer 1 2 3 [] [])
    (SemVer 1 2 3 [NOSS "woof"] []) -- GT
  print $ compare
    (SemVer 1 2 3 [NOSI 123] [])
    (SemVer 1 2 3 [NOSS "woof"] []) -- EQ
  print $ compare
    (SemVer 1 2 3 [] [NOSI 123])
    (SemVer 1 2 3 [] [NOSS "woof"]) -- EQ