{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char 
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

oneS :: Parser String
oneS = string "1"

oneTwoS :: Parser String
oneTwoS = string "12"

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

testParse :: Show a => Parser a -> IO () 
testParse p =
  print $ parseString p mempty "123"

-- Parser a is Parser (String -> Maybe (a, String)) 
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
ichiNiSan :: Parser [Char]
ichiNiSan = 
  char '1' >>= \x ->
    char '2' >>= \y ->
      char '3' >>= \z ->
      return [x, y, z]
  -- do
  --   x <- char '1'
  --   y <- char '2'
  --   z <- char '3'
  --   pure [x, y, z]

-- char '1' >>= \x ->
--   char '2' >>= \y ->
--     return [x, y]

-- Simplifying by removing Maybe in Parser:
--   Parser $ \s ->
--     let (a, s') = (unparser (char '1')) s
--     in (unparser ((\x -> char '2' >>= \y -> return [x, y]) a)) s'

--   Parser $ \s ->
--     let (a, s') = (unparser (char '1')) s
--     in (unparser ((\x ->
--                     (Parser $ \t ->
--                       let (c, t') = (unparser (char '2')) t
--                       in (unparser ((\y -> return [x, y]) c)) t')) a)) s'
  
--   Parser $ \s ->
--     let (a, s') = (unparser (char '1')) s
--     in (unparser ((\x ->
--                     (Parser $ \t ->
--                       let (c, t') = (unparser (char '2')) t
--                       in (unparser ((\y -> Parser $ (,) [x, y]) c)) t')) a)) s'
  
--   So given s is "123"...

--   let (a, s') = (unparser (char '1')) "123"
--   in (unparser ((\x ->
--     (Parser $ \t ->
--       let (c, t') = (unparser (char '2')) t
--       in (unparser ((\y -> Parser $ (,) [x, y]) c)) t')) a)) s'

--   Applying (char '1')...

--   let (a, s') = ('1', "23")
--   in (unparser ((\x ->
--     (Parser $ \t ->
--       let (c, t') = (unparser (char '2')) t
--       in (unparser ((\y -> Parser $ (,) [x, y]) c)) t')) a)) s'

--   Parsed value '1' is queued up to be passed to 2nd argument of (>>=), which will
--   return a function that will be passed parsed state "23".

--   (unparser  ((\x ->
--     (Parser $ \t ->
--       let (c, t') = (unparser (char '2')) t
--       in (unparser ((\y -> Parser $ (,) [x, y]) c)) t')) '1')) "23"

--   Parsed value '1' is passed to 2nd argument of (>>=), which binds to 1st element of value list.

--   (unparser
--     (Parser $ \t ->
--       let (c, t') = (unparser (char '2')) t
--       in (unparser ((\y -> Parser $ (,) ['1', y]) c)) t')) "23"

--   (\t ->
--     let (c, t') = (unparser (char '2')) t
--     in (unparser ((\y -> Parser $ (,) ['1', y]) c)) t') "23"

--   Parsed result "23" is passed to 2nd argument of (>>=)...

--   let (c, t') = (unparser (char '2')) "23"
--   in (unparser ((\y -> Parser $ (,) ['1', y]) c)) t'

--   Applying (char '2')...

--   let (c, t') = ('2', "3")
--   in (unparser ((\y -> Parser $ (,) ['1', y]) c)) t'

--   Parsed value '2' is queued up to be passed to 2nd argument of (>>=), which will
--   return a function that will be passed parsed state "3".

--   (unparser ((\y -> Parser $ (,) ['1', y]) '2')) "3"

--   Parsed value '2' is passed to 2nd argument of (>>=), which binds to 2nd element of value list.

--   (unparser (Parser $ (,) ['1', '2'])) "3"

--   Parsed result "3" is passed to 2nd argument of (>>=)...

--   ((,) ['1', '2']) "3"

--   (['1', '2'], "3")

pNL s =
  putStrLn ('\n' : s)

main = do
  -- pNL "stop:" 
  -- testParse @Char stop 
  -- pNL "one:" 
  -- testParse @Char one
  -- pNL "one':" 
  -- testParse @Char one' 
  -- pNL "oneTwo:" 
  -- testParse @Char oneTwo 
  -- pNL "oneTwo':" 
  -- testParse @Char oneTwo'

  -- pNL "oneEof:"
  -- testParse @() $ one >> eof
  -- pNL "oneTwoEof:"
  -- testParse @() $ oneTwo >> eof
  -- pNL "oneTwoThreeEof:"
  -- testParse @() $ oneTwo >> char '3' >> eof

  -- pNL "1, 12, 123:"
  -- testParse @String oneS
  -- testParse @String oneTwoS
  -- testParse @String oneTwoThreeS

  -- testParse @() $ oneTwoThreeS >> stop
  
  testParse @String ichiNiSan
