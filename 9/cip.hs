module Cipher where

import Data.Char ( ord, chr )

main :: IO ()
main = do
  print $ caesar y x
  print $ unCaesar y $ caesar y x
  where x = "thequickbrownfoxjumpsoverthelazydog"
        y = 322

caesar :: Int -> String -> String
caesar i = map $ \n -> chr $ ord 'a' + mod (ord n + i - ord 'a') 26

unCaesar :: Int -> String -> String
unCaesar x = caesar (-x)
