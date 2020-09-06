-- ce.hs
import Data.Char

-- 2
capsOnly :: String -> String
capsOnly = filter isUpper

-- 3
capFirst :: String -> String
capFirst (x:xs) = toUpper x : xs

-- 4
newCapFirst []  = []
newCapFirst (x:xs) = toUpper x : newCapFirst xs

-- 5, 6
capHead :: [Char] -> Char
capHead = toUpper . head

main :: IO ()
main = do
  print $ capsOnly "HbEfLrLxO"
  print $ capFirst "julie"
  print $ newCapFirst "woot"