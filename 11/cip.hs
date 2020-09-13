import Data.Char ( ord, chr )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter message: "
  getLine >>= printMessage . vigenere "ALLY"
  where printMessage =
          putStrLn . ("The Vigenere code is: " ++)

vigenere :: String -> String -> String
vigenere "" _ = error "No keyword provided"
vigenere _ "" = ""
vigenere ks (' ':ms) = ' ' : vigenere ks ms
vigenere (k:ks) (m:ms) =
  (:) (caesarChar (ord k - ord 'A') m)
      (vigenere (ks ++ [k]) ms)

-- Old approach -- doesn't work with spaces
-- vigenere key msg = 
--   zipWith f (repeatString key) msg
--   where f _ ' ' = ' '
--         f a b = caesarChar (ord a - ord 'A') b

repeatString :: String -> String
repeatString "" = ""
repeatString (x:xs) = x : (repeatString $ xs ++ [x])  

caesarChar shift c =
  let startNum = ord 'A'
      delta = ord c - startNum in
    chr $ startNum + mod (delta + shift) 26

-- From before, modified to use caesarChar
caesar :: Int -> String -> String
caesar = map . caesarChar
