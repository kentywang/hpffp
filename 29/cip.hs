import Data.Bool (bool)
import Data.Char (chr, ord, toUpper)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hPutStr, hSetBuffering, hWaitForInput, stderr, stdin, stdout)
import System.Exit

main :: IO ()
main = do
  [key, mode] <- getArgs -- IO [String]
  let upperCaseKey = toUpper <$> key
  case mode of
    "-e" -> encrypt upperCaseKey
    "-d" -> decrypt upperCaseKey
    _ -> error "bad!"

encrypt :: String -> IO ()
encrypt key = do
  hSetBuffering stdout NoBuffering
  putStr "Enter message: "
  b <- hWaitForInput stdin 5000
  if not b
    then exitWithError
    else
      getLine >>= printMessage . vigenere key . fmap toUpper
      where
        printMessage =
          putStrLn . ("The Vigenere code is: " ++)

decrypt :: String -> IO ()
decrypt key = do
  hSetBuffering stdout NoBuffering
  putStr "Enter code: "
  getLine >>= printMessage . deVigenere key . fmap toUpper
  where
    printMessage =
      putStrLn . ("The message is: " ++)

exitWithError :: IO ()
exitWithError = hPutStr stderr "\nNot fast enough!" >> exitWith (ExitFailure 1)

vigenere :: String -> String -> String
vigenere "" _ = error "No keyword provided"
vigenere _ "" = ""
vigenere ks (' ' : ms) = ' ' : vigenere ks ms
vigenere (k : ks) (m : ms) =
  (:)
    (caesarChar (ord k - ord 'A') m)
    (vigenere (ks ++ [k]) ms)

deVigenere :: String -> String -> String
deVigenere "" _ = error "No keyword provided"
deVigenere _ "" = ""
deVigenere ks (' ' : ms) = ' ' : deVigenere ks ms
deVigenere (k : ks) (m : ms) =
  (:)
    (unCaesarChar (ord k - ord 'A') m)
    (deVigenere (ks ++ [k]) ms)

caesarChar :: Int -> Char -> Char
caesarChar shift c =
  let startNum = ord 'A'
      delta = ord c - startNum
   in chr $ startNum + mod (delta + shift) 26

unCaesarChar :: Int -> Char -> Char
unCaesarChar shift c = caesarChar (- shift) c
