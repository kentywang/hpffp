module Main where

import Data.Char ( ord, chr )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import Test.QuickCheck

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter message: "
  getLine >>= printMessage . vigenere "ALLY"
  where printMessage =
          putStrLn . ("The Vigenere code is: " ++)

test :: IO ()
test = do
  quickCheck caesarId
  quickCheck vigenereId 

-- Lifted from StackOverflow
genSafeChar :: Gen Char
genSafeChar = elements ['A'..'Z']

-- Lifted from StackOverflow
genSafeString :: Gen String
genSafeString = listOf genSafeChar

genSuperSafeString :: Gen String
genSuperSafeString = listOf1 genSafeChar

vigenere :: String -> String -> String
vigenere "" _ = error "No keyword provided"
vigenere _ "" = ""
vigenere ks (' ':ms) = ' ' : vigenere ks ms
vigenere (k:ks) (m:ms) =
  (:) (caesarChar (ord k - ord 'A') m)
      (vigenere (ks ++ [k]) ms)

deVigenere :: String -> String -> String
deVigenere "" _ = error "No keyword provided"
deVigenere _ "" = ""
deVigenere ks (' ':ms) = ' ' : deVigenere ks ms
deVigenere (k:ks) (m:ms) =
  (:) (unCaesarChar (ord k - ord 'A') m)
      (deVigenere (ks ++ [k]) ms)

repeatString :: String -> String
repeatString "" = ""
repeatString (x:xs) = x : (repeatString $ xs ++ [x])  

caesarChar shift c =
  let startNum = ord 'A'
      delta = ord c - startNum in
    chr $ startNum + mod (delta + shift) 26

unCaesarChar shift c = caesarChar (-shift) c

-- From before, modified to use caesarChar
caesar :: Int -> String -> String
caesar = map . caesarChar

unCaesar :: Int -> String -> String
unCaesar x = caesar (-x)

caesarId :: Property
caesarId = 
  forAll (arbitrary :: Gen Int) $ \x ->
  forAll genSafeString $ \y ->
    (unCaesar x (caesar x y)) == y

vigenereId :: Property
vigenereId = 
  forAll genSuperSafeString $ \x ->
  forAll genSafeString $ \y ->
    (deVigenere x (vigenere x y)) == y
