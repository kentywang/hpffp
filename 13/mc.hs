import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let l = clean line1 in
    case (l == reverse l) of
      True -> putStrLn "It's a palindrome!"
      False -> do putStrLn "Nope!"
                  exitSuccess

clean :: String -> String
clean = map toLower . filter isAlpha

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = 
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name
        ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  age <- getLine
  case (mkPerson name $ read age) of
    Right p -> 
      putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left (PersonInvalidUnknown msg) ->
      putStrLn $ "Error: " ++ msg
    Left x -> putStrLn $ "Error: " ++ show x