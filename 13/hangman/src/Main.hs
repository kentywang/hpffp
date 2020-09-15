-- src/Main.hs
module Main where

import Test.Hspec
import Control.Monad (forever)
import Data.Char (toUpper, toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 5

maxWrongGuessesAllowed :: Int  -- KW
maxWrongGuessesAllowed = maxWordLength + 20

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l < maxWordLength


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = do
  x <- gameWords
  randomWord x
-- randomWord' = gameWords >>= randomWord

data Puzzle =  -- KW: Made into record syntax and added new field.
  Puzzle { puzzle :: String
         , discovered :: [Maybe Char]
         , guessed :: [Char]
         , chancesLeft :: Int }
         deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed chancesLeft) =
    fmap toUpper (intersperse ' ' $
                  fmap renderPuzzleChar discovered)
                  -- Regular map should work too.
    ++ "\nGuessed so far: " ++ guessed
    ++ "\nChances left: " ++ show chancesLeft

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s 
                       (map (const Nothing) s)
                       []
                       maxWrongGuessesAllowed
  -- Without const, this would have to be my mapping function:
  -- f :: Char -> Maybe Char
  -- f _ = Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) = flip elem s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ xs _) = flip elem xs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c 
renderPuzzleChar _ = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s chancesLeft) c = 
  Puzzle word newFilledInSoFar (c : s) newChancesLeft
  where newChancesLeft =
          case elem c word of
            True  -> chancesLeft
            False -> chancesLeft - 1
        zipper guessed wordChar guessChar =     
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =  
          zipWith (zipper c)
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of 
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed chancesLeft) =
  if chancesLeft == 0 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess 
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle    -- KW: Switched order with gameOver
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: " -- Doesn't show up unlees no buffering.
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame -- Why need forever if we're recursing? Guess it's only
    _   ->                                  -- because we have that 2nd case that isn't recursive.
      putStrLn "Your guess must\
              \ be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering  -- KW
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

test :: IO ()
test = hspec $ do
  describe "fillInCharacter" $ do
    it "adds char to discovered and guessed" $ do
      let p = Puzzle { puzzle = "cat"
                       , discovered = [Nothing, Nothing, Nothing]
                       , guessed = []
                       , chancesLeft = 3 }
          q = Puzzle { puzzle = "cat"
                      , discovered = [Just 'c', Nothing, Nothing]
                      , guessed = "c"
                      , chancesLeft = 3 }
      fillInCharacter p 'c' `shouldBe` q
    it "updates chancesLeft and guessed" $ do
      let p = Puzzle { puzzle = "cat"
                       , discovered = [Nothing, Nothing, Nothing]
                       , guessed = []
                       , chancesLeft = 3 }
          q = Puzzle { puzzle = "cat"
                      , discovered = [Nothing, Nothing, Nothing]
                      , guessed = "b"
                      , chancesLeft = 2 }
      fillInCharacter p 'b' `shouldBe` q
  describe "handleGuess" $ do
    it "doesn't change puzzle if already guessed" $ do
      let p = Puzzle { puzzle = "cat"
                    , discovered = [Nothing, Nothing, Nothing]
                    , guessed = "c"
                    , chancesLeft = 3 }
      handleGuess p 'c' >>= shouldBe p
    it "updates puzzle when char is right" $ do
      let p = Puzzle { puzzle = "cat"
                       , discovered = [Nothing, Nothing, Nothing]
                       , guessed = []
                       , chancesLeft = 3 }
          q = Puzzle { puzzle = "cat"
                       , discovered = [Just 'c', Nothing, Nothing]
                       , guessed = "c"
                       , chancesLeft = 3 }
      handleGuess p 'c' >>= shouldBe q
    it "updates puzzle when char is wrong" $ do
      let p = Puzzle { puzzle = "cat"
                      , discovered = [Nothing, Nothing, Nothing]
                      , guessed = []
                      , chancesLeft = 3 }
          q = Puzzle { puzzle = "cat"
                      , discovered = [Nothing, Nothing, Nothing]
                      , guessed = "b"
                      , chancesLeft = 2 }
      handleGuess p 'b' >>= shouldBe q