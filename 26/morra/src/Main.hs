{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Class as C
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State as S
import Data.Bool
import System.Random (randomRIO)
import Text.Read

data Score = Score
  { player :: Integer,
    computer :: Integer
  }
  deriving (Show)

-- MaybeT (StateT Score IO) Integer
-- MaybeT $ IO (Maybe Integer)
getNum :: String -> MaybeT IO Integer
getNum s = do
  liftIO $ putStrLn s
  MaybeT $ readMaybe <$> getLine -- Return `Nothing` if not an integer

updateScore :: Score -> Integer -> Integer -> Integer -> Score
updateScore (Score playerScore cpuScore) guess guessCpu sum =
  let f = bool 0 1 . (== sum)
   in Score (playerScore + f guess) (cpuScore + f guessCpu)

morra :: Score -> IO ()
morra score =
  maybe (playRound score) (printWinner score) $ checkWinner score

data Winner = Player | Computer deriving (Show)

checkWinner :: Score -> Maybe Winner
checkWinner (Score playerScore cpuScore)
  | playerScore == cpuScore && playerScore >= 3 = Nothing
  | playerScore >= 3 = Just Player
  | cpuScore >= 3 = Just Computer
  | otherwise = Nothing

printWinner :: Score -> Winner -> IO ()
printWinner score winner = do
  putStrLn $ "Congrats to " <> show winner <> "!"
  print score

playRound :: Score -> IO ()
playRound score = do
  score' <- runMaybeT $ do
    -- So we short-circuit within this, huh?
    fngrs <- getNum "How many fingers do you hold out?"
    guess <- getNum "What's your guess?"
    fngrsCpu <- liftIO $ randomRIO @Integer (0, 5) -- [1]
    guessCpu <- liftIO $ randomRIO @Integer (0, 10)
    liftIO $
      putStrLn $
        mconcat
          [ "CPU shows ",
            show fngrsCpu,
            " fingers and guesses ",
            show guessCpu
          ]
    pure $ updateScore score guess guessCpu $ fngrs + fngrsCpu
  case score' of
    Just s -> putStrLn (show s <> "\n") >> morra s
    Nothing -> playRound score

-- Using StateT:

-- StateT $ Score -> IO (a, Score)
-- StateT Score IO a
morra' :: S.StateT Score IO ()
morra' = do
  -- score <- S.get
  -- liftIO $ morra score -- This works, but barely uses StateT.
  mbWinner <- checkWinner'
  -- maybe playRound' printWinner' mbWinner
  maybe player1Round printWinner' mbWinner -- [2]

checkWinner' :: S.StateT Score IO (Maybe Winner)
checkWinner' = do
  score <- S.get
  pure $ checkWinner score

printWinner' :: Winner -> S.StateT Score IO ()
printWinner' winner = do
  liftIO $ putStrLn $ "Congrats to " <> show winner <> "!"
  S.get >>= liftIO . print

data PlayerInput = PlayerInput
  { fingers :: Integer,
    guess :: Integer
  }

getPlayerInput :: S.StateT Score IO (Maybe PlayerInput)
getPlayerInput = liftIO $
  runMaybeT $ do
    fngrs <- getNum "How many fingers do you hold out?"
    guess <- getNum "What's your guess?"
    pure $ PlayerInput fngrs guess

playRound' :: S.StateT Score IO ()
playRound' = do
  mbInput <- getPlayerInput
  case mbInput of
    Nothing -> playRound'
    Just playerInput -> do
      cpuInput <- getCpuInput
      liftIO $ printCpuInput cpuInput
      updateScore' playerInput cpuInput
      score <- S.get
      liftIO $ putStrLn (show score <> "\n")
      morra'

player1Round :: S.StateT Score IO ()
player1Round = do
  mbInput <- getPlayerInput
  twentyLines
  maybe player1Round player2Round mbInput

twentyLines = liftIO $ putStr $ replicate 20 '\n'

-- Could be refactored to eliminate duplication
player2Round :: PlayerInput -> S.StateT Score IO ()
player2Round player1Input = do
  mbInput <- getPlayerInput
  twentyLines
  case mbInput of
    Nothing -> player2Round player1Input
    Just player2Input -> do
      liftIO $ printCpuInput player1Input
      liftIO $ printCpuInput player2Input
      updateScore' player1Input player2Input
      score <- S.get
      liftIO $ putStrLn (show score <> "\n")
      morra'

updateScore' :: PlayerInput -> PlayerInput -> S.StateT Score IO ()
updateScore' playerInput cpuInput = do
  let sum = fingers playerInput + fingers cpuInput
  S.modify $ bool id raisePlayer1Score (guess playerInput == sum)
  S.modify $ bool id raisePlayer2Score (guess cpuInput == sum)

raisePlayer1Score :: Score -> Score
raisePlayer1Score (Score p1 p2) = Score (p1 + 1) p2

raisePlayer2Score :: Score -> Score
raisePlayer2Score (Score p1 p2) = Score p1 $ p2 + 1

printCpuInput (PlayerInput fngrsCpu guessCpu) =
  putStrLn $
    mconcat
      [ "CPU shows ",
        show fngrsCpu,
        " fingers and guesses ",
        show guessCpu
      ]

getCpuInput :: S.StateT Score IO PlayerInput
getCpuInput = do
  fngrsCpu <- liftIO $ randomRIO @Integer (0, 5)
  guessCpu <- liftIO $ randomRIO @Integer (0, 10)
  pure $ PlayerInput fngrsCpu guessCpu

main :: IO ()
main = do
  -- morra $ Score 0 0
  S.evalStateT morra' $ Score 0 0

{--

Hmm, so rewriting an app into into StateT involves a flexible mix of rewriting
functions with the StateT monad and embedding existing functions within
a StateT monad. It can be more or less comprehensive or depending on my preference,
I guess.

[1]: Why need liftIO here? Why can't randomRIO just return the MaybeT?
[2]: For 2-player mode.

--}