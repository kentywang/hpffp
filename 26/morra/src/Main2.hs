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

data Winner = Player | Computer deriving (Show)

data PlayerInput = PlayerInput
  { fingers :: Integer }

getNum :: String -> MaybeT IO Integer
getNum s = do
  liftIO $ putStrLn s
  MaybeT $ readMaybe <$> getLine -- Return `Nothing` if not an integer

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

morra' :: S.StateT Score IO ()
morra' = do
  mbWinner <- checkWinner'
  maybe playRound' printWinner' mbWinner

checkWinner' :: S.StateT Score IO (Maybe Winner)
checkWinner' = do
  score <- S.get
  pure $ checkWinner score

printWinner' :: Winner -> S.StateT Score IO ()
printWinner' winner = do
  liftIO $ putStrLn $ "Congrats to " <> show winner <> "!"
  S.get >>= liftIO . print

getPlayerInput :: S.StateT Score IO (Maybe PlayerInput)
getPlayerInput = liftIO $
  runMaybeT $ do
    fngrs <- getNum "How many fingers do you hold out?"
    pure $ PlayerInput fngrs

playRound' :: S.StateT Score IO ()
playRound' = do
  mbInput <- getPlayerInput
  case mbInput of
    Nothing -> playRound'
    Just playerInput -> do
      cpuInput <- getCpuInput
      liftIO $ printInputs playerInput cpuInput
      updateScore' playerInput cpuInput
      score <- S.get
      liftIO $ putStrLn (show score <> "\n")
      morra'

updateScore' :: PlayerInput -> PlayerInput -> S.StateT Score IO ()
updateScore' playerInput cpuInput = do
  let sum = fingers playerInput + fingers cpuInput
  S.modify $ bool raisePlayer2Score raisePlayer1Score (odd sum)

raisePlayer1Score :: Score -> Score
raisePlayer1Score (Score p1 p2) = Score (p1 + 1) p2

raisePlayer2Score :: Score -> Score
raisePlayer2Score (Score p1 p2) = Score p1 $ p2 + 1

printInputs (PlayerInput fngrsPlayer) (PlayerInput fngrsCpu) =
  putStrLn $
    mconcat
      [ "CPU shows ",
        show fngrsCpu,
        " fingers, making the total finger: ",
        show $ fngrsPlayer + fngrsCpu
      ]

getCpuInput :: S.StateT Score IO PlayerInput
getCpuInput = do
  fngrsCpu <- liftIO $ randomRIO @Integer (0, 5)
  pure $ PlayerInput fngrsCpu

main :: IO ()
main = do
  S.evalStateT morra' $ Score 0 0

-- This version is 'Odds and Evens'. Didn't actually add the CPU learning behavior.