module Main where

import System.Random

{--
  This works, so don’t get why the book says the IO Applicative instance
  can’t specify the order of the execution of the actions or have actions
  contingent on effects from other actions, while the Monad instance can?
--}
main :: IO ()
main = do
  blah <- huehue
  either (>>= print) id blah

f :: Int -> Either (IO Int) (IO ())
f = \i -> case even i of
  True -> Left $ return i
  False -> Right $ putStrLn "Hola"

huehue :: IO (Either (IO Int) (IO ()))
huehue = f <$> (randomIO :: IO Int)