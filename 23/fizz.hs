import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n`mod`5 == 0 = "Buzz"
           | n`mod`3 == 0 = "Fizz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main =
  -- mapM_ putStrLn $
  --   reverse $ fizzbuzzList [1..100]
  mapM_ putStrLn $ fizzbuzzFromTo 1 100

rtlMapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
rtlMapM_ f xs =
  foldr g (pure ()) xs
-- We just want the monadic structure, discarding its inner value.
    where g cur acc = do
            acc
            _ <- f cur
            return ()
-- When the monad is State, then this generates the new State and
-- returns a wrapped unit, with the new State's state implicitly
-- included in the returned structure.
-- If we don't sequence acc, it will only run g once from the left,
-- thus only mapping the first value.

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start end = execState (rtlMapM_ addResult [start..end]) []