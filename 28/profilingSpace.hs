-- module Main where

-- import Control.Monad

-- blah :: [Integer]
-- blah = [1..1000]

-- main :: IO ()
-- main = replicateM_ 1000 (print blah)

module Main where

import Control.Monad

incdInts :: [Integer] -> [Integer] 
-- not a CAF
incdInts x = map (+1) x
-- Gonna be a CAF this time.
-- incdInts = map (+1)

main :: IO ()
main = do
  replicateM_ 100000 $ print $ incdInts [1..] !! 1000000