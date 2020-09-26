{-# LANGUAGE InstanceSigs #-}

module BadMonad where

import Control.Monad ((>=>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) =
    CountMe i (f a)

instance Applicative CountMe where 
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a) 
    
instance Monad CountMe where
  return = pure

  -- RI: return x >>= f == f x
  -- LI: x >>= return == x
  (>>=) :: CountMe a -> (a -> CountMe b) -> CountMe b
  CountMe n a >>= f =
    let CountMe n' b = f a -- Interesting syntax!
    in CountMe (n + n') b
    -- This also works:
    -- CountMe (n + n') b
    -- where CountMe n' b = f a

instance Arbitrary a
      => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary -- Neat!

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

sayHi :: String -> IO String 
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a 
readM = return . read

getAge :: String -> IO Int 
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "