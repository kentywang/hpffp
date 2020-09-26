{-# LANGUAGE InstanceSigs #-}

module Main where

-- import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Sum a b = 
    First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [First x, Second y]
 
instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

dummySum :: Sum Bool (String, String, String)
dummySum = Second ("one", "two", "three")

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b 

instance Applicative (Sum a) where
  pure = Second

  {--
    It seems like when we encounter just "structure" as one of <*>'s
    arguments (e.g. 'First a', since 'Sum a' is the structure),
    we're obliged to return that structure for the result of the <*>
    evaluation because returning the "content" argument (e.g. 'Second b')
    would violate the output type definition of 'Sum a c', since b /= c,
    whereas returning the "structure" argument wouldn't, since it doesn't
    even contain anything about the content type (and thus is polymorphic
    for that, I guess).
  --}
  (<*>) :: Sum a (b -> c) -> Sum a b -> Sum a c
  First a <*> _ = First a
  _ <*> First a' = First a'
  Second b <*> Second b' = Second $ b b'

instance Monad (Sum a) where 
  return = pure

  (>>=) :: Sum a b -> (b -> Sum a c) -> Sum a c
  First a >>= _ = First a
  Second b >>= f = f b
  -- Why doesn't the following work? It makes the checker hang.
  -- someSecond >>= f = join $ f <$> someSecond
  -- Maybe it's because you can't define >>= in terms of join. 

test :: Sum String String
test = do
  hi <- Second "hi"
  there <- First "there"
  friendo <- Second "friendo"
  pure $ hi ++ there ++ friendo

main :: IO ()
main = do
  quickBatch $ monad dummySum