{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

listPure :: a -> [a]
listPure = pure

listAp :: [a -> b] -> [a] -> [b]
listAp = (<*>)

ioPure :: a -> IO a
ioPure = pure

ioAp :: IO (a -> b) -> IO a -> IO b
ioAp = (<*>)

tuPure :: Monoid s => a -> (s, a)
tuPure = pure

tuAp :: Monoid s => (s, a -> b) -> (s, a) -> (s, b)
tuAp = (<*>)

tuTest = tuPure (+1) <*> (3 :: Sum Int, 4)

fnPure :: a -> e -> a
fnPure = pure

fnAp :: (e -> a -> b) -> (e -> a) -> e -> b
fnAp = (<*>)

fnTest = fnPure 2 ()
fnTest2 = fnAp (\n -> (*n)) (+1) 3
-- Ah, so the applied function utilizes as its arguments
-- both the argument to the firstmost applied function in the
-- composition and the output of that firstmost applied function.
-- This is why f x = g x (h x) is equivalent to f = ap g h.

-- 1

data Pair a = Pair a a deriving (Eq, Show)

dummyPair = Pair ("o", "m", "g") ("l", "o", "l")

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure :: a -> Pair a
  pure a = Pair a a

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  Pair f g <*> Pair x y = Pair (f x) (g y)

pairTest = Pair (+1) (*2) <*> Pair 3 4 -- Pair 4 8

-- 2

-- This is essentially the behavior of a tuple.
data Two a b = Two a b deriving (Eq, Show)

dummyTwo = Two "omg" ("l", "o", "l")

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two s) where
  fmap :: (a -> b) -> Two s a -> Two s b
  fmap f (Two s a) = Two s (f a)

instance Monoid s => Applicative (Two s) where
  pure :: a -> Two s a
  pure a = Two mempty a

  (<*>) :: Two s (a -> b) -> Two s a -> Two s b
  Two u f <*> Two v x = Two (u <> v) (f x)

twoTest = Two [1] (*2) <*> Two [3] 4 -- Two [1, 3] 8

-- 3

data Three a b c = Three a b c deriving (Eq, Show)

dummyThree = Three "omg" [1 :: Int] ("l", "o", "l")

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three s t) where
  fmap :: (a -> b) -> Three s t a -> Three s t b
  fmap f (Three s t a) = Three s t (f a)

instance (Monoid s, Monoid t) => Applicative (Three s t) where
  pure :: a -> Three s t a
  pure a = Three mempty mempty a

  (<*>) :: Three s t (a -> b) -> Three s t a -> Three s t b
  Three u a f <*> Three v b x = Three (u <> v) (a <> b) (f x)

threeTest = Three [1] "omg" (*2) <*> Three [3] "lol" 4
-- Three [1, 3] "omg" 8

-- 4

data Three' a b = Three' a b b deriving (Eq, Show)

dummyThree' = Three' "omg" ("h", "e", "y") ("l", "o", "l")

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' s) where
  fmap :: (a -> b) -> Three' s a -> Three' s b
  fmap f (Three' s a a') = Three' s (f a) (f a')

instance (Monoid s) => Applicative (Three' s) where
  pure :: a -> Three' s a
  pure a = Three' mempty a a

  (<*>) :: Three' s (a -> b) -> Three' s a -> Three' s b
  Three' u f g <*> Three' v x y = Three' (u <> v) (f x) (g y)

three'Test = Three' [1] (+1) (*2) <*> Three' [2] 3 4
-- Three' [1, 2] 4 8

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

dummyFour = Four "omg" [1 :: Int] "hey" ("l", "o", "l")

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    o <- arbitrary
    return $ Four x y z o

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four s t u) where
  fmap :: (a -> b) -> Four s t u a -> Four s t u b
  fmap f (Four s t u a) = Four s t u (f a)

instance (Monoid s, Monoid t, Monoid u) =>
         Applicative (Four s t u) where
  pure :: a -> Four s t u a
  pure a = Four mempty mempty mempty a

  (<*>) :: Four s t u (a -> b) -> Four s t u a -> Four s t u b
  Four u a m f <*> Four v b n x = Four (u <> v) (a <> b) (m <> n) (f x)

fourTest = Four [1] "omg" [2] (*2) <*> Four [3] "lol" [5] 4
-- Four [1, 3] "omglol" [2, 5] 8

-- 6

data Four' a b = Four' a a a b deriving (Eq, Show)

dummyFour' = Four' "omg" "omg" "omg" ("l", "o", "l")

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    o <- arbitrary
    return $ Four' x y z o

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' s) where
  fmap :: (a -> b) -> Four' s a -> Four' s b
  fmap f (Four' s t u a) = Four' s t u (f a)

instance (Monoid s) => Applicative (Four' s) where
  pure :: a -> Four' s a
  pure a = Four' mempty mempty mempty a

  (<*>) :: Four' s (a -> b) -> Four' s a -> Four' s b
  Four' u a m f <*> Four' v b n x = Four' (u <> v) (a <> b) (m <> n) (f x)

four'Test = Four' [1] [2] [3] (*2) <*> Four' [4] [5] [6] 4
-- Four' [1, 4] [2, 5] [3, 6] 8

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
{-
Same as
  pure (,,) <*> stops <*> vowels <*> stops
or
  (,,) <$> xs <*> ys <*> zs
= [(,,) x1, (,,) x2, ...] <*> ys <*> zs
= [(,,) x1 y1, (,,) x1 y2, ..., (,,) x2 y1, ...] <*> zs
= [(x1, y1, z1), (x1, y1, z2), ... (x1, y2, z1), ...]
-}
combosTest = combos stops vowels stops

main :: IO ()
main = do
  quickBatch $ applicative dummyPair
  -- quickBatch $ applicative dummyTwo
  -- quickBatch $ applicative dummyThree
  -- quickBatch $ applicative dummyThree'
  -- quickBatch $ applicative dummyFour
  -- quickBatch $ applicative dummyFour'