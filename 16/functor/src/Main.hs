{-# LANGUAGE ViewPatterns #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) => f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Seems when I use this, the type is "locked"
-- to the first binding.
-- fc' = functorCompose'

type IntFC =
  [Int] -> Fun Int Int -> Fun Int Int -> Bool

-- 16.10 Instances of Func

-- 1

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

type IdFC =
  Identity Int -> Fun Int Int -> Fun Int Int -> Bool

-- 2

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

type PairFC =
  Pair Int -> Fun Int Int -> Fun Int Int -> Bool

-- 3

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoFC =
  Two Bool Int -> Fun Int Int -> Fun Int Int -> Bool

-- 4

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeFC =
  Three Bool Char Int -> Fun Int Int -> Fun Int Int -> Bool

-- 5

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

type Three'FC =
  Three' Bool Int -> Fun Int Int -> Fun Int Int -> Bool

-- 6

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourFC =
  Four Bool Char Bool Int -> Fun Int Int -> Fun Int Int -> Bool

-- 7

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

type Four'FC =
  Four' Bool Int -> Fun Int Int -> Fun Int Int -> Bool

-- 8

data Trivial = Trivial

-- Can't implement Functor instance because kind is *, not * -> *.

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where 
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    elements [LolNope, Yeppers a]

type PossiblyFC =
  Possibly Int -> Fun Int Int -> Fun Int Int -> Bool

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  -- Gen has Functor instance, so why not use it? :)
  arbitrary = frequency [ (1, fmap First arbitrary)
                        , (1, fmap Second arbitrary) ]

type SumFC =
  Sum Bool Int -> Fun Int Int -> Fun Int Int -> Bool

main :: IO ()
main = do
  quickCheck (functorCompose' :: IdFC)
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: PairFC)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck (functorCompose' :: Three'FC)
  quickCheck (functorCompose' :: FourFC)
  quickCheck (functorCompose' :: Four'FC)
  quickCheck (functorCompose' :: PossiblyFC)
  quickCheck (functorCompose' :: SumFC)
