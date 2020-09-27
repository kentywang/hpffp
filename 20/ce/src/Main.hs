{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Monoid
import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

-- 1
-- This is the opposite of Data.Functor.Constant.
data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = arbitrary >>= pure . Constant
  
-- Can treat the Constant a in the type level as
-- essentially a single type constructor.
instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2
data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ Two a b
  
-- Like a tuple's Foldable.
instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    pure $ Three a b c
  
instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    pure $ Three' a b b'
  
instance Foldable (Three' a) where
  foldMap :: (Monoid m) => (b -> m) -> Three' a b -> m
  foldMap f (Three' _ b b') = f b <> f b'
  -- Technically still passes the property tests even with just "f b",
  -- but it makes more sense to keep both values by appending them.

-- 5
data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    b'' <- arbitrary
    pure $ Four' a b b' b''
  
instance Foldable (Four' a) where
  foldMap :: (Monoid m) => (b -> m) -> Four' a b -> m
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

main :: IO ()
main = do
  let ce1 :: Constant Bool (String, String, String, Sum Int, All)
      ce1 = undefined
      ce2 :: Two Bool (String, String, String, Sum Int, All)
      ce2 = undefined
      ce3 :: Three Int Int (String, String, String, Sum Int, All)
      ce3 = undefined
      ce4 :: Three' Int
                   (String, String, String, Sum Int, All) 
      ce4 = undefined
      ce5 :: Four' Int
                   (String, String, String, Sum Int, All) 
      ce5 = undefined
  quickBatch $ foldable ce1
  quickBatch $ foldable ce2
  quickBatch $ foldable ce3
  quickBatch $ foldable ce4
  quickBatch $ foldable ce5