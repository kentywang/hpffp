{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where 
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where 
  mempty = Fools

instance EqProp Bull where 
  (=-=) = eq

-- Ziplist

instance Semigroup a => Semigroup (ZipList' a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty

-- instance Arbitrary a => Arbitrary (ZipList' a) where
--   arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

-- instance Eq a => EqProp (ZipList a) where
--   (=-=) = eq

-- Suggested helper functions

append :: List a -> List a -> List a
append Nil xs = xs
append (Cons a b) xs = Cons a $ append b xs

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b 
-- Courtesy of http://pointfree.io, of course.
flatMap = (concat' .) . fmap
-- flatMap f = concat' . (fmap f)

-- List

data List a = 
    Nil
  | Cons a (List a) 
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, Cons a b]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)
  
-- Don't think we can use a Monoid instance for List
-- here since we're dealing with a higher-kinded type
-- List at the type level.
instance Applicative List where
  pure :: x -> List x
  pure = flip Cons Nil

  -- Didn't initially think to flatmap over the wrapped functions instead of
  -- the wrapped values, but that's the way to get it in the right
  -- order.
  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f g <*> lst =
    flatMap y $ Cons f g
    where y = flip fmap lst
    -- Parser can't tell that this "a" is the same
    -- as the "a" in <*>'s type signature
    -- y :: (a -> b) -> List b
  -- Original:
  -- Cons f g <*> Cons a b =
  --   Cons (f a) $ append (fmap f b)
  --                       (g <*> Cons a b) 

-- ZipList'

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x y)
  | n > 0 = Cons x $ take' (n-1) y
  | otherwise = Nil

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ZipList' Nil, ZipList' $ Cons a b]

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l
          ys' = let (ZipList' l) = ys 
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
  pure :: x -> ZipList' x
  pure x = ZipList' $ repeat' x

  -- Below is wrong! Since for identity that would create
  -- ZipList' (Cons id nil) which would then apply to just the
  -- first element of the other ZipList'.
  -- pure = ZipList' . pure

  -- Then I thought the solution was to
  -- not wrap the argument in a List, but expect
  -- it was already wrapped:
  -- pure (Cons a b) = ZipList' (Cons a b)
  -- pure Nil = ZipList' Nil
  -- But then how would an atom (that is, non-list)
  -- be the arg?

  -- And then I realized that the argument can't
  -- be assumed to be a List. So in order to satisfy
  -- the identity law, we need to repeat the argument
  -- infinitely in the ZipList'. 

  (<*>) :: ZipList' (a -> b)
        -> ZipList' a
        -> ZipList' b
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' fs <*> ZipList' as =
    ZipList' $ zap' fs as

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zap' :: List (a -> b) -> List a -> List b
zap' Nil _ = Nil
zap' _ Nil = Nil
zap' (Cons f g) (Cons a b) = Cons (f a) $ zap' g b

dummyZl :: ZipList' (String, Sum Int, String)
dummyZl = ZipList' $ Cons ("one", Sum 2, "tree")
                          Nil

-- Validation

-- Renamed with single quote to avoid ambiguity.
data Validation e a = Failure' e | Success' a deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

-- same as Either
instance Functor (Validation e) where
  fmap f (Success' a) = Success' $ f a
  fmap _ (Failure' e) = Failure' e

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' e) (Failure' e') = Failure' $ e <> e'
  (<*>) (Failure' e) _ = Failure' e
  (<*>) _ (Failure' e') = Failure' e'
  (<*>) (Success' a) (Success' a') = Success' $ a a'

dummyValidation :: Validation [Char] ((,,) [Char] Bool [Sum Int])
dummyValidation = Success' ("one", False, [0 :: Sum Int]);

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative $ Just ("one", 2 :: Int, False))
  quickBatch $ applicative $ Cons (1 :: Sum Int, False, "one") Nil
  quickBatch $ monoid dummyZl
  quickBatch $ applicative dummyZl
  quickBatch $ applicative dummyValidation