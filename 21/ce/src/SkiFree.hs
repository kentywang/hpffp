{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

-- instance ( Applicative n
--          , Testable (n Property)
--          , EqProp a )
--         => EqProp (S n a) where
--   (S x y) =-= (S p q) =
--         (property $ (=-=) <$> x <*> p)
--     .&. (y =-= q)
instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n
      => Foldable (S n) where
  foldMap :: Monoid m => (a -> m) -> S n a -> m
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n
      => Traversable (S n) where
  traverse :: Applicative f => (a -> f b) -> S n a -> f (S n b)
  traverse f (S n a) = liftA2 S (traverse f n) (f a)
  -- traverse f n :: f (n b)
  -- So what do we do with an `S`, an `f (n b)`, and an `f b`
  -- to get to `f (S n b)`?
  -- Ap, of course! This will lift the `S` into the `f` Applicative
  -- where it applies to `n b` and `b` to reach `S n b`.

main = do
  -- stuff <- sample' (arbitrary :: Gen (S [] Int))
  -- print stuff
  let t1a :: S [] (Int, Int, [Int])
      t1b :: S Maybe (Int, Int, [Int], Int, Int)
      t1a = undefined
      t1b = undefined
  quickBatch $ functor t1a
  quickBatch $ foldable t1b
  quickBatch $ traversable t1a