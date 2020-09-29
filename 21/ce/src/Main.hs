{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (liftA2, liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= pure . Identity

instance Functor Identity where 
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where 
  sequenceA :: Applicative f => Identity (f a) -> f (Identity a)
  sequenceA (Identity fa) = Identity <$> fa 

-- Constant

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = arbitrary >>= pure . Constant

instance Functor (Constant a) where
  fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap :: Monoid m => (b -> m) -> Constant a b -> m
  foldMap _ _ = mempty

instance Traversable (Constant a) where 
  sequenceA :: Applicative f => Constant a (f b) -> f (Constant a b)
  sequenceA (Constant a) = pure $ Constant a

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where 
  sequenceA :: Applicative f => Optional (f a) -> f (Optional a)
  sequenceA Nada = pure Nada
  sequenceA (Yep fa) = Yep <$> fa

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, Cons a b]

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where 
  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons fa fas) = liftA2 Cons fa $ sequenceA fas
    -- pure Cons <*> fa <*> sequenceA fas
    -- Cons <$> fa <*> sequenceA fas
    -- Without Monad constraint, can't use do or bind, but still can 
    -- achieve a similar solution as Ch 18's ejectMonad!

-- Three

data Three a b c =
  Three a b c
  deriving (Eq, Show)

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

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three s t) where 
  sequenceA :: Applicative f => Three s t (f a) -> f (Three s t a)
  sequenceA (Three s t fa) = Three s t <$> fa

-- Pair

data Pair a b = Pair a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance Functor (Pair s) where
  fmap :: (a -> b) -> Pair s a -> Pair s b
  fmap f (Pair s a) = Pair s (f a)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair s) where
  traverse :: Applicative f => (a -> f b) -> Pair s a -> f (Pair s b)
  traverse f (Pair s a) = Pair s <$> f a

-- Big

data Big a b =
  Big a b b
  deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    pure $ Big a b b'

instance Functor (Big s) where
  fmap :: (a -> b) -> Big s a -> Big s b
  fmap f (Big s a a') = Big s (f a) (f a')

instance Foldable (Big a) where
  foldMap :: (Monoid m) => (b -> m) -> Big a b -> m
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse :: Applicative f => (y -> f z) -> Big x y -> f (Big x z)
  traverse f (Big x y y') = Big x <$> f y <*> f y'
    -- liftA2 (Big x) (f y) (f y')

-- Bigger

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    b'' <- arbitrary
    pure $ Bigger a b b' b''

instance Functor (Bigger s) where
  fmap :: (a -> b) -> Bigger s a -> Bigger s b
  fmap f (Bigger s a a' a'') = Bigger s (f a) (f a') (f a'')

instance Foldable (Bigger a) where
  foldMap :: (Monoid m) => (b -> m) -> Bigger a b -> m
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse :: Applicative f => (y -> f z) -> Bigger x y -> f (Bigger x z)
  traverse f (Bigger x y y' y'') = liftA3 (Bigger x) (f y) (f y') (f y'')

-- Tree

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a) 
            deriving (Eq, Show)


instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [Empty, Leaf a, Node b a c]

instance Functor Tree where 
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node ta b tc) = Node (fmap f ta) (f b) (fmap f tc)

-- foldMap is a bit easier 
-- and looks more natural, 
-- but you can do foldr too 
-- for extra credit.
instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node ta b tc) = mconcat [foldMap f ta, f b, foldMap f tc]

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node ta b tc) = 
    foldr f w ta
    where w = f b (foldr f z tc)
    -- Accumulate right branch first, then fold that into current node's value,
    -- then fold that the left branch.

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = fmap Leaf $ f a
  traverse f (Node ta b tc) = 
    liftA3 Node (traverse f ta)
                (f b)
                (traverse f tc)

main :: IO ()
main = do
  let t1a :: Identity (Int, Int, [Int])
      t1b :: Identity (Int, Int, [Int], Int, Int)
      t1a = undefined
      t1b = undefined
      t2a :: Constant Int (Int, Int, [Int])
      t2b :: Constant Int (Int, Int, [Int], Int, Int)
      t2a = undefined
      t2b = undefined
      t3a :: Optional (Int, Int, [Int])
      t3b :: Optional (Int, Int, [Int], Int, Int)
      t3a = undefined
      t3b = undefined
      t4a :: List (Int, Int, [Int])
      t4b :: List (Int, Int, [Int], Int, Int)
      t4a = undefined
      t4b = undefined
      t5a :: Three Int Int (Int, Int, [Int])
      t5b :: Three Int Int (Int, Int, [Int], Int, Int)
      t5a = undefined
      t5b = undefined
      t6a :: Pair Int (Int, Int, [Int])
      t6b :: Pair Int (Int, Int, [Int], Int, Int)
      t6a = undefined
      t6b = undefined
      t7a :: Big Int (Int, Int, [Int])
      t7b :: Big Int (Int, Int, [Int], Int, Int)
      t7a = undefined
      t7b = undefined
      t8a :: Bigger Int (Int, Int, [Int])
      t8b :: Bigger Int (Int, Int, [Int], Int, Int)
      t8a = undefined
      t8b = undefined
      t9a :: Tree (Int, Int, [Int])
      t9b :: Tree (Int, Int, [Int], Int, Int)
      t9a = undefined
      t9b = undefined
  -- quickBatch $ functor t1a
  -- quickBatch $ foldable t1b
  -- quickBatch $ traversable t1a
  -- quickBatch $ functor t2a
  -- quickBatch $ foldable t2b
  -- quickBatch $ traversable t2a
  -- quickBatch $ functor t3a
  -- quickBatch $ foldable t3b
  -- quickBatch $ traversable t3a
  -- quickBatch $ functor t4a
  -- quickBatch $ foldable t4b
  -- quickBatch $ traversable t4a
  -- quickBatch $ functor t5a
  -- quickBatch $ foldable t5b
  -- quickBatch $ traversable t5a
  -- quickBatch $ functor t6a
  -- quickBatch $ foldable t6b
  -- quickBatch $ traversable t6a
  -- quickBatch $ functor t7a
  -- quickBatch $ foldable t7b
  -- quickBatch $ traversable t7a
  -- quickBatch $ functor t8a
  -- quickBatch $ foldable t8b
  -- quickBatch $ traversable t8a
  quickBatch $ functor t9a
  quickBatch $ foldable t9b
  quickBatch $ traversable t9a