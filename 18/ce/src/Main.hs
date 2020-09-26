{-# LANGUAGE InstanceSigs #-}

module CE18 where

import Control.Monad ((>=>), join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap :: (a -> b) -> Nope a -> Nope b
  fmap _ _ = NopeDotJpg
  -- Interestingly, fmap _ x = x doesn't work because
  -- Expected type: Nope b
  -- Actual type: Nope a

instance Applicative Nope where
  pure :: a -> Nope a
  pure = \_ -> NopeDotJpg

  (<*>) :: Nope (a -> b) -> Nope a -> Nope b
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  (>>=) :: Nope a -> (a -> Nope b) -> Nope b
  _ >>= f = NopeDotJpg

-- 2

-- So opposite of Either; Left is the content, Right is the structure
-- (or error side).
data PhhhbbtttEither b a =
    Left a 
  | Right b 
  deriving (Eq, Show)

instance (Arbitrary b, Arbitrary a)
      => Arbitrary (PhhhbbtttEither b a) where
  arbitrary =
    arbitrary >>= \b ->
    arbitrary >>= \a ->
    elements [CE18.Left a, CE18.Right b]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance Functor (PhhhbbtttEither b) where
  fmap :: (a -> c) -> PhhhbbtttEither b a -> PhhhbbtttEither b c
  fmap f (CE18.Left a) = CE18.Left $ f a
  fmap _ (CE18.Right b) = CE18.Right b -- Again, can't do fmap _ x = x

instance Applicative (PhhhbbtttEither b) where
  pure :: a -> PhhhbbtttEither b a
  pure = CE18.Left

  (<*>) :: PhhhbbtttEither b (a -> c)
        -> PhhhbbtttEither b a
        -> PhhhbbtttEither b c
  CE18.Right b <*> CE18.Right b' = CE18.Right b
  CE18.Right b <*> CE18.Left a = CE18.Right b
  CE18.Left _ <*> CE18.Right b' = CE18.Right b'
  CE18.Left f <*> CE18.Left a = CE18.Left $ f a
  -- Instead of returning the leftmost Right, could also
  -- summarize the Rights with mappend (given Monoid constraint).
  -- But cannot return the rightmost Right, because that would
  -- violate the ap property for Monad, where (<*>) == ap,
  -- where ap is defined as:

  -- ap :: (Monad m) => m (a -> b) -> m a -> m b
  -- ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
  
  -- Or:
  
  -- ap m1 m2 = m1 >>= \x1 -> 
  --            m2 >>= \x2 ->
  --            return (x1 x2)
  
  -- This definition, in conjunction is (>>=)'s definition in the
  -- Monad instance, means if m1 is a Right, then the sequence
  -- short circuits, just returning that first, leftmost right.

instance Monad (PhhhbbtttEither b) where
  (>>=) :: PhhhbbtttEither b a
        -> (a -> PhhhbbtttEither b c)
        -> PhhhbbtttEither b c
  CE18.Right b >>= _ = CE18.Right b
  CE18.Left a >>= f = f a

-- 3

newtype Identity a = Identity a 
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where 
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where 
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where 
  return = pure
  
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a

-- 4

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
  
instance Applicative List where
  pure :: x -> List x
  pure = flip Cons Nil

  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f g <*> lst =
    flatMap y $ Cons f g
    where y = flip fmap lst

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil
  lst >>= f = flatMap f lst

-- 1
-- This works since id surprisingly fixs the type
-- signature (a -> m b) in this scenario, as
-- a & m b in this case are the same.
-- Seems like we can only unwrap nested layers, but
-- never the last layer.
j :: Monad m => m (m a) -> m a
j = (>>= id)
-- Previously:
-- j mma = mma >>= id
-- Previously:
-- j mma = mma >>= \ma -> ma >>= return
-- Previously:
-- j mma = mma >>= \ma -> ma >>= \a -> return a
-- Originally:
-- j mma = do
--   ma <- mma
--   a <- ma
--   return a

test1a = j [[1, 2], [], [3]]
-- [1,2,3]
test1b = j (Just (Just 1))
-- Just 1
test1c = j (Just Nothing)
-- Nothing
test1d = j Nothing
-- Nothing

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= \a ->
             mb >>= \b ->
             pure $ f a b
-- Using <*> from Applicative:
-- l2 f ma mb = f <$> ma <*> mb

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= \a ->
          mf >>= \f ->
          pure $ f a

-- 5
-- Challenging problem! Basically, we need to use each recursive
-- step to remove the monad of the head and the monad of the already
-- recursed tail of the list and just keep the monad on the outside
-- of the list.
meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh la f = ejectMonad $ fmap f la

-- AKA sequence in base?
ejectMonad :: Monad m => [m a] -> m [a]
ejectMonad [] = pure []
ejectMonad (ma:mas) = -- do
  -- a <- ma
  -- as <- ejectMonad mas
  -- return $ a : as
  ma >>= \a ->
    ejectMonad mas >>= \as ->
      return $ a : as
  {--
    Depending on the Monad and value, ejectMonad mas can
    short-circuit the evaluation (such as if it is a Nothing or a Left),
    resulting in propagating the failure to the top.
  --}

test5a = meh [2, 4, 6] (\n -> if even n then Just n else Nothing)
test5b = meh [2, 4, 5] (\n -> if even n then Just n else Nothing)

test5c = ejectMonad [Just 1, Just 2, Just 3] -- Just [1, 2, 3]
test5d = ejectMonad [Just 1, Just 2, Nothing] -- Nothing
test5e = ejectMonad [Nothing, Just 2, Just 3] -- Nothing

test5f = ejectMonad [ Prelude.Right 1
                    , Prelude.Right 2 ] -- Right [1, 2]
test5g = ejectMonad [ Prelude.Right 1
                    , Prelude.Left '2'
                    , Prelude.Left '3' ] -- Left '2'

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType = ejectMonad

main = do
  let ce1 :: Nope (Int, String, Int)
      ce1 = undefined
      ce2 :: PhhhbbtttEither (Int, String, Int) (Int, String, Int)
      ce2 = undefined
      ce3 :: Identity (Int, String, Int)
      ce3 = undefined
      ce4 :: List (Int, String, Int)
      ce4 = undefined
  -- quickBatch $ functor ce1
  -- quickBatch $ applicative ce1
  -- quickBatch $ monad ce1
  -- quickBatch $ functor ce2
  -- quickBatch $ applicative ce2
  -- quickBatch $ monad ce2 
  -- quickBatch $ functor ce3
  -- quickBatch $ applicative ce3
  -- quickBatch $ monad ce3
  quickBatch $ functor ce4
  quickBatch $ applicative ce4
  quickBatch $ monad ce4