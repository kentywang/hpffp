{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  
instance Functor Identity where
  fmap f (Identity x) = Identity (f x) 

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity x = Identity (f x)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
  pure :: b -> Constant a b
  pure _ = Constant mempty

  (<*>) :: Constant a (b -> c) 
        -> Constant a b 
        -> Constant a c
  Constant notTheF <*> Constant notTheArg =
    Constant (notTheF <> notTheArg)
  -- Why is the correct answer to mappend the
  -- function with the value? I thought "...we have
  -- to map our function over the argument that
  -- gets discarded. So there is no value to map
  -- over, and the function application doesn’t
  -- happen." And shouldn't we not modify the 
  -- structure? Here, the structure is altered
  -- via mappend (while staying the in the same
  -- closed set). And for Constant, there's
  -- nothing but structure.

one :: Maybe [Char]
one = const <$> Just "Hello" <*> pure "World"

two :: Num a => Maybe (a, a, String, [a])
two = (,,,) <$> Just 90
            <*> Just 10
            <*> Just "Tierness" 
            <*> pure [1, 2, 3]