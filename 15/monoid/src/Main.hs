module Main where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (a <> mempty) == a

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools) 
                        , (1, return Twoo) ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool


{-- Exercise: Maybe Another Monoid --}

data Optional a = 
  Nada | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  First' o@(Only _) <> _ = First' o
  First' Nada <> (First' o@(Only _)) = First' o
  _ <> _ = First' Nada

instance Monoid (First' a) where 
  mempty = First' Nada

firstPrimeGen :: Arbitrary a => Gen (First' a)
firstPrimeGen = do
  a <- arbitrary
  frequency [ (1, return (First' Nada)) 
            , (3, return (First' (Only a))) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstPrimeGen

-- firstMappend :: First' a -> First' a -> First' a
-- firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool
  
type FstId = First' String -> Bool

main :: IO ()
main = do
  -- let ma = monoidAssoc
  --     mli = monoidLeftIdentity
  --     mlr = monoidRightIdentity
  -- quickCheck (ma :: BullMappend)
  -- quickCheck (mli :: Bull -> Bool)
  -- quickCheck (mlr :: Bull -> Bool)
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
