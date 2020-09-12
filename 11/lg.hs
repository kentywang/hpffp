{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Show, TooMany)

newtype Corgi = Corgi (Int , String) deriving (Show)  -- This is a unary data constructor!

instance TooMany Corgi where
  tooMany (Corgi (n, _)) = n >= 100

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany $ Goats $ x + y

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y