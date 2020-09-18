import Data.Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
   mempty = Nada

-- Need Semigroup for the associative parts, and Monoid
-- for the identity part.
instance Semigroup a => Semigroup (Optional a) where
  Only a <> Only b = Only (a <> b)
  _ <> (Only b) = Only b
  (Only a) <> _ = Only a
  _ <> _ = Nada

main = do
  print $ 
    Only (Sum 1) `mappend` Only (Sum 1)
    -- Only (Sum {getSum = 2})