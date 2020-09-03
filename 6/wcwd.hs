data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

instance Ord Papu where
  compare (Papu _ _) (Papu _ _) = EQ 

--1.
phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. Needed Ord on datatypes
-- If derived Ord, then subtypes need it too.
-- Bypassable if I instantiate Ord myself.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'