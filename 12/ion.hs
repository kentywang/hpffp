-- As natural as any
-- competitive bodybuilder
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

-- > natToInteger Zero
-- 0
-- > natToInteger (Succ Zero)
-- 1
-- > natToInteger (Succ (Succ Zero)) -- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- > integerToNat 0
-- Just Zero
-- > integerToNat 1
-- Just (Succ Zero)
-- > integerToNat 2
-- Just (Succ (Succ Zero))
-- > integerToNat (-1) -- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x
  | x < 0     = Nothing
  | otherwise = Just $ integerToNat' x -- complains if I use x > 0

-- | Only to be used in integerToNat,
-- or else need to add more unexpected
-- input handling.
integerToNat' :: Integer -> Nat
integerToNat' x
  | x == 0    = Zero
  | otherwise = Succ $ integerToNat' $ x - 1