-- 1.
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn a == TisAn b = a == b

-- 2.
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two a b == Two x y =
    a == x && b == y

-- 3.
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt j) = (==) i j
  (==) (TisAString s) (TisAString t) = (==) s t
  (==) (TisAnInt _) (TisAString _) = False
  (==) (TisAString _) (TisAnInt _) = False  -- No symmetry?!

-- 4.
data Pair a = Pair a a

instance Eq t => Eq (Pair t) where
  Pair a b == Pair c d =
    a == c && b == d

-- Why doesn't the following work for Corgi 1 = Corgi 'x'?
-- Error: No instance for (Num Char) arising from the literal ‘1’
-- I think there's some sort of rule I'm breaking using (==) across
-- Num and other typeclasses...
data Corgi a = Corgi a
instance Eq x => Eq (Corgi x) where
  Corgi _ == Corgi _ = True

-- 5.
data Tuple a b = Tuple a b

instance (Eq x, Eq y) => Eq (Tuple x y) where
  Tuple a b == Tuple c d =
    a == c && b == d

-- 6.
data Which a = ThisOne a | ThatOne a

-- Apparently you can't mix and match (==)
-- and (/=) definitions. You need all of one.
instance Eq x => Eq (Which x) where
  ThisOne a /= ThisOne b =
    a /= b
  ThatOne a /= ThatOne b =
    a /= b
  ThisOne _ /= ThatOne _ = True
  ThatOne _ /= ThisOne _ = True

-- 7.
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello b = a == b
  Goodbye a == Goodbye b = a == b
  Hello _ == Goodbye _ = False
  Goodbye _ == Hello _ = False
