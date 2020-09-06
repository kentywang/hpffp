eftBool :: Bool -> Bool -> [Bool]
-- eftBool False True = [False, True]
-- eftBool False False = [False]
-- eftBool True True = [True]
-- eftBool True False = []
eftBool = abstraction

eftOrd :: Ordering -> Ordering -> [Ordering]
-- eftOrd LT GT = [LT, EQ, GT]
-- eftOrd LT EQ = [LT, EQ]
-- eftOrd LT _ = [LT]
-- eftOrd EQ GT = [EQ, GT]
-- eftOrd EQ EQ = [EQ]
-- eftOrd EQ _ = []
-- eftOrd GT GT = [GT]
-- eftOrd GT _ = []
eftOrd = abstraction

eftInt :: Int -> Int -> [Int]
eftInt = abstraction

eftChar :: Char -> Char -> [Char]
eftChar = abstraction

abstraction :: (Ord a, Enum a) => a -> a -> [a]
abstraction x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : abstraction (succ x) y