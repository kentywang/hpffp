-- 1.
-- go 15 2 0
-- go 13 2 1
-- ...
-- go 1 2 7
-- (7, 1)

sumToN :: (Eq a, Num a) => a -> a
sumToN 0 = 0
sumToN n = n + sumToN (n - 1)

mult :: (Eq a, Num a, Ord a) => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult x y = go 0 y
  where go sm ct
          | ct == 0 = sm
          | ct < 0 =
              go (sm - x) (ct + 1)
          | otherwise =
              go (sm + x) (ct - 1)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)

data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

myDiv :: Integral a
      => a
      -> a
      -> DividedResult
myDiv num denom = go (abs num) 0
  where 
    sign1 = if num > 0 then 1 else -1
    sign2 = if denom > 0 then 1 else -1
    dAbs = abs denom
    go nAbs count
      | dAbs == 0 = DividedByZero
      | nAbs < dAbs = Result (count * sign1 * sign2)
      | otherwise =
          go (nAbs - dAbs) (count + 1)
