-- 1.
-- go 15 2 0
-- go 13 2 1
-- ...
-- go 1 2 7
-- (7, 1)

sumToN :: (Eq a, Num a) => a -> a
sumToN 0 = 0
sumToN n = n + sumToN (n - 1)

mult :: (Integral a) => a -> a -> a
mult x y = go x y
  where go sum count
          | count == 0 = 0
          | count == 1 = sum
          | otherwise  =
              go (sum + x) (count - 1)

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
