-- I think I remember this from SICP...

-- Values            Order
--   2:3:5:8           3:4:5:6  Newer (built from previous 2 values)
--  /|/|/|/|          /|/|/|/|
-- 1:1:2:3:5         1:2:3:4:5  Older

-- It looks like within scanl the passed function
-- can access the previous value in the stream (the one directly
-- below it in my diagram, but to access more than that (such as the
-- one before), it needs to be scan over itself (the stream offset by one).
fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

-- filter (<100) fibs
-- doesn't work like I expect since it'll try
-- to make an infinite list instead of stopping
-- after a certain point.
fibsLess100 = 1 : scanl' (+) 1 fibsLess100
  where scanl' f q (x:xs)
          | q > 100 = []
          | otherwise = q : scanl' f (f q x) xs

-- While we always want the previous value in the next factorial
-- number, we don't need anything else from the previous values of
-- the factorial stream. Instead, we want to scan over [1,2,3,...].
factorial = scanl (*) 1 [1..]