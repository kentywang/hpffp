module Ch4Exercises where

-- 8.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs n = if (n > 0) then n else -n

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (x, y) (u, v) = ((y, v), (x, u))
