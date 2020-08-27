--- ahc.hs
module AHeadCode where

-- 1.
u = x * 3 + y
  where x = 3
        y = 1000

-- Seems like it breaks if variables don't line up in a where.

-- 2.
v = x * 5
  where  y = 10
         x = 10 * 5 + y

-- 3.
w = z / x + y
  where x = 7
        y = negate x
        z = y * 10

    