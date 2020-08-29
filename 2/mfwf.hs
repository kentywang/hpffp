-- mfwf.hs
module MFWF where

z = 7
-- x= y ^ 2 -- 225
waxOn = x * 5
  where x = y ^ 2 -- 1125
y= z + 8 -- 15

triple x = x * 3

waxOff x = triple x ^ 2 -- function application is prioritized