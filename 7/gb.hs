--3.a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

-- 3.b
addFive = \x -> \y -> (if x > y then y else x) + 5

-- 3.c
mflip f x y = f y x
