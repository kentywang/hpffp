-- Correcting syntax
-- 1.
x = (+)

f xs = w `x` 1
  where w = length xs

-- 2.
-- But you didn't even introduce anonymous functions yet, guys...
myId = \x -> x

-- 3.
g (a, b) = a