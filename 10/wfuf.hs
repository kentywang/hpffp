myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f = foldr (\x y -> if f x then True else y)
--                 False
-- myAny f = foldr (\x y -> f x || y)
--                 False
-- myAny = flip foldr False . (\f x y -> f x || y)
-- myAny = flip foldr False . \f x -> (f x ||)
-- Not my work.
myAny = flip foldr False . ((||) .)  -- Key takeaway: ending with a point prepares
--                       ^              expression for function as argument.
-- This point lets next argument to the
-- entire expression be for setting up
-- the final argument for foldr. Without
-- it, the final argument (the expression)
-- to the right of the dot would already
-- be "loaded".

myElem :: Eq a => a -> [a] -> Bool
-- myElem a = any (\x -> x == a)
-- myElem a = any (== a)
myElem = any . (==)

myElem2 :: Eq a => a -> [a] -> Bool
-- myElem2 a = foldr (\x y -> x == a || y) False
-- myElem2 a = foldr (\x -> ((x == a) ||) False
-- Not my work. A little too crazy.
-- myElem2 a = foldr ((||) . (a ==)) False
myElem2 = flip foldr False . ((||) .) . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\curr -> (f curr :)) []
-- Not my work.
myMap = flip foldr [] . ((:) .)

myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter f = foldr (\curr acc -> if f curr then curr : acc else acc) []
myFilter f = foldr (\curr -> if f curr then (curr :) else id) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f = foldr (\curr -> (f curr ++)) []
-- Woohoo! Got this one myself.
squishMap = flip foldr [] . ((++) .)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id  -- Is this what they call lowering?

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- Dunno how to do this with foldr, so using a variant that has no start value.
myMaximumBy f = foldr1 (\curr max -> if f curr max == GT then curr else max)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\curr max -> if f curr max == LT then curr else max)
