myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem = myAny . (==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

-- Trying to use tail call recursion to not build stack.
-- I initially thought it would mean more time,
-- such as O(1 + 2 + ... + n) = O(n (n + 1) / 2) = O(n^2)
-- instead of O(n), but that depends on if the underlying
-- concat that I was using works by concatting
-- only once (O(n)) or by ++ing each element
-- to the next. Since concat is just foldr (++) [], it's
-- just O(n^2), meaning my tail call recursive approach
-- should no worse than it in time and space performance.
-- The space use on stack is O(1), while heap is O(n) (since should
-- grow linearly with number of lists).
squish :: [[a]] -> [a]
squish [] = []
-- squish (x:xs) = x ++ squish xs
squish (x:xs) = go x xs
  where go acc [] = acc
        go acc (curr:next) = go (acc ++ curr) next

-- First approach interleaves function application and concatenation,
-- second approach concatenates at the end.
squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap _ [] = []
-- squishMap f (x:xs) = go (f x) xs
--   where go acc [] = acc
--         go acc (curr:next) = go (f curr ++ acc) next
squishMap f = squish . map f  -- Variable name matches body. Neat.

squishAgain :: [[a]] -> [a]
squishAgain = squish

-- Don't need Ord instance on 'a' here since 1st argument will supply
-- a means of ordering them, meaning myMaximumBy won't be comparing
-- them directly.
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy = extremaBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = extremaBy LT

extremaBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
extremaBy _ _ [] = error "empty list"  -- matching maximumBy's behavior 
extremaBy ordering f (x:xs) = go x xs
  where go currExtrema [] = currExtrema
        go currExtrema (y:ys) = go (newExtrema currExtrema y) ys
        newExtrema a b = if f b a == ordering then b else a

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare