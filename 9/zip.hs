-- zip.hs
myZip :: [a] -> [b] -> [] ((,) a b)
myZip _ [] = []  -- If I used a guard for this, e.g. | b == [] = [], then would need Eq.
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

newZip = myZipWith (,)