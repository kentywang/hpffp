a :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]
-- [2]

b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- Just ["Hi,lol","Hellolol"]

c :: Num a => a -> a 
c = (*2) . (\x -> x - 2)
-- c 1 should be -2

-- Removed unneed parens since function composition is 
-- associative.
d :: (Enum a, Num a, Show a) => a -> [Char]
d = (return '1' ++) . show . (\x -> [x, 1..3])
-- d 0 returns "1[0,1,2,3]"

e :: IO Integer
e = let ioi :: IO Integer
        ioi = readIO "1" :: IO Integer
        changed :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi 
    in fmap (*3) changed
-- 3693