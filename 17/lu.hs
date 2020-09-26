import Control.Applicative
import Data.List (elemIndex)

test :: IO [Char]
test = liftA2 (++) getLine getLine
-- Identical to (++) <$> getLine <*> getLine

test2 :: IO Int
test2 = fmap length test
-- Identical to pure length <*> test

added :: Maybe Integer
added =
  pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
  -- (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer) 
tupled = fmap (,) y <*> z
-- liftA2 (,) y z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x y'
-- max' <$> x <*> y'

xs = [1, 2, 3]
ys = [4, 5, 6]
x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''
-- sum <$> (liftA2 (,) x' y'')
