lefts' :: [Either a b] -> [a]
-- v1
-- lefts' = map unwrapLeft . filter f
--   where f (Left a) = True
--         f _        = False
--         unwrapLeft (Left a) = a
-- v2
lefts' = foldr f []
  where f (Left a) xs = a : xs
        f _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right b) xs = b : xs
        f _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = go ([], [])
  where go :: ([a], [b]) -> [Either a b] -> ([a], [b])
        go partitions []       = partitions
        go (a, b) (Left x:xs)  = go (x : a, b) xs
        go (a, b) (Right x:xs) = go (a, x : b) xs

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- | This seems to serve to encapsulate the branching
-- logic (as well as the unwrapping the Either),
-- keeping it out of other functions' concerns.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _)  = Nothing 
eitherMaybe'' f x = Just $ either' g f x
  where g = undefined
  -- Passing an undefined function
  -- since it's not used.

lst :: [Either Integer Char]
lst = [Left 1, Right 'a', Left 3, Right 'b']

main = do
  print $ lefts' lst
  print $ rights' lst
  print $ partitionEithers' lst
  print $ eitherMaybe' (+1) $ Left 1  -- Nothing
  print $ eitherMaybe' (+1) $ Right 1  -- Just 2
  print $ either (+1) (subtract 1) $ Left 0  -- 1
  print $ either (+1) (subtract 1) $ Right 0  -- (-1)
  print $ eitherMaybe'' (+1) $ Left 1  -- Nothing
  print $ eitherMaybe'' (+1) $ Right 1  -- Just 2


