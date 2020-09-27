import Data.Monoid (All(All, getAll),  Sum(Sum, getSum), Any(Any, getAny) )

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (Any . (==a))

-- Not sure how this could be implemented with foldMap since we need to make
-- comparisons. Also, these are different from the library's version since
-- they return a Maybe.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = let f :: (Ord a) => a -> Maybe a -> Maybe a
              f cur Nothing = Just cur
              f cur (Just acc) = Just $ min cur acc
          in foldr f Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = let f :: (Ord a) => a -> Maybe a -> Maybe a
              f cur Nothing = Just cur
              f cur (Just acc) = Just $ max cur acc
          in foldr f Nothing

null :: (Foldable t) => t a -> Bool
null = getAll . foldMap (\x -> All False)

length :: (Foldable t) => t a -> Int
length = foldr (\_-> (+1)) 0
-- So with types like Maybe or Either or ((,) a),
-- the function argument of foldr only gets called
-- once.

toList :: (Foldable t) => t a -> [a]
toList = foldMap (:[])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m)
        => (a -> m) -> t a -> m
foldMap' f = foldr g mempty
  where g cur acc = f cur <> acc