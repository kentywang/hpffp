-- module CoreDump where

-- discriminatory :: Bool -> Int 
-- discriminatory b =
--   case b of False -> 0 
--             True -> 1

const 1 undefined
(\a _ -> a) 1 undefined
(\_ -> 1) undefined
1

const undefined 1
(\a _ -> a) undefined 1
(\_ -> undefined) 1
undefined

flip const undefined 1
(\f a b -> f b a) (\a _ -> a) undefined 1
(\a b -> (\c _ -> c) b a) undefined 1
(\b -> (\c _ -> c) b undefined) 1
(\c _ -> c) 1 undefined
(\_ -> 1) undefined
1

flip const 1 undefined
(\f a b -> f b a) (\a _ -> a) 1 undefined
(\a b -> (\c _ -> c) b a) 1 undefined
(\b -> (\c _ -> c) b 1) undefined
(\c _ -> c) undefined 1
(\_ -> undefined) 1
undefined

const undefined undefined
(\a _ -> a) undefined undefined
(\_ -> undefined) undefined
undefined

foldr const 'z' ['a'..'e']
(\f z (x:xs) -> f x (foldr f z xs)) (\a _ -> a) 'z' ['a'..'e']
(\z (x:xs) -> (\a _ -> a) x (foldr (\a _ -> a) z xs)) 'z' ['a'..'e']
(\(x:xs) -> (\a _ -> a) x (foldr (\a _ -> a) 'z' xs)) ['a'..'e']
(\a _ -> a) 'a' (foldr (\a _ -> a) 'z' ['b'..'e'])
(\_ -> 'a') (foldr (\a _ -> a) 'z' ['b'..'e'])
'a'

-- Using ['a'] for simplicity, but can be extended to "abcde"
foldr (flip const) 'z' "a"
(\f z (x:xs) -> f x (foldr f z xs)) ((\f a b -> f b a) (\a _ -> a)) 'z' "a"
(\z (x:xs) -> ((\f a b -> f b a) (\a _ -> a)) x (foldr ((\f a b -> f b a) (\a _ -> a)) z xs)) 'z' "a"
(\(x:xs) -> ((\f a b -> f b a) (\a _ -> a)) x (foldr ((\f a b -> f b a) (\a _ -> a)) 'z' xs)) "a"
((\f a b -> f b a) (\a _ -> a)) 'a' (foldr ((\f a b -> f b a) (\a _ -> a)) 'z' ())
(\a b -> (\c _ -> c) b a) 'a' (foldr ((\f a b -> f b a) (\a _ -> a)) 'z' ())
(\b -> (\c _ -> c) b 'a') (foldr ((\f a b -> f b a) (\a _ -> a)) 'z' ())
(\c _ -> c) (foldr ((\f a b -> f b a) (\a _ -> a)) 'z' ()) 'a'
(\_ -> foldr ((\f a b -> f b a) (\a _ -> a)) 'z' ()) 'a'
foldr ((\f a b -> f b a) (\a _ -> a)) 'z' ()
(\f z (x:xs) -> f x (foldr f z xs)) ((\f a b -> f b a) (\a _ -> a)) 'z' ()
(\z (x:xs) -> ((\f a b -> f b a) (\a _ -> a)) x (foldr ((\f a b -> f b a) (\a _ -> a)) z xs)) 'z' ()
(\(x:xs) -> ((\f a b -> f b a) (\a _ -> a)) x (foldr ((\f a b -> f b a) (\a _ -> a)) 'z' xs)) ()
-- Switching to other pattern match since () doesn't fit (x:xs)
(\xs -> 'z') ()
'z'