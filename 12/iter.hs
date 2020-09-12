import Data.List ( unfoldr )

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case (f x) of
    Just (m, n) -> m : myUnfoldr f n
    otherwise   -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr $ g f
  where g :: (b -> b) -> (b -> Maybe (b, b))
        g h x = (\y -> Just (x, y)) $ h x
        -- I was able to "add" another argument!
        -- Look at the type signature closely. 

main = do
  print $ take 10 $ iterate (+1) 0
  print $ take 10 $ myIterate (+1) 0
  print $ (==) (take 10 (unfoldr (\b -> Just (b, b+1)) 0))
               (take 10 (myUnfoldr (\b -> Just (b, b+1)) 0))
  print $ (==) (unfoldr (\_ -> Nothing :: Maybe (Char, Int)) 0)
               (myUnfoldr (\_ -> Nothing :: Maybe (Char, Int)) 0)
  print $ take 10 $ betterIterate (+1) 0