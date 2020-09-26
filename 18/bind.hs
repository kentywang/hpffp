-- bind.hs

import Control.Monad (join)

bind :: Monad m 
     => (a -> m b)
     -> m a
     -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs =
  xs >>=
  \x ->
  if even x
  then [x*x, x*x]
  else []

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer 
g i=
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)