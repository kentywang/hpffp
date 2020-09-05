tensDigit :: Integral a => a -> a
tensDigit x = d
  where (d,_) = divMod x 10

hunsD :: Integral a => a -> a
hunsD = fst . (`divMod` 100)

-- What should this function even do?
foldBool :: a -> a -> Bool -> a
-- foldBool x _ False = x
-- foldBool _ y True = y
foldBool x y b
  | b == False = x
  | b == True  = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
