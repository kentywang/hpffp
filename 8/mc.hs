mc91 :: (Ord a, Num a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise =  -- 91 also works
    mc91 . mc91 $ x + 11

main :: IO ()
main = do
  print $ map mc91 [95..110]