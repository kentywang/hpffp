data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x =
  case (f x) of
    Nothing        -> Leaf
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n
  | n < 0    = Leaf
  | otherwise = unfold f 0
  where f :: Integer -> Maybe (Integer, Integer, Integer)
        f m
          | m == n = Nothing
          | otherwise = Just (m+1, m, m+1)

main :: IO ()
main = do
  putStr "treeBuild 0:\n\t"
  print $ treeBuild 0
  putStr "treeBuild 1:\n\t"
  print $ treeBuild 1
  putStr "treeBuild 2:\n\t"
  print $ treeBuild 2