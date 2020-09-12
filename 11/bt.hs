data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  concat [[a], preorder left, preorder right]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  concat [preorder left, [a], preorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  concat [preorder left, preorder right, [a]]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

-- any traversal order is fine
foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree f z = foldr f z . inorder

testFoldTree = foldTree (-) 0 testTree == 2