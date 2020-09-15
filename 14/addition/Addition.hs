module Addition where
  
import Test.Hspec
import Test.QuickCheck

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "3 times 5 is 15" $ do
      mult 3 5 `shouldBe` 15
    it "22 times 0 is 0" $ do
      mult 22 0 `shouldBe` 0
    it "1 times -2 is -2" $ do
      mult 1 (-2) `shouldBe` -2
    it "-3 times -5 is 15" $ do
      mult (-3) (-5) `shouldBe` 15
    it "x + 1 is always\
       \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

mult :: (Eq a, Num a, Ord a) => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult x y = go 0 y
  where go sm ct
          | ct == 0 = sm
          | ct < 0 =
              go (sm - x) (ct + 1)
          | otherwise =
              go (sm + x) (ct - 1)

-- choose :: System.Random.Random a
--        => (a, a) -> Gen a
-- elements :: [a] -> Gen a
genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]