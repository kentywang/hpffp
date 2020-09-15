module WordNumberTest where

import Test.Hspec 
import WordNumber (digitToWord, digits, wordNumber)
import Test.QuickCheck
import Data.List (sort)
import Data.Char

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  describe "half" $ do
    it "times 2 is identity" $ do
      property (\n -> halfIdentity n == (n :: Double))
  describe "sorted lists" $ do
      it "are ordered ascending" $ do
        property (\n -> listOrdered (sort (n :: [Int])))

qc :: IO ()
qc = do
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck multAssociative
  quickCheck multCommutative
  quickCheck nonZeroQuotRemProp
  quickCheck nonZeroDivModProp
  -- quickCheck expAssociative
  -- quickCheck expCommutative
  quickCheck reverseId
  quickCheck applicationOpProp
  quickCheck composeProp
  quickCheck concatProp
  -- quickCheck takenLengthProp' 
  -- Fails when taken is more than length of list
  quickCheck readShowProp
  -- quickCheck squareIdentity'
  -- Fails because of precision loss?
  quickCheck idemCaps
  quickCheck idemSort

-- for a function
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
  snd $ foldr go (Nothing, True) xs 
  where go _ status@(_, False) = status 
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

associative :: (Int -> Int -> Int)
            -> Int -> Int -> Int -> Bool
associative f x y z =
  f x (f y z) == f (f x y) z

commutative :: (Int -> Int -> Int)
            -> Int -> Int -> Bool
commutative f x y =
  f x y == f y x

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative = associative (+)

plusCommutative :: Int -> Int -> Bool
plusCommutative = commutative (+)

multAssociative :: Int -> Int -> Int -> Bool
multAssociative = associative (+)

multCommutative :: Int -> Int -> Bool
multCommutative = commutative (*)

quotRemProp :: Int -> Int -> Bool
quotRemProp x y =
  (quot x y)*y + (rem x y) == x

divModProp :: Int -> Int -> Bool
divModProp x y =
  (div x y)*y + (mod x y) == x
  
-- Don't really understand this too well.
nonZeroInt :: Gen Int
nonZeroInt = fmap getNonZero arbitrary

-- Don't really understand this too well.
nonZeroQuotRemProp :: Property
nonZeroQuotRemProp =
  forAll nonZeroInt $ \x ->
  forAll nonZeroInt $ \y ->
    quotRemProp x y

nonZeroDivModProp :: Property
nonZeroDivModProp =
  forAll nonZeroInt $ \x ->
  forAll nonZeroInt $ \y ->
    divModProp x y

expCommutative = commutative (^)
expAssociative = associative (^)

reverseId :: [Int] -> Bool
reverseId x = reverse (reverse x) == id x

-- How do I generate random types?
-- Blind avoids problem with functions
-- not being showable.
applicationOpProp :: Blind (Int -> Int) -> Int -> Bool
applicationOpProp (Blind f) a =
  (f $ a) == f a

composeProp :: Blind (Double -> String)
            -> Blind (Int -> Double)
            -> Int -> Bool
composeProp (Blind f) (Blind g) x =
  (f . g) x == (\y -> f (g y)) x

-- Not sure how we can even compare these
-- two since the left one takes a list
-- and the right one takes two lists.
-- foldr (:) == (++)

concatProp :: [[Int]] -> Bool
concatProp xs = foldr (++) [] xs == concat xs

-- Why can't I use "takenLengthProp n (NonemptyList Int)"
-- to destructure my argument? It says:
-- "Not in scope: data constructor ‘NonEmptyList’"
takenLengthProp :: Int -> [Int] -> Bool
takenLengthProp n xs =
  length (take n xs) == n

nonEmptyIntList :: Gen [Int]
nonEmptyIntList = fmap getNonEmpty arbitrary

nonNegInt :: Gen Int
nonNegInt = fmap getNonNegative arbitrary

takenLengthProp' :: Property
takenLengthProp' =
  forAll nonNegInt $ \n ->
  forAll nonEmptyIntList $ \xs ->
    takenLengthProp n xs

readShowProp :: String -> Bool
readShowProp x = (read (show x)) == x 

nonNegDouble :: Gen Double
nonNegDouble = fmap getNonNegative arbitrary

-- for a function
square x = x * x
-- why does this property not hold?
-- Examine the type of sqrt.
squareIdentity :: Double -> Double
squareIdentity = square . sqrt

squareIdentity' :: Property
squareIdentity' =
  forAll nonNegDouble $ \x ->
  squareIdentity x == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord = fmap toUpper

idemCaps :: String -> Bool
idemCaps x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

idemSort :: String -> Bool
idemSort x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

data Fool = Fulse
          | Frue
          deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' = frequency [(2, return Fulse),
                      (1, return Frue)]

sampleGens = do
  sample genFool'