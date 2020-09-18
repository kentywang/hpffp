module Main where

import Test.QuickCheck ( quickCheck
                       , arbitrary
                       , Arbitrary
                       , CoArbitrary
                       , elements
                       , frequency )

-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where 
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where 
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) 
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (a <> mempty) == a

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

type IdAssoc =
  Identity String ->  -- String has Semigroup instance.
  Identity String ->
  Identity String ->
  Bool

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty  -- Haskell, man.

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

-- 3

data Two a b = Two a b deriving (Eq, Show)

-- Initially as just a semigroup, the behavior was:
-- Leftmost a is preserved, while b is its mappend result.
-- But that first condition doesn't work as a monoid
-- since left identity property would not be satisfied.
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two u v = Two (x <> u) (y <> v)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

type TwoAssoc =
  Two [Int] String ->
  Two [Int] String ->
  Two [Int] String ->
  Bool

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup b) => Semigroup (Three a b c) where
  Three x y _ <> Three _ v z = Three x (y <> v) z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc =
  Three Int String Int ->
  Three Int String Int ->
  Three Int String Int ->
  Bool

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

-- Just returns the leftmost.
instance Semigroup (Four a b c d) where
  Four a b c d <> Four _ _ _ _ = Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc =
  Four Int Char Bool Int ->
  Four Int Char Bool Int ->
  Four Int Char Bool Int ->
  Bool

-- 6

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = arbitrary >>= return . BoolConj

type BoolConjAssoc = BoolConj -> BoolConj
                  -> BoolConj -> Bool

-- 7

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = arbitrary >>= return . BoolDisj

type BoolDisjAssoc = BoolDisj -> BoolDisj
                  -> BoolDisj -> Bool

-- 8

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

-- Rightmost Fst is preserved, while leftmost Snd is
-- preserved. Prefers Snd.
instance Semigroup (Or a b) where
  Snd b <> _ = Snd b
  Fst a <> Snd b = Snd b
  Fst a <> Fst a' = Fst a'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc =
  Or Int String ->
  Or Int String ->
  Or Int String ->
  Bool

-- 9

newtype Combine a b =
  Combine { unCombine :: (a -> b) }
   
instance Show (Combine a b) where
  show f = "Combine function"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g =
    Combine (\x -> f x <> g x)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)

-- Don't really understand how this works.
instance (CoArbitrary a, Arbitrary b) =>
         Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- Can't test for function equality, so
-- just resort to functon output equality
-- as next best option.
-- I guess I need to specify "Semigroup n"
-- in the type sig here because technically
-- there's no restriction that the second
-- type of Combine has to have a Semigroup
-- instance.
combFuncAssoc :: (Semigroup n, Eq n) => Combine m n
              -> Combine m n -> Combine m n
              -> m -> Bool
combFuncAssoc f g h x =
  unCombine (f <> g <> h) x == unCombine ((f <> g) <> h) x

-- Similar idea for identity.
combFuncIdLeft :: (Monoid n, Eq n) => Combine m n -> m -> Bool
combFuncIdLeft f x =
  unCombine (mempty <> f) x == unCombine f x

combFuncIdRight :: (Monoid n, Eq n) => Combine m n -> m -> Bool
combFuncIdRight f x =
  unCombine (f <> mempty) x == unCombine f x

type CombAssoc =
  Combine Int String ->
  Combine Int String ->
  Combine Int String ->
  Int -> Bool

-- 10

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show f = "Comp function"

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

compFuncAssoc :: (Eq a) => Comp a
              -> Comp a -> Comp a
              -> a -> Bool
compFuncAssoc f g h x =
  unComp (f <> g <> h) x == unComp ((f <> g) <> h) x

compFuncIdLeft :: Eq n => Comp n -> n -> Bool
compFuncIdLeft f x =
  unComp (mempty <> f) x == unComp f x

compFuncIdRight :: Eq n => Comp n -> n -> Bool
compFuncIdRight f x =
  unComp (f <> mempty) x == unComp f x

-- Don't really understand how this works.
instance (CoArbitrary a, Arbitrary a) =>
         Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type CompAssoc =
  Comp Char ->
  Comp Char ->
  Comp Char ->
  Char ->
  Bool

-- 11

data Validation a b = Failure a | Success b deriving (Eq, Show)

-- Mappend Failures & get leftmost Success.
instance Semigroup a => Semigroup (Validation a b) where
  Success b <> _ = Success b
  Failure a <> Success b = Success b
  Failure a <> Failure a' = Failure (a <> a')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Failure a))
              , (1, return (Success b)) ]

type ValidAssoc =
     Validation String Int
  -> Validation String Int
  -> Validation String Int
  -> Bool

-- 8m

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Show (Mem s a) where
  show f = "Mem function"

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

-- First element mappends, second element composes. What's the big deal?
instance Semigroup a => Semigroup (Mem s a) where 
  Mem f <> Mem g = Mem h
    where h x = let y = f x
                    z = g x in
                  (fst y <> fst z, snd (f (snd z)))

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem $ \x -> (mempty, x)

memFuncAssoc :: (Semigroup a, Eq s, Eq a) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
memFuncAssoc f g h x =
  runMem (f <> g <> h) x == runMem (f <> (g <> h)) x

memFuncIdLeft :: (Monoid a, Eq s, Eq a) => Mem s a -> s -> Bool
memFuncIdLeft f x =
  runMem (mempty <> f) x == runMem f x

memFuncIdRight :: (Monoid a, Eq s, Eq a) => Mem s a -> s -> Bool
memFuncIdRight f x =
  runMem (f <> mempty) x == runMem f x

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (combFuncAssoc :: CombAssoc)
  quickCheck (compFuncAssoc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Two [Int] String -> Bool)
  quickCheck (monoidRightIdentity :: Two [Int] String -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (combFuncIdLeft :: Combine Int String -> Int -> Bool)
  quickCheck (combFuncIdRight :: Combine Int String -> Int -> Bool)
  quickCheck (compFuncIdLeft :: Comp Char -> Char -> Bool)
  quickCheck (compFuncIdRight :: Comp Char -> Char -> Bool)
  quickCheck (memFuncAssoc :: Mem Int String -> Mem Int String -> Mem Int String -> Int -> Bool)
  quickCheck (memFuncIdLeft :: Mem Int String -> Int -> Bool)
  quickCheck (memFuncIdRight :: Mem Int String -> Int -> Bool)

elevenTest :: IO ()
elevenTest = do
  let failure :: String
              -> Validation String Int
      failure = Failure
      success :: Int
              -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

f' = Mem $ \s -> ("hi", s + 1)

memTest = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0