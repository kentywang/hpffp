{-# LANGUAGE FlexibleInstances #-}

-- 1

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- 1

data Quant a b = 
    Finance
  | Desk a
  | Bloor b deriving Show

instance Functor (Quant e) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a

qtest :: Quant a Integer
qtest = fmap (+1) (Bloor 2)

-- 2

data K a b = K a deriving Show

instance Functor (K a) where
  fmap _ (K x) = K x
  -- Why can't I do fmap _ x = x here?

-- 3

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a

-- should remind you of an
-- instance you've written before
instance Functor (Flip K' a) where
  fmap f (Flip (K' b)) = Flip $ K' (f b)
  -- Why can't I just do:
  -- fmap f (Flip (K' b)) = Flip (fmap f (K' b))
  -- Sure, it won't have the same behavior,
  -- but shouldn't it pass the typechecker?

-- 4

data EvilGoateeConst a b = GoatyConst b deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

etest = fmap (+1) $ GoatyConst 2

-- 5

data LiftItOut f a = LiftItOut (f a) deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

ltest = fmap (+1) $ LiftItOut $ Right 3

-- 6

data Parappa f g a = DaWrappa (f a) (g a) deriving Show

instance (Functor p, Functor q) => Functor (Parappa p q) where
  fmap f (DaWrappa g h) = DaWrappa (fmap f g) (fmap f h)
  -- Note that writing out (DaWrappa (g x) (h x)) doesn't
  -- work on the term level. I think it's because the (g x)
  -- and (h x) are already applied.

-- 7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething p q) = IgnoringSomething p $ fmap f q

-- 8

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b $ fmap f c

-- 9

data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) $ fmap f b

listTest = fmap (+1) $ Cons 1 $ Cons 2 $ Cons 3 Nil

-- 10

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving Show

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x)
                                       (fmap f y) 
                                       (fmap f z)

goatTest = fmap (+1) (MoreGoats (OneGoat 1)
                                (OneGoat 2) (MoreGoats (NoGoat)
                                                       (OneGoat 3)
                                                       (OneGoat 4)))

-- 11

instance Show (String -> a) where
  show f = "Trust me."

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
                deriving Show

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str $ f a
  fmap f (Read g) = Read $ f <$> g
  -- Also works with just Read $ fmap f g

talkTest = fmap show (Read g)
  where g :: String -> Integer
        g "Hello" = 83770
        g _       = 322
        h f = f "Hello"

-- Not sure how to test.
