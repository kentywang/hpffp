{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (replicateM, join)
import Control.Monad.Trans.State 
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

-- Six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use 'error'
  -- _extremely_ sparingly. 
  x ->
    error $
      "intToDie got non 1-6 integer: " ++ show x

-- Why can we use do syntax here (and in the test function below)?
-- KW: I think the let expression is just sugar for a applied lambda (like
-- in Scheme), so really this do block is just 1 expression, which means it
-- doesn't have to be a monadic expression.
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

test = do
  let x = 1
  2

-- So even though state's type signature is this:
-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- We can still use the result of it as type State?
-- I guess StateT is polymorphic?
rollDie :: State StdGen Die 
rollDie = state $ do
  (n, s) <- randomR (1, 6) 
  return (intToDie n, s)

rollDieM :: Moi StdGen Die 
rollDieM = Moi $ do
  (n, s) <- randomR (1, 6) 
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die) 
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

-- Why doesn't this generate the same number for both rolls?
-- And why does the first of the tuple have the first generated number?
-- Shouldn't it be the second of the tuple?
-- A: Answered when I tried to implement it myself.
test2 = 
  let roll = (,) <$> rollDie <*> rollDie
  in evalState roll $ mkStdGen 1

test2M = 
  let roll = (,) <$> rollDieM <*> rollDieM
  in fst $ runMoi roll $ mkStdGen 1

rollsToGetTwenty :: StdGen -> Int 
rollsToGetTwenty g = go 0 0 g
  where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20 = count 
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= n = count 
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
  go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
  go sum count dice gen
    | sum >= n = (count, dice) 
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) (intToDie die : dice) nextGen

-- State

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Show (Moi s a) where
  show f = "Moi function"

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Moi s a) where
  arbitrary = do
    f <- arbitrary
    return $ Moi f

-- Probably not right. Lifted from EqProp (a -> b) instance:
-- https://hackage.haskell.org/package/checkers-0.4.4/docs/src/Test-QuickCheck-Checkers.html#%3D-%3D
instance (Show s, Arbitrary s, EqProp s, EqProp a) => EqProp (Moi s a) where
  (Moi f) =-= (Moi f') = property (liftA2 (=-=) f f')

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = 
    Moi $ liftA2 (,) (f . fst) snd . g
  -- Using the Applicative instance of functions, not Moi.

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure = Moi . (,)
  -- pure a = Moi $ (,) a

  -- Unlike Reader/function, we don't pass the same input to the 1st arg!
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g = Moi $ \s ->
    let (aToB, s') = f s 
        (a, s'') = g s'
    in (aToB a, s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= g = Moi $ \s ->
    let (a, s') = f s
    in runMoi (g a) s'

main = do
  let trigger :: Moi Int (Int, Int, [Int])
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger 
  -- Above was failing at first, so I tried my own property tests.
  -- Then I found the issue (I was doing "let (a, s') = f s'",
  -- which for some reason wasn't caught by the type checker),
  -- and the tests pass now.
  -- quickCheck $ monadRightIdentityForMoi @Int @Int

functorTest = runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0 -- (1, 0)

applicativeTest = runMoi h 1 -- (4, 3) 
  where h = Moi f <*> Moi g
        f = (,) (+1) . (+2)  -- \s -> ((+ 1), s + 2)
        g = (,) 3 -- \s -> (3, s)
applicativeTestS = runState h 1 -- Should match.
  where h = state f <*> state g
        f = (,) (+1) . (+2)
        g = (,) 3

monadTest = runMoi h 1 -- (6, 4)
  where h :: Moi Int Int
        h = Moi f >>= g
        f = flip (,) 1 . (+2)
        g a = Moi $ \s -> (a + 3, s * 4)

monadTestS = runState h 1
  where h :: State Int Int
        h = state f >>= g
        f = flip (,) 1 . (+2)
        g a = state $ \s -> (a + 3, s * 4)

monadRightIdentityForMoi :: ( Arbitrary s
                            , Arbitrary a
                            , Eq s
                            , Eq a
                            )
                           => s -> a -> Bool
monadRightIdentityForMoi s a =
  let m = pure a
  in runMoi (m >>= return) s == runMoi m s

-- Chapter exercises

get :: Moi s s
get = Moi $ join (,) -- Thanks, pointfree.io. Better than \s -> (s, s)
                     -- or liftA2 (,) id id.

t1 = runMoi Main.get "curryIsAmaze" -- ("curryIsAmaze", "curryIsAmaze")

put :: s -> Moi s ()
-- put s = Moi $ \_ -> ((), s)
put = Moi . const . (,) ()

t2 = runMoi (Main.put "blah") "woot" -- ((),"blah")

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

t3 = exec (Main.put "wilma") "daphne"
-- "wilma"

t4 = exec Main.get "scooby papu"
-- "scooby papu"

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

t5 = eval Main.get "bunnicula"
-- "bunnicula"

t6 = eval Main.get "stake a bunny"
-- "stake a bunny"

modify :: (s -> s) -> Moi s ()
-- modify f = Moi $ (,) () . f
modify = Moi . ((,) () .)

t7 = runMoi (Main.modify (+1)) 0
-- ((),1)

t8 = runMoi (Main.modify (+1) >> Main.modify (+1)) 0
-- ((),2)