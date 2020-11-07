module Main where

import Criterion.Main
import Control.Applicative
import Data.Sequence

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push a (Queue en de) = Queue (a : en) de

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue en []) = pop $ Queue [] $ Prelude.reverse en -- This makes it O(n)!
pop (Queue en (x:xs)) = Just (x, Queue en xs)

pushL :: a -> [a] -> [a]
pushL a xs = a : xs

-- O(n)
popL :: [t] -> Maybe (t, [t])
popL [] = Nothing
popL (x:xs) = Just $ go x xs
  where go :: t -> [t] -> (t, [t])
        go y [] = (y, []) -- Inspired by `init` function
        go y (z:zs) =
          liftA2 (,)
                 fst
                 ((y :) . snd)
          $ go z zs
-- This is dramatically better than my left-associative appending approach (O(n^2))

data QueueOp = Push | Pop

runQOpsL :: [QueueOp] -> [Int] -> [Int]
runQOpsL [] lst = lst
runQOpsL (op:ops) lst =
  let f = case op of
            Push -> pushL 1
            Pop -> \lst' -> maybe lst' snd (popL lst')
  in runQOpsL ops $ f lst

runQOps :: [QueueOp] -> Queue Int -> Queue Int
runQOps [] q = q
runQOps (op:ops) q =
  let f = case op of
            Push -> push 1
            Pop -> \q' -> maybe q' snd (pop q')
  in runQOps ops $ f q

runQOpsS :: [QueueOp] -> Seq Int -> Seq Int
runQOpsS [] s = s
runQOpsS (op:ops) s =
  let f = case op of
            Push -> (1 <|)
            Pop -> \s' -> deleteAt (Data.Sequence.length s' - 1) s'
  in runQOpsS ops $ f s

ops :: [QueueOp]
ops = mconcat $ Prelude.replicate 10000 [Push, Pop]

al :: [Int]
al = []

aq :: Queue Int
aq = Queue [] []

bl :: [Int]
bl = Prelude.replicate 1000 1

bq :: Queue Int
bq = Queue bl []

bs :: Seq Int
bs = fromList bl

main :: IO ()
main = defaultMain
  [ bench "pupo list" $ whnf (runQOpsL ops) bl -- Don't think I need nf since Maybe contents are
  , bench "pupo queue" $ whnf (runQOps ops) bq -- forced during runQOps.
  , bench "pupo seq" $ whnf (runQOpsS ops) bs
  ]

-- Compiled 
-- 1.699 ms
-- 284.0 μs (would be faster/slower with fewer/more queue migrations)
-- 717.7 μs

-- Moral of the story: Okasaki's queue essentially caches the
-- traversal.

-- Seq should have O(log n) pops?
-- List should have O(n) pops
-- Okasaki queue should have O(1) pops (when no migration from enqueue to dequeue list needed)