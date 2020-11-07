module Main where

import Criterion.Main
-- import Data.DList

{--
So difference lists sort of delay concatenation so as to minimize the amount
of concats?

The basic concept is that left-associative cats are expensive:
n ++ n ++ n involves O(5n) time (or maybe O(3n) if we don't need to copy the cdr
when creating a new list)

When optimally, if we reorder the operations to be right-associative, i.e
n ++ (n ++ n), we could do it in O(3n) (or maybe O(2n) if we don't need to
copy the cdr when creating a new list).

Actually, I believe we don't need to copy the cdr. Haskell uses persistent data structures
so it's "perfectly safe to pass references to such data  structures because they ever change."
https://www.quora.com/In-functional-languages-like-Haskell-Elixir-every-function-creates-a-copy-of-its-arguments-whether-its-a-big-list-of-values-or-a-small-string-how-does-that-not-leak-memory
--}

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($ []) . unDL
-- Since unDL returns a function with 1 parameter, it itself is thus essentially
-- a function with 2 parameters. Thanks, pointfree.io. (Modified more based on dlist lib)
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL $ (x:) . unDL xs
-- We only prepend the element after the returned function is applied,
-- so we don't need to duplicate work.
-- `x ++ ys ++ zs` is thus applied in the order of
-- `x ++ (ys ++ zs)`, with `zs` being the argument
-- passed to the returned function.
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x:)
-- This reorders the ops so that `xs ++ y ++ zs`
-- is applied in the order of `xs ++ (y ++ zs)`
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL $ xs . ys
-- `xs ++ ys ++ zs` reorders to
-- `xs ++ (ys ++ zs)`
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs) -- [1] ++ ([2] ++ ([3] ++ []))
        -- This looks pretty efficient actually. I would expect this to be
        -- as quick as DList or even faster.
  -- where go n xs 
  --   | n > i = xs
  --   | otherwise = go (n+1) (xs ++ [n])
  -- For some reason this left-associative version is almost as fast!
  -- Wonder if it was compiler-optimized.

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]

-- I dunno. Either whnf or nf yield no advantage for DLists over the regular
-- ol' appending (even the left-assoc appending). And I checked my implementation
-- with the dlist lib to ensure I did it right. Chalking it up to GHC optimizations?

-- Ah, apparently when I use `stack ghci` to run it, all the tests are way slower.
-- When I use `stack run <executable_name>`, the DList version actually pulls ahead.

-- ...But that's against the `[n] ++ xs` test. And when I switch to `xs ++ [n]`, it
-- actually speeds up!! 8ms to 14ns. That's more than 500,000x faster.
-- I dunno what's going on.