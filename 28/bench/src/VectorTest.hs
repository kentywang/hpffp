module Main where

import Criterion.Main
import qualified Data.Vector as V

-- slice :: Int -> Int -> [a] -> [a]
-- slice from len xs =
--   take len (drop from xs)

-- l :: [Int]
-- l = [1..1000]

-- v :: V.Vector Int
-- v = V.fromList [1..1000]

-- main :: IO ()
-- main = defaultMain
--   [ bench "slicing list" $
--     whnf (head . slice 100 900) l
--   , bench "slicing vector" $
--     whnf (V.head . V.slice 100 900) v
--   ]

-- testV :: Int -> V.Vector Int
-- testV n =
--   V.map ((+ n) . (+ n) . (+ n) . (+ n)) (V.fromList [1 .. 10000])

-- testV' :: Int -> V.Vector Int
-- testV' n =
--   V.map (+ n) $ V.map (+ n) $ V.map (+ n) $ V.map (+ n) (V.fromList [1 .. 10000])

-- main :: IO ()
-- main =
--   defaultMain
--     [ bench "vector map prefused" $ whnf testV 9998,
--       bench "vector map will be fused" $ whnf testV' 9998
--     ]
--     -- 918.5 μs vs 2.920ms... I don't think the loop fusion happened?

-- vec :: V.Vector Int
-- vec = V.fromList [1..10000]

-- slow :: Int -> V.Vector Int 
-- slow n = go n vec
--   where go 0 v = v
--         go n v = go (n-1) (v V.// [(n, 0)])

-- batchList :: Int -> V.Vector Int 
-- batchList n = vec V.// updates
--   where updates =
--           fmap (\n -> (n, 0)) [0..n]

-- main :: IO ()
-- main = defaultMain
--   [ bench "slow" $ whnf slow 9998 
--   , bench "batch list" $
--     whnf batchList 9998
--   ]

import qualified Data.Vector.Unboxed as UV

vec :: V.Vector Int
vec = V.fromList [1..100000]

unboxedVec :: UV.Vector Int
unboxedVec = UV.fromList [1..100000]

batchList :: Int -> V.Vector Int 
batchList n = vec V.// updates
  where updates =
          fmap (\n -> (n, 0)) [0..n]

batchList' :: Int -> UV.Vector Int 
batchList' n = unboxedVec UV.// updates
  where updates =
          fmap (\n -> (n, 0)) [0..n]

main :: IO ()
main = defaultMain
  [ bench "batch list" $
    whnf batchList 9998
  , bench "batch list unboxed" $
    whnf batchList' 9998
  ]