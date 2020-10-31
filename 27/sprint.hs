import Debug.Trace

let x = 1 -- x = _
let x = ['1'] -- x = "1" because '1' is Char (not polymorphic), so GHC knows its constant and thus and evaluates (always to WHNF)?
let x = [1] -- x = _
let x = 1 :: Int -- x = 1
let f = \x -> x -- f = _
let x = f 1 -- x = _ because poly, f = _ because named argument?
let f :: Int -> Int; f = \x -> x -- f = _
let x = f 1 -- x = _ (remember, f 1 hasn't been eval'd yet), f = _

-- Mine
g :: a -> Int
g = const (trace "woo" 2) -- g = _
g 1 -- woo, g = _
g 1 -- woo again since g isn't saved, g = _

g :: Int -> Int
g = const (trace "woo" 2) -- g = _
g 1 -- woo, g = _
g 1 -- no woo since g is saved?, g = _ 

I'm surprised at this result. g is evaluated only once, yet :sprint still shows g = _?

1. snd (undefined, 1)
-- 1
2. let x = undefined
   let y = x `seq` 1 in snd (x, y)
-- _
3. length $ [1..5] ++ undefined
-- _
4. length $ [1..5] ++ [undefined]
-- 6
5. const 1 undefined
-- 1
6. const 1 (undefined `seq` 1)
-- 1
7. const undefined 1
-- _

-- Making it bottom out:
x = undefined
y = "blah"
main = do
  seq x $ print (snd (x, y))