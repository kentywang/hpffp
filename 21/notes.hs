-- Investigating why
-- (fmap . fmap) sum Just [1, 2, 3] 
-- works.

-- Not sure this clarifies anything for me...
class Functor f => TestClass f where
  -- foo :: (Functor g) => f a -> f (g a)
  bar :: f a -> f (Maybe a)

instance TestClass ((->) p) where
  -- foo = undefined
  bar = (Just .)

y :: Integer -> Maybe Int
y = bar (\n -> 2)


k :: ((->) a (Maybe a)) 
--    ^^^^^^  ^^^^^ ^
--   (  f    (  g   a)) where f and g are Functors.
--   (  
k = Just

{- 
(fmap . fmap) sum :: (Functor f, Functor g, Foldable t, Num b) 
                  => f (g (t b))     [1]
                  -> f (g b)         [2]

[1]: We know

      ((->) a (Maybe a))          fits 
      (  f    (  g   a))          , so

      ((->) (t b) (Maybe (t b)))  fits
      (    f      (  g   (t b)))

[2]: We've now filled in the f and g constraints with types
     ((->) (t b)) and Maybe, respectively.

      (    f      (  g   b))      is thus now
      ((->) (t b) (Maybe b))      , which is in standard arrow terms

      (t b) -> g b
      So the consequence of the making definite the types
      f and g has made this function result become a
      function itself! Thus, evaluating

      (fmap . fmap) sum Just            returns a new function, so
      (fmap . fmap) sum Just [1, 2, 3]  is simply calling that returned function.
-}
