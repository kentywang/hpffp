{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

newtype Identity a =
  Identity { runIdentity :: a }

newtype Compose f g a =
  Compose { getCompose :: f (g a) } 
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
      => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = 
    Compose $ (<*>) <$> f <*> a

-- f :: f (g (a -> b))
-- a :: f (g a)

-- Let's look at some type signature options for <*>:

-- (<*>) :: f (a -> b) -> f a -> f b
-- With this interpresation, it seems there's no use,
-- since in our case f never wraps a function, but another
-- structure around a function.

-- (<*>) :: g (a -> b) -> g a -> g b
-- This could be useful within f.

-- (<*>) :: f (g a -> g b) -> f (g a) -> f (g b)
-- This looks like all we need!
-- If there's a way to get the first arg, then we're done,
-- since the 2nd arg is just a.

-- But how do we get that 1st arg?

-- It looks awfully similar to `f g (a -> b)`, our f.
-- Maybe there's a function (call it u) that can
-- transform f to it?

-- u :: f g (a -> b) -> f (g a -> g b)
-- So u just needs to be mapped over f with a function
-- (call it v) that has this type sig:

-- v :: g (a -> b) -> (g a -> g b)
-- But that's just <*> using the Applicative instance of g!

-- So as expected, we used both the <*> for f (in the form of
-- f (g a -> g b) -> f (g a) -> f (g b)) and for g (in the form of
-- g (a -> b) -> g a -> g b).

-- Putting that all together:
-- u f <*> a

--   where u :: f g (a -> b) -> f (g a -> g b)
--         u = \f -> fmap v f

--         v :: g (a -> b) -> (g a -> g b)
--         v = \gAToB -> gAToB <*> -- Or just <*>

-- Struggled for a few hours on this, but I'm so proud I figured
-- it out! The hardest part was realzing one possible instantation
-- of <*> was `f (g a -> g b) -> f (g a) -> f (g b)`. Once that piece
-- was noticed, the rest eventually came out!

-- Some of my previous (dead-end) brainstorming notes:

-- fmap :: (g a -> g b) -> f (g a) -> f (g b)
-- fmap (\ga -> unwrapF f <*> ga) a -- Dead-end because that unwrapF function proved elusive.

-- (x <*> y) <*> z
-- x <*> (y <*> z)

-- wrapF $ unwrapF f <*> unwrapF a -- wrapF is just `pure`
-- -- This was the closest idea to the real solution,
-- -- but the issue was I was still trying to find a
-- -- nonexistent unwrapF. I didn't realize I could apply <*>
-- -- to `a` (as <*>'s 2nd argument) without unwrapping `a`, by
-- -- leveraging the <*> for f!

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap aToM (Compose fga) = foldMap (foldMap aToM) fga

-- foldMap aToM (Compose fga) = foldMap u fga

-- foldMap :: (Foldable f, Monoid m) => (g a -> m) -> f (g a) -> m
-- u :: (Foldable g, Monoid m) => g a -> m
-- u ga = foldMap v ga

-- foldMap :: (Foldable g, Monoid m) => (a -> m) -> g a -> m
-- v :: a -> m

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative p
           => (a -> p b) 
           -> Compose f g a
           -> p (Compose f g b)
  traverse aToPb (Compose fga) = Compose <$> traverse (traverse aToPb) fga

  -- traverse :: Applicative p => (a -> p b) -> g a -> p (g b)
  -- traverse :: Applicative p => (g a -> p (g b)) -> f (g a) -> p (f (g b))
