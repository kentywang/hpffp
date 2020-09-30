import Data.Char
-- import Control.Monad.Reader

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
-- tupled xs = (cap xs, reverse xs)
-- tupled = (,) <$> cap <*> rev
-- tupled = do
--   a <- cap
--   b <- rev
--   return (a, b)
tupled = 
  cap >>= \a -> 
    rev >>= \b -> 
      pure (a, b)

-- Looks like the Reader in Control.Monad.Reader
-- is different than what the book presents?
newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id