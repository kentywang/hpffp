import Control.Monad.Trans.Maybe
import Control.Monad
-- import Control.Applicative

isValid :: String -> Bool 
isValid v = '!' `elem` v

-- This is the IO instance for Alternative:
  -- instance Alternative IO where
  --   empty = failIO "mzero"
  --   (<|>) = mplusIO

-- Where failIO is:
  -- failIO :: String -> IO a
  -- failIO s = IO (raiseIO# (mkUserError s))

-- So if guard's type was `IO ()`, then we can't do much with the failure case,
-- since it raises an exception.
-- Adding a <|> doesn't help either, since it's type would have to be IO (),
-- which is the same type as the success case.

-- So we need to have guard be type `MaybeT IO ()`, since then the success
-- case is `Maybe (return ())` and the failure case is `MaybeT (return Nothing)`,
-- both of which are handleable.

-- We essentially have this (where guard :: MaybeT IO ()):
  -- guard (isValid v) >>= \_ -> return v

-- Since that pretty much means mapping `\_ -> return v` to
-- `MaybeT IO ()` and then `join`ing, we're applying
-- `\_ -> return v` to either a Nothing or a Just () wrapping in an IO,
-- which we can handle and is not an exception.

-- Btw, we can use guard for MaybeT because there is this instance:
  -- instance (Functor m, Monad m) => Alternative (MaybeT m) where
  --     empty = MaybeT (return Nothing)
  --     x <|> y = MaybeT $ do
  --         v <- runMaybeT x
  --         case v of
  --             Nothing -> runMaybeT y
  --             Just _  -> return v

-- Thanks to https://stackoverflow.com/a/31263127
maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- MaybeT $ Just <$> getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!" 
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE" 
    Just e ->
      putStrLn
        ("Good, was very excite: " ++ e)