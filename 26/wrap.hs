module OuterInner where

import Control.Monad.Trans.Except 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Reader

-- We only need to use return once 
-- because it's one big Monad
embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap
readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- The other way around
readerWrap :: ReaderT () IO (Either String (Maybe Int))
readerWrap = ReaderT readerUnwrap
eitherWrap :: ExceptT String (ReaderT () IO) (Maybe Int)
eitherWrap = ExceptT readerWrap
maybeWrap :: MaybeT (ExceptT String (ReaderT () IO)) Int
maybeWrap = MaybeT eitherWrap

-- Darn, relied on online answer for this one.
embedded' :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded' = let x :: () -> Either String (Maybe Int)
                x = const (Right (Just 1))
                y :: a -> IO a
                y = pure
                z :: () -> IO (Either String (Maybe Int))
                z = y . x
            in MaybeT $ ExceptT $ ReaderT z