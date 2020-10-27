{-# Language InstanceSigs #-}

import Data.Functor.Classes
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (Reader, reader)

main :: IO (EitherT Bool [] Integer)
main = do
  return $ fmap (+1) $ EitherT [Left True, Right 2]

-- Trying to write a MaybeT with the Maybe as the outside monad.

-- newtype MaybeT m a =
--   MaybeT { runMaybeT :: Maybe (m a) }

-- instance (Functor m) => Functor (MaybeT m) where
--   fmap f (MaybeT ma) =
--     MaybeT $ (fmap . fmap) f ma

-- instance (Applicative m) => Applicative (MaybeT m) where
--   pure x = MaybeT (pure (pure x))

--   (MaybeT fab) <*> (MaybeT mma) =
--     MaybeT $ (<*>) <$> fab <*> mma

-- -- instance (Monad m) => Monad (MaybeT m) where
-- --   return = pure

-- --   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
-- --   MaybeT mbma >>= f =
-- --     -- mbma :: Maybe (m a)
-- --     -- ma :: m a
-- --     -- f <$> ma :: m (MaybeT m b)
-- --     -- Can't do much after that.

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mmba >>= f =
    MaybeT $ do
      mba <- mmba
      case mba of
        Nothing -> pure Nothing
        Just a -> runMaybeT $ f a

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

-- Writing this is more complicated than I thought.
-- instance Show1 m => Show1 (EitherT e m) where

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea 

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT emab <*> EitherT ema = EitherT $ (<*>) <$> emab <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT mea >>= f =
    EitherT $ do
      ea <- mea
      -- This doesn't work because the case return types don't match
      -- case f <$> ea of
      --   Left _ -> mea
      --   Right (EitherT meb) -> meb
      case ea of
        Left e -> pure $ Left e
        Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left l) = Right l
swapEither (Right r) = Left r

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT mab) = do
  ab <- mab
  case ab of
    Left a -> amc a
    Right b -> bmc b

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))

  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  ReaderT rma >>= f =
    ReaderT $ \r -> do
      a <- rma r
      let rmb = runReaderT $ f a
      rmb r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) =
    StateT $ \s -> g <$> sma s -- `sma s` is type `m (a, s)`
    where g (a, s) = (f a, s)

-- Is it possible to do this with Applicative m? A: Nope.
instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma =
    StateT $ \s -> do
      (ab, s') <- smab s
      (a, s'') <- sma s'
      pure (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f =
    StateT $ \s -> do
      (a, s') <- sma s
      let smb = runStateT $ f a
      smb s'

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m =
    StateT $ (<$> m) . flip (,)
  -- Should be the same as:
  -- lift m = StateT $ \s -> do
  --   a <- m
  --   return (a, s)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO 
  -- The liftIO used here is type IO a -> m a

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO

-- Below experiment shows that the inner value of the monad transformer, foo,
-- is different. I think this is because ReaderT doesn't actually use Reader in
-- its data construction.

-- Otherwise, I guess they operate essentially the same, especially if you implement
-- Reader as just a partially applied function instead of the type alias of
-- ReaderT (as it is here).
x :: ReaderT r Maybe Int
x = ReaderT $ foo
  where foo :: a -> Maybe Int
        foo = \_ -> Just 2

y :: MaybeT (Reader r) Int
y = MaybeT $ foo
  where foo :: Reader a (Maybe Int)
        foo = reader $ \_ -> Just 2
