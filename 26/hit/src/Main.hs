{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

-- So even though we pass the same Config value for each response,
-- the inner value `counts` can be different because IORef means
-- it's a mutable reference?
data Config = Config
  { counts :: IORef (M.Map Text Integer),
    prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = 
  case M.lookup k m of
    Nothing -> (M.insert k 1 m, 1)
    Just a -> (M.insert k (a + 1) m, (a + 1))

app :: Scotty () -- This is: ScottyT Text (ReaderT Config IO) ()
app =
  get "/:key" $ do
    unprefixed <- (param "key" :: Handler Text) -- Why is this allowed to be not `Handler ()`?
    pref <- lift $ ReaderT $ pure . prefix
    cts <- lift $ ReaderT $ readIORef . counts -- And isn't this `Handler (M.Map Text Integer)`?
    let key' = mappend pref unprefixed
        (newMap, newInt) = bumpBoomp key' cts -- I didn't realize this is even possible.
    -- newInteger <- counts
    html $
      mconcat
        [ "<h1>Success! Count was: ",
          key',
          TL.pack $ show newInt,
          "</h1>"
        ]
    lift $ ReaderT $ \r -> modifyIORef' (counts r) (const newMap)

main :: IO ()
main = do
  prefixArg <- TL.pack . head <$> getArgs
  counter <- newIORef M.empty -- counter :: IORef (M.Map Text Integer))
  let config :: Config
      config = Config counter prefixArg
      -- runR :: ReaderT Config IO Response -> IO Response
      runR (ReaderT cIOr) = cIOr config
  scottyT 3000 runR app

-- This works, but I understand so little of the actual inner workings that
-- it makes me feel uncomfortable. I need to spend a lot more time with it.