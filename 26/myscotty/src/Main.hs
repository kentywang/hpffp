{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Web.Scotty
-- import Web.Scotty.Internal.Types (ActionT(..))
-- import Data.Monoid (mconcat)
-- import Control.Monad
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.State.Strict hiding (get)

-- main = scotty 3000 $ do
--   get "/:word" $ do
--     beam <- param "word"
--     (ActionT
--      . ExceptT . liftM Right
--      . ReaderT . const
--      . \m -> StateT (\s -> do
--                         a <- m
--                         return (a, s))
--      ) (putStrLn "hello")
--     html $
--       mconcat ["<h1>Scotty, ",
--                beam,
--                " me up!</h1>"]

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Maybe
-- import Data.Maybe (fromMaybe)
-- import Data.Text.Lazy (Text)
-- import Web.Scotty

-- param' :: Parsable a => Text -> MaybeT ActionM a
-- param' k =
--   MaybeT $
--     rescue
--       (Just <$> param k)
--       (const (return Nothing))

-- type Reco = (Integer, Integer, Integer, Integer)

-- main =
--   scotty 3000 $ do
--     get "/:word" $ do
--       beam <- param "word"
--       reco <- runMaybeT $ do
--         a <- param' "1"
--         liftIO $ print a
--         b <- param' "2"
--         c <- param' "3"
--         d <- param' "4"
--         (lift . lift) $ print b
--         return ((a, b, c, d) :: Reco)
--       liftIO $ print reco
--       html $
--         mconcat
--           [ "<h1>Scotty, ",
--             beam,
--             " me up!</h1>"
--           ]

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Web.Scotty

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k =
  ExceptT $
    rescue
      (Right <$> param k)
      (const (return (Left $ "The key: " ++ show k ++ " was missing!")))

type Reco = (Integer, Integer, Integer, Integer)

tshow = TL.pack . show

main =
  scotty 3000 $ do
    get "/" $ do
      reco <- runExceptT $ do
        a <- param' "1"
        liftIO $ print a
        b <- param' "2"
        c <- param' "3"
        d <- param' "4"
        (lift . lift) $ print b
        return ((a, b, c, d) :: Reco)
      case reco of
        (Left e) -> text (TL.pack e)
        (Right r) ->
          html $
            mconcat
              [ "<h1>Success! Reco was: ",
                tshow r,
                "</h1>"
              ]
