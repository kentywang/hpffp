{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

-- My changes
shortyTaken :: TL.Text -> TL.Text
shortyTaken tbs =
  TL.concat [ tbs
            , " was already taken."
            ]

app :: R.Connection
    -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Nothing -> text (shortyAintUri uri)
      Just _  -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        -- My changes
        uri <- liftIO (getURI rConn shorty)
        case uri of
          Left reply -> text (TL.pack (show reply))
          Right mbBS -> case mbBS of
            Just _ -> html (shortyTaken $ TL.pack shawty)
            Nothing -> do
              resp <- liftIO (saveURI rConn shorty uri')
              html (shortyCreated resp shawty)
        -- End
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)

foo :: ReaderT R.Connection IO ()
foo = ask >>= lift . scotty 3000 . app
  -- This this kinda weird. We lift that scotty up
  -- into ReaderT R.Connection only to be accessed
  -- again and be passed R.Connection so we end up
  -- with the original IO (). I guess it makes more
  -- sense when there are multiple things that need
  -- the R.Connection passed to it to chain up.
  -- do
  -- rConn <- ask
  -- lift $ scotty 3000 $ app rConn

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  -- Pass in the connection as read-only state. 
  runReaderT foo $ rConn
