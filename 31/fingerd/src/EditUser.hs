{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception
import           Control.Monad (forever)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import           Text.RawString.QQ

import System.Environment (getArgs)

main :: IO ()
main = do
  [a, b, c, d, e] <- (fmap . fmap) T.pack getArgs
  conn <- open "finger.db"
  maybeUser <- getUser conn a
  case maybeUser of
    Nothing   -> do
      putStrLn $ "Couldn't find matching user for username: " ++ show a
      return ()
    Just user -> do
      execute conn editUser (b, c, d, e, a)
      [user] <- query conn getUserQuery (Only a)
      print (user :: User)
      SQLite.close conn


-- Abstract these into library module
getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> return $ Nothing
    [user] -> return $ Just user
    
getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

allUsers :: Query
allUsers = "SELECT * from users"

data User = User { userId :: Integer
                 , username :: Text
                 , shell :: Text
                 , homeDirectory :: Text
                 , realName :: Text
                 , phone :: Text
                 }
  deriving (Eq, Show)

editUser :: Query
editUser = [r|
UPDATE users
SET shell = ?,
    homeDirectory = ?,
    realName = ?,
    phone = ?
WHERE username = ?
|]
