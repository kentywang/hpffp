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
  let userRow = makeUserRow a b c d e
  conn <- open "finger.db"
  execute conn insertUser userRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn

makeUserRow :: Text -> Text -> Text -> Text -> Text -> UserRow
makeUserRow a b c d e = (Null, a , b, c, d, e)

-- Abstract these into library module
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

allUsers :: Query
allUsers = "SELECT * from users"

type UserRow = (Null, Text, Text, Text, Text, Text)

data User = User { userId :: Integer
                 , username :: Text
                 , shell :: Text
                 , homeDirectory :: Text
                 , realName :: Text
                 , phone :: Text
                 }
  deriving (Eq, Show)

insertUser :: Query
insertUser = "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"
