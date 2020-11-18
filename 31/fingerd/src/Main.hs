{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent (forkIO, newMVar, putMVar, takeMVar
                                   , MVar)
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

data User = User { userId :: Integer
                 , username :: Text
                 , shell :: Text
                 , homeDirectory :: Text
                 , realName :: Text
                 , phone :: Text
                 }
  deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers =
  [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> return $ Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where
    meRow :: UserRow
    meRow = ( Null
            , "callen"
            , "/bin/zsh"
            , "/home/callen"
            , "Chris Allen"
            , "555-123-4567")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  [ "Login: "
  , e username
  , "\t\t\t\t"
  , "Name: "
  , e realName
  , "\n"
  , "Directory: "
  , e homeDir
  , "\t\t\t"
  , "Shell: "
  , e shell
  , "\n"]
  where
    e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing   -> do
      putStrLn $ "Couldn't find matching user for username: " ++ show username
      return ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name   -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> MVar () -> IO ()
handleQueries dbConn sock m = forever
  $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    takeMVar m
    handleQuery dbConn soc
    putMVar m ()
    Network.Socket.close soc

makeUserRow :: [Text] -> UserRow
makeUserRow [a, b, c, d, e] = (Null, a, b, c, d, e)

-- No check for if user already exists, yolo
insertUserToDb :: Connection -> Socket -> [Text] -> IO ()
insertUserToDb dbConn soc userFields = do
  let (username:_) = userFields
      userRow = makeUserRow userFields
  execute dbConn insertUser userRow
  [user] <- query dbConn getUserQuery $ Only username -- the username
  sendAll soc (formatUser user)

handleInsert :: Connection -> Socket -> IO ()
handleInsert dbConn soc = do
  msg <- recv soc 1024
  insertUserToDb dbConn soc $ format msg -- no validation for insert request, yolo
  where
    format = T.words . T.strip . decodeUtf8

handleInserts :: Connection -> Socket -> MVar () -> IO ()
handleInserts dbConn sock m = forever
  $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling insert"
    takeMVar m
    handleInsert dbConn soc
    putMVar m ()
    Network.Socket.close soc

fingerService :: MVar () -> IO ()
fingerService m = withSocketsDo
  $ do
    addrinfos <- getAddrInfo
      (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
      Nothing
      (Just "79")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)
    listen sock 1
    -- only one connection open at a time 
    conn <- open "finger.db"
    handleQueries conn sock m
    SQLite.close conn
    Network.Socket.close sock

-- Listen to some other socket. If it receives a request, update the DB.
dbInsertService :: MVar () -> IO ()
dbInsertService m = withSocketsDo
  $ do
    addrinfos <- getAddrInfo
      (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
      Nothing
      (Just "80")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)
    listen sock 1
    -- only one connection open at a time 
    conn <- open "finger.db"
    handleInserts conn sock m
    SQLite.close conn
    Network.Socket.close sock

-- Technically we don't need MVar for this, since our shared mutable state is the db file
-- itself. But we can use MVar to act as a lock on when threads can run/not run.
main :: IO ()
main = do
  m <- newMVar ()
  forkIO $ dbInsertService m
  fingerService m
