import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 322
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map g . filter f
  where f (DbDate time) = True
        f _ = False
        g (DbDate time) = time

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map g . filter f
  where f (DbNumber n) = True
        f _ = False
        g (DbNumber n) = n

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max t . filterDbDate
  where t = UTCTime (ModifiedJulianDay 0) 0  -- Not sure if book was looking for this...

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb n = (fromIntegral (sumDb n)) / (fromIntegral (length m))
  where m = filterDbNumber n

main :: IO ()
main = do
  print $ filterDbDate theDatabase
  print $ filterDbNumber theDatabase
  print $ mostRecent theDatabase
  print $ sumDb theDatabase
  print $ avgDb theDatabase