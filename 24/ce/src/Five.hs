{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Five where

import Control.Applicative
import Data.List.Extra (trim)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map as M
import Test.QuickCheck
import Text.RawString.QQ
import Text.Trifecta

-- Can't use type alias for LogFile and Events as we'll
-- be creating our own Show instances.
newtype LogFile =
  LogFile { getDateLogs :: [DateLog] }
  deriving Eq

data DateLog =
  DateLog { getDay    :: Day
          , getDateLogEvents :: Events }
  deriving Eq

newtype Events =
  Events { getEvents :: [Event] }
  deriving Eq

data Event =
  Event { getTimeOfDay :: TimeOfDay
        , getActivity  :: Activity  }
  deriving Eq

type Activity = String

-- `spaces` seems to skip newlines too, so careful.

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseComment :: Parser ()
parseComment = string "--" >> many (noneOf "\n") >> oneOf "\n" >> pure ()

-- | Also acts as a general-purpose whitespace consumer.
skipComments :: Parser ()
skipComments = do
  -- Skip any extra empty lines before &
  -- after the comment line.
  spaces
  skipMany parseComment
  spaces

-- | Consumes trailing WS, including and stopping after NL.
-- Not working right as intended right now...
whiteLine :: Parser ()
whiteLine = do
  many (char ' ') >> oneOf "\n" >> pure ()

parseDateLog :: Parser DateLog
parseDateLog = do
  skipComments
  date <- parseDate
  events <- many parseEvent
  if isOrdered events
  then pure $ DateLog date (Events events)
  -- Not optimized, could do after every time parse,
  -- instead of after all events parsed.
  else fail "Activities must be ordered by time."
  where isOrdered :: [Event] -> Bool
        isOrdered = snd . foldl f (midnight, True)
        f :: (TimeOfDay, Bool) -> Event -> (TimeOfDay, Bool)
        f tup@(_, False) _ = tup
        f (x, True) (Event y _) = (y, x <= y)

parseDate :: Parser Day
parseDate = do
  char '#'
  spaces
  -- allowing for non-4 digit years
  yyyy <- read <$> some digit
  char '-'
  mm <- read <$> count 2 digit
  char '-'
  dd <- read <$> count 2 digit
  skipComments
  return $ fromGregorian yyyy mm dd

parseEvent :: Parser Event
parseEvent = do
  time <- parseTime
  char ' '
  activity <- parseActivity
  pure $ Event time activity

parseTime :: Parser TimeOfDay
parseTime = do
  hh <- read <$> count 2 digit
  char ':'
  mm <- read <$> count 2 digit
  return $ TimeOfDay hh mm 0

-- manyTill here solves the problem where I need to continuously
-- check a certain string (in this case the comment and
-- newline and eof markerst) to see if I should end early. So
-- it's kinda like noneOf, but works for strings.
parseActivity :: Parser Activity
parseActivity =
  fmap trim $ manyTill anyChar $
    -- try parseComment <|>
    -- try (char '\n' >> pure ()) <|>
    -- try eof
    choice [ parseComment
           , (char '\n' >> pure ())
           , eof ]

parseLog :: Parser LogFile
parseLog = LogFile <$> some parseDateLog

instance Show LogFile where
  show (LogFile dateLogs) = foldl f "" dateLogs
    where f acc cur = acc <> "\n" <> show cur

instance Show DateLog where
  show (DateLog d evts) =
    mconcat [ "# " , show d, "\n", show evts]

instance Show Events where
  show (Events evts) = foldr f "" evts
    where f cur acc = show cur <> "\n" <> acc

instance Show Event where
  show (Event time activity) =
    take 5 (show time) <> " " <> activity

instance Arbitrary LogFile where
  arbitrary = LogFile <$> arbitrary

instance Arbitrary DateLog where
  arbitrary = do
    day <- arbitrary
    events <- arbitrary
    return $ DateLog day $ Events events

instance Arbitrary Day where
  arbitrary = do
    yyyy <- suchThat arbitrary (>=0)
    mm <- suchThat arbitrary (>=0)
    dd <- suchThat arbitrary (>=0)
    return $ fromGregorian yyyy mm dd

instance Arbitrary TimeOfDay where
  arbitrary = do
    hh <- suchThat arbitrary (>=0)
    mm <- suchThat arbitrary (>=0)
    return $ TimeOfDay hh mm 0

instance Arbitrary Event where
  arbitrary = do
    time <- arbitrary
    activity <- listOf $ elements $ ['a'..'z'] <> [' ', '-', ',']
    pure $ Event time activity

-- This is not the best representation of the log file
-- since we start counting durations with the first event
-- of each day, meaning the time since midnight to the
-- first event is not counted.
logToActDurMaps :: LogFile -> [M.Map Activity DiffTime]
logToActDurMaps (LogFile dateLogs) =
  daysEventsToActDurMap . getEvents . getDateLogEvents <$> dateLogs

oneDayDuration :: DiffTime
oneDayDuration = secondsToDiffTime $ 60 * 60 * 24

daysEventsToActDurMap :: [Event] -> M.Map Activity DiffTime
daysEventsToActDurMap = snd . foldr f (oneDayDuration, mempty)
  where f :: Event -> (DiffTime, M.Map Activity DiffTime)
          -> (DiffTime, M.Map Activity DiffTime)
        -- Add the current event's duration to property
        -- in map (or create a new property with that duration if
        -- activity doesn't yet exist in map).
        f (Event tod act) (nextActTime, accumMap) =
          let currActTime = timeOfDayToTime tod
          in (currActTime, M.insertWith (+) act (nextActTime - currActTime) accumMap)

-- mconcat won't do what we want here since it is just an alias
-- for `unions`, which is a left-biased foldl of the BSTs.
activitySumDuration :: LogFile -> M.Map Activity DiffTime
activitySumDuration = M.unionsWith (+) . logToActDurMaps

-- Duplication of work (logToActDurMaps), so not optimal.
avgActivityDuration :: LogFile -> M.Map Activity DiffTime
avgActivityDuration = liftA2 (<$>) f activitySumDuration
  -- Need to convert length (i.e. number of dateLogs) to DiffTime to
  -- for it to be divisor to each DiffTime.
  -- Not sure why need secondsToDiffTime instead of picosecondsToDiffTime,
  -- since Pico is how DiffTime stores the value internally.
  where f =
          flip (/) . secondsToDiffTime .
          toInteger . length . logToActDurMaps

-- | Fails on empty strings, but that's expected?
-- Also need to generate activities ordered in time.
main = do
  let logFile = parseString parseLog mempty sampleLog
  -- print $ logFile

  -- logFiles <- sample' $ arbitrary @LogFile
  -- sequenceA $
  --   print .
  --   parseString parseLog mempty .
  --   show <$> logFiles
  -- return ()

  print $ activitySumDuration <$> logFile
  print $ avgActivityDuration <$> logFile

sampleLog :: String
sampleLog = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]