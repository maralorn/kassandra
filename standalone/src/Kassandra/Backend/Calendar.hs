{-# LANGUAGE BlockArguments #-}

module Kassandra.Backend.Calendar (
  getEvents,
) where

import Data.Default
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Time
import Kassandra.Calendar
import Say
import System.FilePath
import System.FilePattern.Directory
import System.IO.Unsafe
import Text.ICalendar
import Control.Concurrent.Async

dirName :: Seq FilePath
dirName = ("/home/maralorn/.calendars/nextcloud/" </>) <$> fromList ["Planung", "Standard", "Uni"]

getEvents :: IO (Seq CalendarEvent)
getEvents = do
  calendarFiles <- (fromList =<<) <$> forM dirName (\x -> fmap (x </>) <$> getDirectoryFiles x ["*.ics"])
  now <- getCurrentTime
  Seq.sort . Seq.filter (onlyNextWeek now) . translateCalendar . join
    <$> forConcurrently
      calendarFiles
      ( parseICalendarFile def >=> \case
          Right (fromList -> calendarsInFile, fmap toText -> warnings) -> do
            mapM_ (\x -> sayErr [i|WARN: #{x}|]) warnings
            pure calendarsInFile
          Left (toText -> parseError) -> do
            sayErr [i|ERROR: #{parseError}|]
            pure mempty
      )

onlyNextWeek :: UTCTime -> CalendarEvent -> Bool
onlyNextWeek now CalendarEvent{time}
  | SimpleEvent start end <- time = end >= now && start <= addUTCTime (14 * nominalDay) now
onlyNextWeek _ _ = False

translateCalendar :: Seq VCalendar -> Seq CalendarEvent
translateCalendar calendars = fmap translateEvent . fromList . Map.elems . vcEvents =<< calendars

translateEvent :: VEvent -> CalendarEvent
translateEvent vEvent =
  CalendarEvent
    { uid = toStrict (uidValue (veUID vEvent))
    , description = (maybe "" (toStrict . summaryValue) . veSummary) vEvent
    , todoList = mempty
    , time
    }
 where
  time
    | Just (DTStartDateTime start _) <- veDTStart vEvent
      , Just (Left (DTEndDateTime end _)) <- veDTEndDuration vEvent =
      SimpleEvent (datetimeToUTC start) (datetimeToUTC end)
    | otherwise = RecurringEvent

{-# NOINLINE timezone #-}
timezone :: TimeZone
timezone = unsafePerformIO getCurrentTimeZone

datetimeToUTC :: DateTime -> UTCTime
datetimeToUTC = \case
  FloatingDateTime x -> localTimeToUTC timezone x
  UTCDateTime x -> x
  ZonedDateTime x _ -> localTimeToUTC timezone x
