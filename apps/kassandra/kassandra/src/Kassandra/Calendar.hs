module Kassandra.Calendar (
  CalendarEvent (..),
  CalendarList (..),
  EventTime (..),
  TZTime (..),
  sortEvents,
  tzTimeToUTC,
  zonedDay,
  switchToCurrentZone,
) where

import Data.Time
import Kassandra.Config

data TZTime = TZTime
  { time :: ZonedTime
  , zone :: Text
  }
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
makeLabels ''TZTime

data EventTime
  = SimpleEvent {start :: TZTime, end :: TZTime}
  | AllDayEvent {startDay :: Day, endDay :: Day}
  | RecurringEvent
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
makeLabels ''EventTime

data CalendarList = CalendarList
  { entries :: Seq DefinitionElement
  , completed :: Set Text
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
makeLabels ''CalendarList

data CalendarEvent = CalendarEvent
  { uid :: Text
  , time :: EventTime
  , description :: Text
  , location :: Maybe Text
  , comment :: Maybe Text
  , todoList :: CalendarList
  , calendarName :: Text
  }
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
makeLabels ''CalendarEvent

sortEvents :: CalendarEvent -> CalendarEvent -> Ordering
sortEvents = sortEventTimes `on` (^. #time)

sortEventTimes :: EventTime -> EventTime -> Ordering
sortEventTimes lhs rhs = case (lhs, rhs) of
  (SimpleEvent startTimeLhs _, SimpleEvent startTimeRhs _) -> compare (tzTimeToUTC startTimeLhs) (tzTimeToUTC startTimeRhs)
  (AllDayEvent startDayLhs _, AllDayEvent startDayRhs _) -> compare startDayLhs startDayRhs
  (AllDayEvent startDayLhs _, SimpleEvent startTimeRhs _) -> case compare startDayLhs (tzTimeDay startTimeRhs) of EQ -> LT; a -> a
  (SimpleEvent startTimeLhs _, AllDayEvent startDayRhs _) -> case compare (tzTimeDay startTimeLhs) startDayRhs of EQ -> GT; a -> a
  (_, _) -> EQ

switchToCurrentZone :: MonadIO m => ZonedTime -> m ZonedTime
switchToCurrentZone time = do
  let inUtc = zonedTimeToUTC time
  zone <- liftIO $ getTimeZone inUtc
  pure $ utcToZonedTime zone inUtc

tzTimeToUTC :: TZTime -> UTCTime
tzTimeToUTC = zonedTimeToUTC . (^. #time)

tzTimeDay :: TZTime -> Day
tzTimeDay = localDay . zonedTimeToLocalTime . (^. #time)

zonedDay :: ZonedTime -> Day
zonedDay = localDay . zonedTimeToLocalTime
