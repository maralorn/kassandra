module Kassandra.Calendar (
  CalendarEvent (..),
  EventTime (..),
  TZTime (..),
  sortEvents,
  tzTimeToUTC,
) where

import Data.Time
import Kassandra.Config

data EventTime
  = SimpleEvent {start :: TZTime, end :: TZTime}
  | AllDayEvent {startDay :: Day, endDay :: Day}
  | RecurringEvent
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TZTime = TZTime
  { time :: ZonedTime
  , zone :: Text
  }
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
makeLabels ''TZTime

data CalendarEvent = CalendarEvent
  { uid :: Text
  , time :: EventTime
  , description :: Text
  , todoList :: Seq DefinitionElement
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
  (AllDayEvent startDayLhs _, SimpleEvent startTimeRhs _) -> compare startDayLhs (zonedDay startTimeRhs)
  (SimpleEvent startTimeLhs _, AllDayEvent startDayRhs _) -> compare (zonedDay startTimeLhs) startDayRhs
  (_, _) -> EQ

tzTimeToUTC :: TZTime -> UTCTime
tzTimeToUTC = zonedTimeToUTC . (^. #time)

zonedDay :: TZTime -> Day
zonedDay = localDay . zonedTimeToLocalTime . (^. #time)
