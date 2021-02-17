module Kassandra.Calendar (
  CalendarEvent (..),
  EventTime (..),
) where

import Data.Time
import Kassandra.Config

data EventTime
  = SimpleEvent {startTime :: UTCTime, endTime :: UTCTime}
  | AllDayEvent {startDay :: Day, endDay :: Day}
  | RecurringEvent
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CalendarEvent = CalendarEvent
  { uid :: Text
  , time :: EventTime
  , description :: Text
  , todoList :: Seq DefinitionElement
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Ord EventTime where
  SimpleEvent startTimeLhs _ <= SimpleEvent startTimeRhs _ = startTimeLhs <= startTimeRhs
  AllDayEvent startDayLhs _ <= AllDayEvent startDayRhs _ = startDayLhs <= startDayRhs
  AllDayEvent startDayLhs _ <= SimpleEvent startTimeRhs _ = startDayLhs <= utctDay startTimeRhs
  SimpleEvent startTimeLhs _ <=  AllDayEvent startDayRhs _ = utctDay startTimeLhs <= startDayRhs
  _ <= _ = True
instance Ord CalendarEvent where
  lhs <= rhs = time lhs <= time rhs
