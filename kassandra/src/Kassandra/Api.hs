module Kassandra.Api (
  SocketMessage (..),
  SocketRequest (..),
) where

import Kassandra.Calendar
import Kassandra.Config (UIConfig)

type SocketError = Text

data SocketMessage
  = TaskUpdates (NESeq Task)
  | CalendarEvents (Seq CalendarEvent)
  | UIConfigResponse UIConfig
  | SocketError SocketError
  | ConnectionEstablished
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketMessage

data SocketRequest
  = UIConfigRequest
  | AllTasks
  | CalenderRequest
  | ChangeTasks (NESeq Task)
  | SetCalendarList Text CalendarList
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketRequest
