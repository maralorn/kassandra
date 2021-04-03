module Kassandra.Api (
  SocketMessage (..),
  SocketRequest (..),
) where

import Kassandra.Calendar
import Kassandra.Config (UIConfig)

type SocketError = Text

data SocketMessage
  = TaskUpdates (NonEmpty Task)
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
  | ChangeTasks (NonEmpty Task)
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketRequest
