module Kassandra.Api (
  SocketMessage (..),
  SocketRequest (..),
) where

import Kassandra.Config (UIConfig)

type SocketError = Text

data SocketMessage = TaskUpdates (NonEmpty Task) | CalendarUpdate | UIConfigResponse UIConfig | SocketError SocketError | ConnectionEstablished
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketMessage

data SocketRequest = UIConfigRequest | AllTasks | ChangeTasks (NonEmpty Task)
      deriving stock (Show, Read, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketRequest
