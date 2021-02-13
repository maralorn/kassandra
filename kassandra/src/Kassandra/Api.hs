module Kassandra.Api (
  SocketMessage (TaskUpdates, UIConfigResponse),
  SocketRequest (AllTasks, ChangeTasks, UIConfigRequest),
) where

import Kassandra.Config (UIConfig)

data SocketMessage = TaskUpdates (NonEmpty Task) | CalendarUpdate | UIConfigResponse UIConfig
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketMessage

data SocketRequest = UIConfigRequest | AllTasks | ChangeTasks (NonEmpty Task)
      deriving stock (Show, Read, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON)
makePrismLabels ''SocketRequest
