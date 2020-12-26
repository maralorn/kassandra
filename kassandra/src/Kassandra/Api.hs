module Kassandra.Api
  ( SocketMessage(TaskUpdates, UIConfigResponse)
  , _TaskUpdates
  , SocketRequest(AllTasks, ChangeTasks, UIConfigRequest)
  , _AllTasks
  , _ChangeTasks
  )
where

import Kassandra.Config (UIConfig)

declarePrisms [d|
  data SocketMessage = TaskUpdates (NonEmpty Task) | UIConfigResponse UIConfig
   deriving stock (Show, Read, Eq, Generic)
   deriving anyclass (ToJSON, FromJSON)|]
declarePrisms [d|
  data SocketRequest = UIConfigRequest | AllTasks | ChangeTasks (NonEmpty Task)
   deriving stock (Show, Read, Eq, Generic)
   deriving anyclass (ToJSON, FromJSON)|]
