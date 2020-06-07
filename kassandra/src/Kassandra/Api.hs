module Kassandra.Api
  ( SocketMessage
  , _TaskUpdates
  , SocketRequest(AllTasks, ChangeTasks)
  , _AllTasks
  , _ChangeTasks
  )
where

declarePrisms [d|newtype SocketMessage = TaskUpdates [Task] deriving (Show, Read, Eq, ToJSON, FromJSON, Generic)|]
declarePrisms [d|data SocketRequest = AllTasks | ChangeTasks (NonEmpty Task) deriving (Show, Read, Eq, ToJSON, FromJSON, Generic)|]
