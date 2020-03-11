module Common.Api
  ( SocketMessage(TaskUpdates)
  , SocketRequest(AllTasks)
  )
where

newtype SocketMessage = TaskUpdates [Task] deriving (Show, Read, Eq, ToJSON, FromJSON, Generic)
data SocketRequest = AllTasks | ChangeTasks (NonEmpty Task) deriving (Show, Read, Eq, ToJSON, FromJSON, Generic)
