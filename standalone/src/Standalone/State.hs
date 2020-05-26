module Standalone.State
  ( ioStateProvider
  , ioStateFeeder
  )
where

import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                )
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import           Frontend.Types          hiding ( getTasks )
import           Frontend.State                 ( stateProvider
                                                , TaskProvider
                                                , StateProvider
                                                )
import           Backend                        ( taskMonitor )

ioTaskProvider
  :: (WidgetIO t m) => MVar (NonEmpty Task -> IO ()) -> TaskProvider t m
ioTaskProvider callbackSlot changeTaskEvent = do
  void . R.performEvent $ liftIO . saveTasks . toList <$> changeTaskEvent
  (tasksEvent, newTasksCallBack) <- R.newTriggerEvent
  tasks <- R.foldDyn (flip . foldr . join $ HashMap.insert . (^. #uuid))
                     HashMap.empty
                     (changeTaskEvent <> tasksEvent)
  putMVar callbackSlot newTasksCallBack
  R.holdUniqDyn tasks

ioStateFeeder :: MVar (NonEmpty Task -> IO ()) -> IO ()
ioStateFeeder callbackSlot = do
  callback <- takeMVar callbackSlot
  concurrently_ (whenNotNullM (getTasks []) callback) (taskMonitor callback)

ioStateProvider
  :: WidgetIO t m => MVar (NonEmpty Task -> IO ()) -> StateProvider t m
ioStateProvider callbackSlot = stateProvider (ioTaskProvider callbackSlot)
