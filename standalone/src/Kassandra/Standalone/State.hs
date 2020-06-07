module Kassandra.Standalone.State
  ( ioStateProvider
  , ioStateFeeder
  , taskMonitor
  )
where

import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                )
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import           Kassandra.Types         hiding ( getTasks )
import           Kassandra.State                ( stateProvider
                                                , TaskProvider
                                                , StateProvider
                                                )
import qualified Network.Simple.TCP            as Net
import qualified Data.Aeson                    as Aeson

taskMonitor :: (NonEmpty Task -> IO ()) -> IO ()
taskMonitor newTasksCallBack = do
  putStrLn "Listening for changed or new tasks on 127.0.0.1:6545."
  Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) ->
    Net.recv socket 4096 >>= maybe
      (putStrLn "Unsuccessful connection attempt.")
      (\changes ->
        either
            (\err -> putStrLn [i|Couldn‘t decode #{changes} as Task: #{err}|])
            (newTasksCallBack . one)
          $ Aeson.eitherDecodeStrict @Task changes
      )

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
