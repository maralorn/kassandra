module Kassandra.State (
  makeStateProvider,
  StateProvider,
  ClientSocket,
  DataState (..),
) where

import qualified Data.HashMap.Strict as HashMap
import Kassandra.Api (SocketRequest (ChangeTasks), SocketMessage)
import Kassandra.Config (UIConfig)
import Kassandra.Types (
  DataChange,
  TaskInfos (..),
  TaskState,
  WidgetIO,
 )
import qualified Reflex as R
import qualified Reflex.Dom as D
import Taskwarrior.IO (createTask)

getParents :: HashMap UUID Task -> UUID -> [UUID]
getParents tasks = go [] (\uuid -> (^. #partof) =<< tasks ^. at uuid)
 where
  go :: (Eq a, Show a) => [a] -> (a -> Maybe a) -> a -> [a]
  go accu f x
    | x `elem` accu = []
    | Just next <- f x = next : go (x : accu) f next
    | otherwise = []

data DataState = DataState
  { taskState :: TaskState
  , uiConfig :: UIConfig
  , calendarData :: ()
  }
makeLabels ''DataState

type StateProvider t m = R.Event t (NonEmpty DataChange) -> m (R.Dynamic t DataState)

type ClientSocket t m = R.Event t SocketRequest -> m (R.Dynamic t (R.Event t SocketMessage))

makeStateProvider :: forall t m. WidgetIO t m => ClientSocket t m -> StateProvider t m
makeStateProvider clientSocket dataChangeEvents = do
  let fanEvent :: (b -> Maybe a) -> R.Event t (NonEmpty b) -> R.Event t (NonEmpty a)
      fanEvent decons = R.fmapMaybe (nonEmpty . mapMaybe decons . toList)
      createTaskEvent = fanEvent (^? #_CreateTask) dataChangeEvents
      changeTaskEvent = fanEvent (^? #_ChangeTask) dataChangeEvents
  changesFromCreateEvents <- createToChangeEvent createTaskEvent
  let localChanges = changeTaskEvent <> changesFromCreateEvents
  remoteChanges <- R.switchDyn <$> clientSocket (ChangeTasks <$> localChanges)
  tasksStateDyn <- buildTaskInfosMap <<$>> holdTasks (localChanges <> R.fmapMaybe (^? #_TaskUpdates) remoteChanges)
  pure $ DataState <$> tasksStateDyn <*> pure D.def <*> pass

createToChangeEvent :: WidgetIO t m => D.Event t (NonEmpty (Text, Task -> Task)) -> m (D.Event t (NonEmpty Task))
createToChangeEvent = R.performEvent . fmap (liftIO . mapM (\(desc, properties) -> properties <$> createTask desc))

holdTasks :: WidgetIO t m => R.Event t (NonEmpty Task) -> m (R.Dynamic t (HashMap UUID Task))
holdTasks = R.foldDyn foldTasks mempty

foldTasks :: Foldable t => t Task -> HashMap UUID Task -> HashMap UUID Task
foldTasks = flip (foldr (\task -> HashMap.insert (task ^. #uuid) task))

buildChildrenMap :: HashMap a Task -> HashMap UUID [a]
buildChildrenMap =
  HashMap.fromListWith (++)
    . mapMaybe (\(uuid, task) -> (,pure uuid) <$> task ^. #partof)
    . HashMap.toList

buildDependenciesMap :: HashMap a Task -> HashMap UUID [a]
buildDependenciesMap = HashMap.fromListWith (++) . (HashMap.toList >=> \(uuid, task) -> (,pure uuid) <$> toList (task ^. #depends))

buildTaskInfosMap :: HashMap UUID Task -> TaskState
buildTaskInfosMap tasks =
  HashMap.mapWithKey foldTaskMap tasks
 where
  foldTaskMap uuid task =
    TaskInfos
      { task = task
      , children = HashMap.lookupDefault [] uuid childrenMap
      , parents = getParentTasks uuid
      , revDepends = HashMap.lookupDefault [] uuid dependenciesMap
      , blocked = isBlockedTask task
      }
  isBlockedTask = isBlocked tasks
  getParentTasks = getParents tasks
  dependenciesMap = buildDependenciesMap tasks
  childrenMap = buildChildrenMap tasks

isBlocked :: HashMap UUID Task -> Task -> Bool
isBlocked tasks task = any isActive . mapMaybe (`HashMap.lookup` tasks) . toList $ task ^. #depends
 where
  isActive t = has (#status % #_Pending) t || has (#status % #_Waiting) t
