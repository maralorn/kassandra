module Kassandra.State (
  makeStateProvider,
  StateProvider,
  ClientSocket,
  DataState (..),
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import Kassandra.Api (SocketMessage (..), SocketRequest (..))
import Kassandra.Calendar
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

getParents :: HashMap UUID Task -> UUID -> Seq UUID
getParents tasks = go mempty (\uuid -> (^. #partof) =<< tasks ^. at uuid)
 where
  go :: (Eq a, Show a) => Seq a -> (a -> Maybe a) -> a -> Seq a
  go accu f x
    | x `elem` accu = mempty
    | Just next <- f x = next <| go (x <| accu) f next
    | otherwise = mempty

data DataState = DataState
  { taskState :: TaskState
  , uiConfig :: UIConfig
  , calendarData :: Seq CalendarEvent
  }
makeLabels ''DataState

type StateProvider t m = R.Event t (NESeq DataChange) -> m (R.Dynamic t DataState)

type ClientSocket t m = R.Event t (NESeq SocketRequest) -> m (R.Dynamic t (R.Event t SocketMessage))

makeStateProvider :: forall t m. WidgetIO t m => ClientSocket t m -> StateProvider t m
makeStateProvider clientSocket dataChangeEvents = do
  let fanEvent :: (b -> Maybe a) -> R.Event t (NESeq b) -> R.Event t (NESeq a)
      fanEvent decons = R.fmapMaybe (nonEmptySeq . mapMaybe decons . toSeq)
      createTaskEvent = fanEvent (^? #_CreateTask) dataChangeEvents
      changeTaskEvent = fanEvent (^? #_ChangeTask) dataChangeEvents
      setListEvent = fanEvent (^? #_SetEventList) dataChangeEvents
  changesFromCreateEvents <- createToChangeEvent createTaskEvent
  let localChanges = changeTaskEvent <> changesFromCreateEvents
  rec remoteChanges <- R.switchDyn <$> clientSocket (fold eventsToSend)
      let connectedEvent = (^? #_ConnectionEstablished) <$?> remoteChanges
          eventsToSend =
            [ one . ChangeTasks <$> localChanges
            , AllTasks :<|| fromList [CalenderRequest, UIConfigRequest] <$ connectedEvent
            , uncurry SetCalendarList <<$>> setListEvent
            ]
  let errorEvent = (^? #_SocketError) <$?> remoteChanges
  calendarData <- R.holdDyn mempty $ Seq.sortBy sortEvents <$> ((^? #_CalendarEvents) <$?> remoteChanges)
  uiConfig <- R.holdDyn D.def $ (^? #_UIConfigResponse) <$?> remoteChanges
  D.dynText =<< R.foldDyn (<>) "" errorEvent
  tasksStateDyn <- buildTaskInfosMap <<$>> holdTasks (localChanges <> ((^? #_TaskUpdates) <$?> remoteChanges))
  pure (DataState <$> tasksStateDyn <*> uiConfig <*> calendarData)

createToChangeEvent :: WidgetIO t m => D.Event t (NESeq (Text, Task -> Task)) -> m (D.Event t (NESeq Task))
createToChangeEvent = R.performEvent . fmap (liftIO . mapM (\(desc, properties) -> properties <$> createTask desc))

holdTasks :: WidgetIO t m => R.Event t (NESeq Task) -> m (R.Dynamic t (HashMap UUID Task))
holdTasks = R.foldDyn foldTasks mempty

foldTasks :: Foldable t => t Task -> HashMap UUID Task -> HashMap UUID Task
foldTasks = flip (foldr (\task -> HashMap.insert (task ^. #uuid) task))

buildChildrenMap :: HashMap a Task -> HashMap UUID (Seq a)
buildChildrenMap =
  HashMap.fromListWith (<>)
    . mapMaybe (\(uuid, task) -> (,pure uuid) <$> task ^. #partof)
    . HashMap.toList

buildDependenciesMap :: HashMap a Task -> HashMap UUID (Seq a)
buildDependenciesMap = HashMap.fromListWith (<>) . (HashMap.toList >=> \(uuid, task) -> (,pure uuid) <$> toList (task ^. #depends))

buildTaskInfosMap :: HashMap UUID Task -> TaskState
buildTaskInfosMap tasks =
  HashMap.mapWithKey foldTaskMap tasks
 where
  foldTaskMap uuid task =
    TaskInfos
      { task = task
      , children = HashMap.lookupDefault mempty uuid childrenMap
      , parents = getParentTasks uuid
      , revDepends = HashMap.lookupDefault mempty uuid dependenciesMap
      , blocked = isBlockedTask task
      }
  isBlockedTask = isBlocked tasks
  getParentTasks = getParents tasks
  dependenciesMap = buildDependenciesMap tasks
  childrenMap = buildChildrenMap tasks

isBlocked :: HashMap UUID Task -> Task -> Bool
isBlocked tasks task = any isActive . mapMaybe (`HashMap.lookup` tasks) . toList $ task ^. #depends
 where
  isActive t = has (#status % #_Pending) t
