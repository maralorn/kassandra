module Kassandra.State
  ( TaskProvider
  , makeStateProvider
  , StateProvider
  , AppContext
  , ClientSocket
  ) where

import qualified Data.Dependent.Map            as DMap
import qualified Data.GADT.Compare.TH          as TH
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.List.NonEmpty            as NonEmpty
import           Kassandra.Api                  ( SocketMessage
                                                  ( TaskUpdates
                                                  , UIConfigResponse
                                                  )
                                                , SocketRequest(ChangeTasks)
                                                )
import           Kassandra.Config               ( UIConfig )
import           Kassandra.Types                ( DataChange
                                                  ( ChangeTask
                                                  , CreateTask
                                                  )
                                                , TaskInfos(TaskInfos)
                                                , TaskState
                                                , WidgetIO
                                                )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Taskwarrior.IO                 ( createTask )


data FanTag a where
   ToggleEventTag ::FanTag (NonEmpty (UUID, Bool))
   ChangeTaskTag ::FanTag (NonEmpty Task)
   CreateTaskTag ::FanTag (NonEmpty (Text, Task -> Task))

TH.deriveGEq ''FanTag
TH.deriveGCompare ''FanTag

type TaskProvider t m
  = R.Event t (NonEmpty Task) -> m (R.Dynamic t (HashMap UUID Task))

getParents :: HashMap UUID Task -> UUID -> [UUID]
getParents tasks = go [] (\uuid -> (^. #partof) =<< tasks ^. at uuid)
 where
  go :: (Eq a, Show a) => [a] -> (a -> Maybe a) -> a -> [a]
  go accu f x | x `elem` accu    = []
              | Just next <- f x = next : go (x : accu) f next
              | otherwise        = []

type StateProvider t m
  = R.Event t (NonEmpty DataChange) -> m (R.Dynamic t TaskState)

type AppContext t m = (StateProvider t m, UIConfig)

type ClientSocket t m e
  =  R.Event t SocketRequest
  -> m (R.Dynamic t (Either e (R.Event t SocketMessage)))

makeStateProvider
  :: forall e t m
   . (Show e, WidgetIO t m)
  => ClientSocket t m e
  -> StateProvider t m
makeStateProvider clientSocket dataChangeEvents = do
  let fannedEvent :: FanTag a -> R.Event t a
      fannedEvent =
        R.select
          $   R.fan
          $   DMap.unionsWithKey proofAdd
          .   fmap mapToMap
          .   NonEmpty.toList
          <$> dataChangeEvents
      createTaskEvent = fannedEvent CreateTaskTag
      changeTaskEvent = fannedEvent ChangeTaskTag
  changesFromCreateEvents <- createToChangeEvent createTaskEvent
  let localChanges = changeTaskEvent <> changesFromCreateEvents
  socketMessages <- clientSocket (ChangeTasks <$> localChanges)
  D.dynText (either show (const mempty) <$> socketMessages)
  let msgs          = R.switchDyn $ fromRight R.never <$> socketMessages
      remoteChanges = R.fmapMaybe messageToChange msgs
      messageToChange :: SocketMessage -> Maybe (NonEmpty Task)
      messageToChange (TaskUpdates      a) = Just a
      messageToChange (UIConfigResponse _) = Nothing
      -- TODO: What happens with UIConfigResponses?
  fmap buildTaskInfosMap <$> holdTasks (localChanges <> remoteChanges)

createToChangeEvent
  :: WidgetIO t m
  => D.Event t (NonEmpty (Text, Task -> Task))
  -> m (D.Event t (NonEmpty Task))
createToChangeEvent = R.performEvent . fmap
  (liftIO . mapM (\(desc, properties) -> properties <$> createTask desc))

holdTasks
  :: WidgetIO t m
  => R.Event t (NonEmpty Task)
  -> m (R.Dynamic t (HashMap UUID Task))
holdTasks = R.foldDyn foldTasks mempty

foldTasks :: Foldable t => t Task -> HashMap UUID Task -> HashMap UUID Task
foldTasks = flip (foldr (\task -> HashMap.insert (task ^. #uuid) task))

mapToMap :: DataChange -> DMap.DMap FanTag Identity
mapToMap = \case
  ChangeTask a   -> DMap.singleton ChangeTaskTag (Identity $ pure a)
  CreateTask a b -> DMap.singleton CreateTaskTag (Identity $ pure (a, b))

proofAdd :: FanTag a -> Identity a -> Identity a -> Identity a
proofAdd = liftA2 . \case
  ToggleEventTag -> (<>)
  ChangeTaskTag  -> (<>)
  CreateTaskTag  -> (<>)

buildChildrenMap :: HashMap a Task -> HashMap UUID [a]
buildChildrenMap =
  HashMap.fromListWith (++)
    . mapMaybe (\(uuid, task) -> (, pure uuid) <$> task ^. #partof)
    . HashMap.toList

buildDependenciesMap :: HashMap a Task -> HashMap UUID [a]
buildDependenciesMap =
  HashMap.fromListWith (++)
    . (   HashMap.toList
      >=> \(uuid, task) -> (, pure uuid) <$> toList (task ^. #depends)
      )

buildTaskInfosMap :: HashMap UUID Task -> TaskState
buildTaskInfosMap tasks = HashMap.mapWithKey
  (\u t -> TaskInfos t
                     (HashMap.lookupDefault [] u childrenMap)
                     (getParentTasks u)
                     (HashMap.lookupDefault [] u dependenciesMap)
                     (isBlockedTask t)
  )
  tasks
 where
  isBlockedTask   = isBlocked tasks
  getParentTasks  = getParents tasks
  dependenciesMap = buildDependenciesMap tasks
  childrenMap     = buildChildrenMap tasks

isBlocked :: HashMap UUID Task -> Task -> Bool
isBlocked tasks task =
  any (\t -> has (#status % #_Pending) t || has (#status % #_Waiting) t)
    .  mapMaybe (`HashMap.lookup` tasks)
    .  toList
    $  task
    ^. #depends
