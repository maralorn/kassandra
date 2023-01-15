module Kassandra.Util (
  tellSingleton,
  tellTask,
  tellToggle,
  tellNewTask,
  lookupTask,
  lookupTaskM,
  lookupTasks,
  lookupTasksM,
  lookupTasksDynM,
  defDyn,
  defDynDyn,
  stillTodo,
) where

import Data.HashSet (member)
import Data.Witherable
import Kassandra.Types (
  AppStateChange,
  DataChange,
  HaveApp,
  StandardWidget,
  TaskInfos,
  TaskState,
  TaskTreeStateChange,
  TaskTreeWidget,
  ToggleEvent,
  Widget,
  WriteApp,
  getExpandedTasks,
  getTasks,
 )
import qualified Reflex as R
import qualified Reflex.Dom as D

stillTodo :: TaskInfos -> Bool
stillTodo = has (#status % #_Pending)

tellToggle :: TaskTreeWidget t m r e => R.Event t UUID -> m ()
tellToggle ev = do
  expandedTasks <- getExpandedTasks <&> view #current
  tellSingleton $
    (_Typed @TaskTreeStateChange % _Typed @ToggleEvent #)
      <$> R.attachWith
        (\tasks uuid -> #_ToggleEvent # (uuid, not $ member uuid tasks))
        expandedTasks
        ev

tellTask :: WriteApp t m e => R.Event t Task -> m ()
tellTask =
  tellSingleton
    . fmap (_Typed @AppStateChange % _Typed @DataChange % #_ChangeTask #)

tellNewTask :: WriteApp t m e => R.Event t (Text, Task -> Task) -> m ()
tellNewTask =
  tellSingleton
    . fmap (_Typed @AppStateChange % _Typed @DataChange % #_CreateTask #)

tellSingleton ::
  (R.Reflex t, R.EventWriter t (NESeq event) m) => R.Event t event -> m ()
tellSingleton = R.tellEvent . fmap one

lookupTask :: TaskState -> UUID -> Maybe TaskInfos
lookupTask tasks uuid = tasks ^. at uuid

lookupTasks :: Filterable f => TaskState -> f UUID -> f TaskInfos
lookupTasks tasks = mapMaybe (lookupTask tasks)

lookupTaskM ::
  StandardWidget t m r e =>
  R.Dynamic t UUID ->
  m (R.Dynamic t (Maybe TaskInfos))
lookupTaskM uuid = getTasks <&> \tasks -> R.zipDynWith lookupTask tasks uuid

lookupTasksM :: (Filterable f, HaveApp t m r) => f UUID -> m (R.Dynamic t (f TaskInfos))
lookupTasksM = R.constDyn >>> lookupTasksDynM

lookupTasksDynM ::
  (Filterable f, HaveApp t m r) => R.Dynamic t (f UUID) -> m (R.Dynamic t (f TaskInfos))
lookupTasksDynM uuids =
  getTasks <&> \tasks -> flip lookupTasks <$> uuids <*> tasks

defDyn :: Widget t m => a -> R.Dynamic t (m a) -> m (R.Dynamic t a)
defDyn defVal = R.holdDyn defVal <=< D.dyn

defDynDyn ::
  Widget t m =>
  R.Dynamic t a ->
  R.Dynamic t (m (R.Dynamic t a)) ->
  m (R.Dynamic t a)
defDynDyn defDynamic = fmap join . defDyn defDynamic
