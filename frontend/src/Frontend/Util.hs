{-# LANGUAGE TypeApplications, ScopedTypeVariables, MultiWayIf,  ViewPatterns, OverloadedLabels  #-}
module Frontend.Util
  ( filterCurrent
  , tellSingleton
  , tellTask
  , tellToggle
  , tellNewTask
  , lookupTask
  , lookupTaskM
  , lookupTasks
  , lookupTasksM
  , lookupCurrentDyn
  , lookupCurrent
  )
where

import qualified Reflex                        as R
import           Taskwarrior.Status            as Status
import           Frontend.Types                 ( ToggleEvent
                                                , getExpandedTasks
                                                , AppStateChange
                                                , StandardWidget
                                                , TaskState
                                                , TaskInfos
                                                , HaveApp
                                                , WriteApp
                                                , DataChange
                                                , TaskTreeWidget
                                                , TaskTreeStateChange
                                                , Widget
                                                , getTasks
                                                , getTime
                                                , getFilterState
                                                )
import           Data.HashSet                   ( member )

tellToggle :: TaskTreeWidget t m r e => R.Event t UUID -> m ()
tellToggle ev = do
  expandedTasks <- getExpandedTasks <&> view #current
  tellSingleton
    $   (_Typed @TaskTreeStateChange % _Typed @ToggleEvent #)
    <$> R.attachWith
          (\tasks uuid -> #_ToggleEvent # (uuid, not $ member uuid tasks))
          expandedTasks
          ev

tellTask :: WriteApp t m e => R.Event t Task -> m ()
tellTask = tellSingleton
  . fmap (_Typed @AppStateChange % _Typed @DataChange % #_ChangeTask #)

tellNewTask :: WriteApp t m e => R.Event t (Text, Task -> Task) -> m ()
tellNewTask = tellSingleton
  . fmap (_Typed @AppStateChange % _Typed @DataChange % #_CreateTask #)

tellSingleton
  :: (R.Reflex t, R.EventWriter t (NonEmpty event) m) => R.Event t event -> m ()
tellSingleton = R.tellEvent . fmap one

lookupTask :: TaskState -> UUID -> Maybe TaskInfos
lookupTask tasks uuid = tasks ^. at uuid

lookupTasks :: TaskState -> [UUID] -> [TaskInfos]
lookupTasks tasks = mapMaybe (lookupTask tasks)

lookupTaskM
  :: StandardWidget t m r e
  => R.Dynamic t UUID
  -> m (R.Dynamic t (Maybe TaskInfos))
lookupTaskM uuid = getTasks <&> \tasks -> R.zipDynWith lookupTask tasks uuid


lookupTasksM :: HaveApp t m r => [UUID] -> m (R.Dynamic t [TaskInfos])
lookupTasksM = R.constDyn >>> lookupTasksDynM

lookupTasksDynM
  :: HaveApp t m r => R.Dynamic t [UUID] -> m (R.Dynamic t [TaskInfos])
lookupTasksDynM uuids =
  getTasks <&> \tasks -> flip lookupTasks <$> uuids <*> tasks

lookupCurrent
  :: (Widget t m, HaveApp t m r) => [UUID] -> m (R.Dynamic t [TaskInfos])
lookupCurrent = lookupTasksM >=> filterCurrent

lookupCurrentDyn
  :: (Widget t m, HaveApp t m r)
  => R.Dynamic t [UUID]
  -> m (R.Dynamic t [TaskInfos])
lookupCurrentDyn = lookupTasksDynM >=> filterCurrent

filterCurrent
  :: (Widget t m, HaveApp t m r)
  => R.Dynamic t [TaskInfos]
  -> m (R.Dynamic t [TaskInfos])
filterCurrent taskinfos = do
  time         <- fmap zonedTimeToUTC <$> getTime
  filterStateD <- getFilterState
  let thresholds = R.zipDynWith
        (\time' filterState ->
          ( addUTCTime (-(filterState ^. #deletedFade))   time'
          , addUTCTime (-(filterState ^. #completedFade)) time'
          )
        )
        time
        filterStateD
  R.holdUniqDyn $ R.zipDynWith (filter . filterTask) thresholds taskinfos

filterTask :: ((UTCTime, UTCTime) -> TaskInfos -> Bool)
filterTask (deletedThreshold, completedThreshold) ((^. #status) -> status)
  | Status.Deleted time <- status   = time >= deletedThreshold
  | Status.Completed time <- status = time >= completedThreshold
  | otherwise                       = True
