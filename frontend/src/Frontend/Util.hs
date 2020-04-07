{-# LANGUAGE ScopedTypeVariables, MultiWayIf, LambdaCase, ViewPatterns, OverloadedLabels, QuasiQuotes, TemplateHaskell #-}
module Frontend.Util
  ( filterCurrent
  , tellSingleton
  , tellTask
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
import           Frontend.Types                 ( StandardWidget
                                                , TaskState
                                                , TaskInfos
                                                , FilterState
                                                  ( completedFade
                                                  , FilterState
                                                  , deletedFade
                                                  )
                                                , DataChange(ChangeTask)
                                                , getTasks
                                                , getTime
                                                , getFilterState
                                                )
tellTask :: StandardWidget t m r => R.Event t Task -> m ()
tellTask = tellSingleton . fmap (Right . ChangeTask)

tellSingleton
  :: (R.Reflex t, R.EventWriter t (NonEmpty event) m) => R.Event t event -> m ()
tellSingleton = R.tellEvent . fmap one

lookupTask :: TaskState -> UUID -> Maybe TaskInfos
lookupTask tasks = \uuid -> tasks ^. at uuid

lookupTasks :: TaskState -> [UUID] -> [TaskInfos]
lookupTasks tasks = mapMaybe (lookupTask tasks)

lookupTaskM
  :: StandardWidget t m r
  => R.Dynamic t UUID
  -> m (R.Dynamic t (Maybe TaskInfos))
lookupTaskM uuid = getTasks <&> \tasks -> R.zipDynWith lookupTask tasks uuid


lookupTasksM :: StandardWidget t m r => [UUID] -> m (R.Dynamic t [TaskInfos])
lookupTasksM = R.constDyn >>> lookupTasksDynM

lookupTasksDynM
  :: StandardWidget t m r => R.Dynamic t [UUID] -> m (R.Dynamic t [TaskInfos])
lookupTasksDynM uuids =
  getTasks <&> \tasks -> flip lookupTasks <$> uuids <*> tasks

lookupCurrent :: StandardWidget t m r => [UUID] -> m (R.Dynamic t [TaskInfos])
lookupCurrent = lookupTasksM >=> filterCurrent

lookupCurrentDyn
  :: StandardWidget t m r => R.Dynamic t [UUID] -> m (R.Dynamic t [TaskInfos])
lookupCurrentDyn = lookupTasksDynM >=> filterCurrent

filterCurrent
  :: (StandardWidget t m r)
  => R.Dynamic t [TaskInfos]
  -> m (R.Dynamic t [TaskInfos])
filterCurrent taskinfos = do
  time        <- fmap zonedTimeToUTC <$> getTime
  filterState <- getFilterState
  let thresholds = R.zipDynWith
        (\time' FilterState { completedFade, deletedFade } ->
          (addUTCTime (-deletedFade) time', addUTCTime (-completedFade) time')
        )
        time
        filterState
  R.holdUniqDyn $ R.zipDynWith (filter . filterTask) thresholds taskinfos

filterTask :: ((UTCTime, UTCTime) -> TaskInfos -> Bool)
filterTask (deletedThreshold, completedThreshold) ((^. #status) -> status)
  | Status.Deleted time <- status   = time >= deletedThreshold
  | Status.Completed time <- status = time >= completedThreshold
  | otherwise                       = True
