{-# LANGUAGE PatternSynonyms #-}
module Frontend.TaskWidget
  ( 
  )
where

import qualified Data.Text                     as Text
import qualified Data.HashSet                  as HashSet
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.UDA                ( UDA )
import           Frontend.Util                  ( tellNewTask
                                                , lookupCurrentDyn
                                                , lookupCurrent
                                                , lookupTasksM
                                                , tellTask
                                                , tellToggle
                                                )
import           Frontend.Types                 ( TaskInfos(..)
                                                , getExpandedTasks
                                                , getIsExpanded
                                                , ToggleEvent(ToggleEvent)
                                                , TaskTreeState
                                                , Have
                                                , TaskTreeStateChange
                                                , TaskTreeWidget
                                                , getTime
                                                , StandardWidget
                                                , TaskInfos
                                                , AppState
                                                , getAppState
                                                , al
                                                , fl
                                                )
import           Frontend.TextEditWidget        ( lineWidget
                                                , createTextWidget
                                                )
import           Frontend.BaseWidgets           ( button
                                                , icon
                                                )
import           Frontend.Sorting               ( sortTasks
                                                , SortMode(SortModePartof)
                                                , SortPosition(SortPosition)
                                                )
import           Common.Debug                   ( log
                                                , pattern D
                                                )
import Reflex.Network

type TaskWidget t m r e = (TaskTreeWidget t m r e, HaveTask m r)
type HaveTask m r = Have m r TaskInfos

instance LabelOptic "taskInfos" A_Lens (a, TaskInfos) (a, TaskInfos) TaskInfos TaskInfos where
  labelOptic = _2
