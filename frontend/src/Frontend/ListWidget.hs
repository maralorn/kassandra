
module Frontend.ListWidget
  ( 
   listWidget
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Frontend.Types                 ( al
                                                , StandardWidget
                                                , TaskInfos
                                                , TaskState
                                                , Widget
                                                , getTasks
                                                )
import           Frontend.Util                  ( filterCurrent )
import           Frontend.TaskWidget            ( taskList
                                                , taskTreeWidget
                                                )
import           Frontend.Sorting               ( sortTasks
                                                , SortMode(SortModeTag)
                                                )


listWidget
  :: forall t m r e . StandardWidget t m r e => m ()
listWidget =  do
      let sortMode = SortModeTag ""
      taskList (R.constant sortMode)
               (R.constDyn [0..5])
               (R.constDyn [])
               taskTreeWidget
