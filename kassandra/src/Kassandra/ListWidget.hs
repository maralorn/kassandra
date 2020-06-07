
module Kassandra.ListWidget
  ( listsWidget
  , listWidget
  , TaskList(UUIDList, TagList)
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Kassandra.Types                 ( al
                                                , StandardWidget
                                                , TaskInfos
                                                , TaskState
                                                , Widget
                                                , getTasks
                                                )
import           Kassandra.Util                  ( filterCurrent )
import           Kassandra.TaskWidget            ( taskList
                                                , taskTreeWidget
                                                )
import           Kassandra.Sorting               ( sortTasks
                                                , SortMode(SortModeTag)
                                                )

data TaskList = TagList Text | SubList [TaskList] | UUIDList [UUID] deriving (Eq, Show, Read)

listsWidget :: (StandardWidget t m r e) => m ()
listsWidget = do
  taskState <- getTasks
  D.text "Select a list"
  list <- listSelector (getLists <$> taskState)
  listWidget list
 where
  getLists :: TaskState -> [TaskList]
  getLists =
    fmap TagList
      . HashSet.toList
      . fold
      . fmap (^. (#tags % to HashSet.fromList))
      . filter (has $ #status % #_Pending)
      . (^. al #task)
      . HashMap.elems
  listSelector
    :: (Widget t m) => R.Dynamic t [TaskList] -> m (R.Dynamic t TaskList)
  listSelector lists = D.el "div" $ do
    buttons   <- D.dyn $ mapM listButton <$> lists
    buttonSum <- R.switchHold R.never $ R.leftmost <$> buttons
    R.holdDyn (SubList []) buttonSum
  listButton :: (Widget t m) => TaskList -> m (R.Event t TaskList)
  listButton list | TagList tag <- list = button tag
                  | otherwise           = button "Anonymous List"
   where
    button =
      fmap ((list <$) . D.domEvent D.Click . fst)
        . D.elClass' "a" "selector"
        . D.text

listWidget
  :: forall t m r e . StandardWidget t m r e => R.Dynamic t TaskList -> m ()
listWidget list = D.dyn_ (innerRenderList <$> list)
 where
  innerRenderList :: TaskList -> m ()
  innerRenderList list'
    | UUIDList uuids <- list'
    = do
      tasks <- getTasks
      void
        . D.simpleList
            ((\tasks' -> mapMaybe (`HashMap.lookup` tasks') uuids) <$> tasks)
        $ taskTreeWidget
    | TagList tag <- list'
    = do
      D.text tag
      tasks     <- getTasks
      showTasks <- filterCurrent $ tasksToShow tag <$> tasks
      let sortMode = SortModeTag tag
      taskList (R.constant sortMode)
               (sortTasks sortMode <$> showTasks)
               (R.constDyn [])
               taskTreeWidget
    | SubList sublists <- list'
    = void . D.simpleList (D.constDyn sublists) $ listWidget

  tasksToShow :: Text -> TaskState -> [TaskInfos]
  tasksToShow tag = mapMaybe maybePredicate . HashMap.elems
   where
    maybePredicate :: TaskInfos -> Maybe TaskInfos
    maybePredicate taskInfo =
      if inList taskInfo then Just taskInfo else Nothing
    inList :: TaskInfos -> Bool
    inList = (tag `elem`) . (^. #tags)
