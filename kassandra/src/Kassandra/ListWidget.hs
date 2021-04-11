module Kassandra.ListWidget (
  listsWidget,
  listWidget,
  TaskList (UUIDList, TagList),
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Kassandra.BaseWidgets (button)
import Kassandra.Sorting (
  SortMode (SortModeTag),
  sortTasks,
 )
import Kassandra.TaskWidget (
  taskList,
  taskTreeWidget,
 )
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types (
  StandardWidget,
  TaskInfos,
  TaskState,
  Widget,
  getTasks,
 )
import Kassandra.Util (tellNewTask)
import qualified Reflex as R
import qualified Reflex.Dom as D

data TaskList = TagList Text | SubList [TaskList] | UUIDList [UUID] deriving stock (Eq, Show, Read)

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
      . toList
      . foldMap (^. #tags)
      . filter (has $ #status % #_Pending)
      . (^. mapping #task)
      . HashMap.elems
  listSelector ::
    (Widget t m) => R.Dynamic t [TaskList] -> m (R.Dynamic t TaskList)
  listSelector lists = D.el "div" $ do
    buttons <- D.dyn $ mapM listButton <$> lists
    buttonSum <- R.switchHold R.never $ R.leftmost <$> buttons
    R.holdDyn (SubList []) buttonSum
  listButton :: (Widget t m) => TaskList -> m (R.Event t TaskList)
  listButton list
    | TagList tag <- list = localButton tag
    | otherwise = localButton "Anonymous List"
   where
    localButton =
      fmap ((list <$) . D.domEvent D.Click . fst)
        . D.elClass' "a" "selector"
        . D.text

listWidget ::
  forall t m r e. StandardWidget t m r e => R.Dynamic t TaskList -> m ()
listWidget list = D.dyn_ (innerRenderList <$> list)
 where
  innerRenderList :: TaskList -> m ()
  innerRenderList list'
    | UUIDList uuids <- list' =
      do
        tasks <- getTasks
        void
          . D.simpleList
            ((\tasks' -> mapMaybe (`HashMap.lookup` tasks') uuids) <$> tasks)
          $ taskTreeWidget
    | TagList tag <- list' =
      do
        D.text tag
        tasks <- getTasks
        let showTasks = tasksToShow tag <$> tasks
        let sortMode = SortModeTag tag
        taskList
          (R.constant sortMode)
          (sortTasks sortMode <$> showTasks)
          (R.constDyn IsEmpty)
          taskTreeWidget
        tellNewTask . fmap (,#tags %~ Set.insert tag)
          =<< createTextWidget
            (button "selector" $ D.text "Add task to list")
    | SubList sublists <- list' =
      void . D.simpleList (D.constDyn sublists) $ listWidget

  tasksToShow :: Text -> TaskState -> Seq TaskInfos
  tasksToShow tag = filter inList . fromList . HashMap.elems
   where
    inList :: TaskInfos -> Bool
    inList ((^. #task) -> task) = tag `Set.member` (task ^. #tags) && has (#status % #_Pending) task
