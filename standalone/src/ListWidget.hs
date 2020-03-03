{-# LANGUAGE ScopedTypeVariables, OverloadedLabels #-}
module ListWidget
  ( listsWidget
  , listWidget
  , TaskList(UUIDList)
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import qualified Taskwarrior.Status            as Status
import qualified Data.HashSet                  as HashSet
import           Data.UUID                      ( UUID )
import           Types
import           Util
import           TaskWidget

data TaskList = TagList Text | SubList [TaskList] | UUIDList [UUID] deriving (Eq, Show, Read)

listsWidget :: (StandardWidget t m r) => m ()
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
      . fmap (HashSet.fromList . (^. #tags))
      . filter ((Status.Pending ==) . (^. #status))
      . (^. #task)
      . HashMap.elems
  listSelector
    :: (Widget t m) => R.Dynamic t [TaskList] -> m (R.Dynamic t TaskList)
  listSelector lists = do
    buttons   <- D.dyn $ mapM listButton <$> lists
    buttonSum <- R.switchHold R.never $ R.leftmost <$> buttons
    R.holdDyn (SubList []) buttonSum
  listButton :: (Widget t m) => TaskList -> m (R.Event t TaskList)
  listButton list | TagList tag <- list = (const list <$>) <$> D.button tag
                  | otherwise = (const list <$>) <$> D.button "Anonymous List"

listWidget
  :: forall t m r . (StandardWidget t m r) => R.Dynamic t TaskList -> m ()
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
        $ taskWidget
    | TagList tag <- list'
    = do
      D.text tag
      tasks     <- getTasks
      showTasks <- filterCurrent $ tasksToShow tag <$> tasks
      let sortMode = SortModeTag tag
      taskList (R.constant sortMode)
               (sortTasks sortMode <$> showTasks)
               (R.constDyn [])
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
