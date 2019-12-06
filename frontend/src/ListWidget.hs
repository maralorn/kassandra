{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module ListWidget
  ( listsWidget
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Types
import qualified Data.Maybe                    as Maybe
import qualified Data.HashMap.Strict           as HashMap
import qualified Taskwarrior.Status            as Status
import           ClassyPrelude
import qualified Data.HashSet                  as HashSet
import qualified Taskwarrior.Task              as Task
import           Util                           ( partof )
import           TaskWidget

data TaskList = TagList Text | SubList [TaskList] deriving (Eq, Show, Read)

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
      . fmap (HashSet.fromList . Task.tags)
      . filter ((Status.Pending ==) . Task.status)
      . fmap task
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
    | TagList tag <- list'
    = do
      D.text tag
      tasks <- getTasks
      void . D.simpleList (tasksToShow tag <$> tasks) $ taskWidget
    | SubList sublists <- list'
    = void . D.simpleList (D.constDyn sublists) $ listWidget

  tasksToShow :: Text -> TaskState -> [TaskInfos]
  tasksToShow tag taskState =
    Maybe.mapMaybe maybePredicate . HashMap.elems $ taskState
   where
    maybePredicate :: TaskInfos -> Maybe TaskInfos
    maybePredicate taskInfo = if inList taskInfo && not (parentInList taskInfo)
      then Just taskInfo
      else Nothing
    inList :: TaskInfos -> Bool
    inList TaskInfos { task = Task.Task { tags } } = tag `elem` tags
    parentInList :: TaskInfos -> Bool
    parentInList TaskInfos { task } =
      maybe False inList (partof task >>= flip HashMap.lookup taskState)
