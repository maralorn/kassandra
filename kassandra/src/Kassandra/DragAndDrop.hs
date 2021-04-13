module Kassandra.DragAndDrop (
  childDropArea,
  taskDropArea,
  tellSelectedTasks,
) where

import Kassandra.Config (DefinitionElement)
import Kassandra.Debug (
  Severity (..),
  logRShow,
 )
import Kassandra.Sorting (
  SortPosition,
  saveSorting,
 )
import Kassandra.Types (
  AppStateChange,
  DataChange,
  SelectState,
  StandardWidget,
  TaskInfos,
  WriteApp,
  getSelectState,
  getTasks,
 )
import Kassandra.Util (
  lookupTask,
  tellSingleton,
 )
import qualified Reflex as R
import qualified Reflex.Dom as D

tellSelectedTasks :: (MonadIO m, WriteApp t m e) => R.Event t (Seq DefinitionElement) -> m ()
tellSelectedTasks =
  tellSingleton . fmap (_Typed @AppStateChange % _Typed @SelectState #)
    <=< logRShow Info

taskDropArea ::
  StandardWidget t m r e =>
  R.Dynamic t (Seq UUID) ->
  m () ->
  (R.Event t (NESeq TaskInfos) -> R.Event t (NESeq Task)) ->
  m ()
taskDropArea blacklistD areaW handler = do
  tasksD <- getTasks
  selectStateD <- getSelectState
  let dropActive = do
        selectState <- selectStateD
        let selectedTasks = forM selectState (^? #_ListElement % #_TaskwarriorTask)
        blacklist <- blacklistD
        pure $ selectedTasks >>= \uuids -> if all (`notElem` blacklist) uuids then nonEmptySeq uuids else Nothing
  D.dyn_ $
    dropActive <&> \case
      Just draggedUuid -> do
        dropEl <- fmap fst <$> D.element "span" D.def $ areaW
        let event = D.domEvent D.Click dropEl
        tellSelectedTasks (mempty <$ event)
        let droppedTaskEvent =
              R.attachWithMaybe
                (\tasks -> const . sequence $ lookupTask tasks <$> draggedUuid)
                (R.current tasksD)
                event
        R.tellEvent $
          fmap (_Typed @AppStateChange % _Typed @DataChange % #_ChangeTask #)
            <$> handler droppedTaskEvent
      Nothing -> pass

childDropArea ::
  StandardWidget t m r e =>
  SortPosition t ->
  R.Dynamic t (Seq UUID) ->
  m () ->
  m ()
childDropArea pos blacklistD areaW =
  taskDropArea blacklistD areaW $
    saveSorting (pos ^. #mode) (pos ^. #list)
      . R.attachWith (\u t -> ((^. #task) <$> t, u)) (pos ^. #before)
