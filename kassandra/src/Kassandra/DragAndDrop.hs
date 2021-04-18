module Kassandra.DragAndDrop (
  childDropArea,
  taskDropArea,
  tellSelected,
  insertArea,
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

tellSelected :: (MonadIO m, WriteApp t m e) => R.Event t (Seq DefinitionElement) -> m ()
tellSelected = tellSingleton . fmap (_Typed @AppStateChange % _Typed @SelectState #) <=< logRShow Info

insertArea :: StandardWidget t m r e => R.Dynamic t (Seq DefinitionElement) -> m () -> m (R.Event t (NESeq DefinitionElement))
insertArea blacklistD areaW = do
  selectStateD <- getSelectState
  let dropActive = do
        selectState <- selectStateD
        blacklist <- blacklistD
        pure $ if all (`notElem` blacklist) selectState then nonEmptySeq selectState else Nothing
  evEv <-
    D.dyn $
      dropActive <&> \case
        Just entry -> do
          dropEl <- fmap fst <$> D.element "span" D.def $ areaW
          let event = D.domEvent D.Click dropEl
          tellSelected (mempty <$ event)
          pure $ entry <$ event
        Nothing -> pure R.never
  R.switchHold R.never evEv

taskDropArea ::
  StandardWidget t m r e =>
  R.Dynamic t (Seq UUID) ->
  m () ->
  (R.Event t (NESeq TaskInfos) -> R.Event t (NESeq Task)) ->
  m ()
taskDropArea blacklistD areaW handler = do
  tasksD <- getTasks
  let blackListDefinitionElements = (#_ListElement % #_TaskwarriorTask #) <<$>> blacklistD
  insertEvent <- insertArea blackListDefinitionElements areaW
  let droppedTaskEvent =
        R.attachWithMaybe
          (\tasks -> nonEmptySeq . mapMaybe (lookupTask tasks <=< (^? #_ListElement % #_TaskwarriorTask)) . toSeq)
          (R.current tasksD)
          insertEvent
  R.tellEvent $
    fmap (_Typed @AppStateChange % _Typed @DataChange % #_ChangeTask #)
      <$> handler droppedTaskEvent

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
