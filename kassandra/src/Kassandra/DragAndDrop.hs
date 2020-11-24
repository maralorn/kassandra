module Kassandra.DragAndDrop
  ( droppableElementConfig
  , childDropArea
  , taskDropArea
  , tellDragTask
  )
where

import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Kassandra.Sorting              ( SortPosition
                                                , saveSorting
                                                )
import           Kassandra.Types                ( AppStateChange
                                                , StandardWidget
                                                , TaskInfos
                                                , DragState(DraggedTask, NoDrag)
                                                , getTasks
                                                , getDragState
                                                , DataChange
                                                , WriteApp
                                                )
import           Kassandra.Util                 ( tellSingleton
                                                , lookupTask
                                                )
import           Kassandra.Debug                ( logRShow
                                                , Severity(..)
                                                )

tellDragTask :: (MonadIO m, WriteApp t m e) => R.Event t (Maybe UUID) -> m ()
tellDragTask =
  tellSingleton
    .   fmap
          ( (_Typed @AppStateChange % _Typed @DragState #)
          . maybe NoDrag DraggedTask
          )
    <=< logRShow Info

taskDropArea
  :: StandardWidget t m r e
  => R.Dynamic t [UUID]
  -> m ()
  -> (R.Event t TaskInfos -> R.Event t [Task])
  -> m ()
taskDropArea blacklistD areaW handler = do
  tasksD     <- getTasks
  dragStateD <- getDragState
  let dropActive = do
        dragState <- dragStateD
        blacklist <- blacklistD
        let retval
              | DraggedTask draggedUuid <- dragState
              , draggedUuid `notElem` blacklist
              = Just draggedUuid
              | otherwise
              = Nothing
        pure retval
  D.dyn_ $ dropActive <&> \case
    Just draggedUuid -> do
      dropEl <- fmap fst <$> D.element "span" D.def $ areaW
      let event = D.domEvent D.Click dropEl
      tellSingleton $ (_Typed @AppStateChange % _Typed # NoDrag) <$ event
      let droppedTaskEvent = R.attachWithMaybe
            (const . flip lookupTask draggedUuid)
            (R.current tasksD)
            event
      R.tellEvent
        $   fmap (_Typed @AppStateChange % _Typed @DataChange % #_ChangeTask #)
        <$> R.fmapMaybe nonEmpty (handler droppedTaskEvent)
    Nothing -> pass

childDropArea
  :: StandardWidget t m r e
  => SortPosition t
  -> R.Dynamic t [UUID]
  -> m ()
  -> m ()
childDropArea pos blacklistD areaW =
  taskDropArea blacklistD areaW
    $ saveSorting (pos ^. #mode) (pos ^. #list)
    . R.attachWith (\u t -> (t ^. #task, u)) (pos ^. #before)


droppableElementConfig
  :: forall s d
   . (R.Reflex s, D.DomSpace d)
  => (D.ElementConfig D.EventResult s d)
droppableElementConfig =
  lensVL D.elementConfig_eventSpec
    %~ D.addEventSpecFlags (Proxy :: Proxy d)
                           D.Dragover
                           (const D.preventDefault)
    $  (D.def :: (D.ElementConfig D.EventResult s d))
