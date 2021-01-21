module Kassandra.DragAndDrop
  ( droppableElementConfig
  , childDropArea
  , taskDropArea
  , tellDragTask
  ) where

import           Kassandra.Debug                ( Severity(..)
                                                , logRShow
                                                )
import           Kassandra.Sorting              ( SortPosition
                                                , saveSorting
                                                )
import           Kassandra.Types                ( AppStateChange
                                                , DataChange
                                                , DragState
                                                  ( DraggedTasks
                                                  , NoDrag
                                                  )
                                                , StandardWidget
                                                , TaskInfos
                                                , WriteApp
                                                , getDragState
                                                , getTasks
                                                )
import           Kassandra.Util                 ( lookupTask
                                                , tellSingleton
                                                )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D

tellDragTask :: (MonadIO m, WriteApp t m e) => R.Event t [UUID] -> m ()
tellDragTask =
  tellSingleton
    .   fmap
          ( (_Typed @AppStateChange % _Typed @DragState #)
          . maybe NoDrag DraggedTasks
          . nonEmpty
          )
    <=< logRShow Info

taskDropArea
  :: StandardWidget t m r e
  => R.Dynamic t [UUID]
  -> m ()
  -> (R.Event t (NonEmpty TaskInfos) -> R.Event t (NonEmpty Task))
  -> m ()
taskDropArea blacklistD areaW handler = do
  tasksD     <- getTasks
  dragStateD <- getDragState
  let dropActive = do
        dragState <- dragStateD
        blacklist <- blacklistD
        let retval
              | DraggedTasks draggedUuid <- dragState
              , all (`notElem` blacklist) draggedUuid
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
            (\tasks -> const . sequence $ lookupTask tasks <$> draggedUuid)
            (R.current tasksD)
            event
      R.tellEvent
        $   fmap (_Typed @AppStateChange % _Typed @DataChange % #_ChangeTask #)
        <$> handler droppedTaskEvent
    Nothing -> pass

childDropArea
  :: StandardWidget t m r e
  => SortPosition t
  -> R.Dynamic t [UUID]
  -> m ()
  -> m ()
childDropArea pos blacklistD areaW =
  taskDropArea blacklistD areaW $ saveSorting (pos ^. #mode) (pos ^. #list)
    . R.attachWith (\u t -> ((^. #task) <$> t, u)) (pos ^. #before)

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
