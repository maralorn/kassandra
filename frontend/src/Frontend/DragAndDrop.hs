{-# LANGUAGE ScopedTypeVariables, MultiWayIf, LambdaCase, ViewPatterns, OverloadedLabels, QuasiQuotes, TemplateHaskell #-}
module Frontend.DragAndDrop
  ( droppableElementConfig
  , childDropArea
  , taskDropArea
  )
where

import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Data.Proxy                     ( Proxy(Proxy) )
import           Frontend.Sorting               ( SortPosition
                                                , saveSorting
                                                )
import           Frontend.Types                 ( StandardWidget
                                                , TaskInfos
                                                , DragState(DraggedTask, NoDrag)
                                                , getTasks
                                                , getDragState
                                                , DataChange(ChangeTask)
                                                , AppChange(DragChange)
                                                )
import           Frontend.Util                  ( tellSingleton
                                                , lookupTask
                                                )

taskDropArea
  :: StandardWidget t m r
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
      dropEl <- fmap fst <$> D.element "span" droppableElementConfig $ areaW
      let event = D.domEvent D.Drop dropEl
      tellSingleton $ (Left . DragChange $ NoDrag) <$ event
      let droppedTaskEvent = R.attachWithMaybe
            (const . flip lookupTask draggedUuid)
            (R.current tasksD)
            event
      R.tellEvent
        $   fmap (Right . ChangeTask)
        <$> (R.fmapMaybe nonEmpty $ handler droppedTaskEvent)
    Nothing -> pass

childDropArea
  :: StandardWidget t m r
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
