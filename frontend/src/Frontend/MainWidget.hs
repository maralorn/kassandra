{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import           Frontend.Types                 ( DragState(NoDrag)
                                                , AppState(AppState)
                                                , FilterState(FilterState)
                                                , getTasks
                                                , TaskInfos
                                                , AppStateChange
                                                , WidgetIO
                                                , StandardWidget
                                                , TaskState
                                                )
import           Frontend.ListWidget            ( listsWidget
                                                , listWidget
                                                , TaskList(TagList)
                                                )
import           Frontend.State                 ( StateProvider )
import           Frontend.TaskWidget            ( taskTreeWidget )
import           Frontend.TextEditWidget        ( createTextWidget )
import           Frontend.BaseWidgets           ( button )
import           Frontend.Util                  ( tellNewTask )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Debug.Trace                   as Trace
import           Control.Concurrent



{-# NOINLINE incrementRef #-}
incrementRef :: (Show a) => IORef Int -> a -> String
incrementRef ref x = unsafePerformIO
  (atomicModifyIORef
    ref
    (\a -> (a + 1, show x <> " triggered time: " <> show (a + 1)))
  )

countTriggers
  :: (R.MonadSample t (R.PullM t), R.Reflex t, Show a)
  => IORef Int
  -> R.Dynamic t a
  -> R.Dynamic t a
countTriggers ref d =
  let e'    = R.traceEventWith (incrementRef ref) $ R.updated d
      getV0 = do
        x <- R.sample $ R.current d
        Trace.trace ("initialized Counter") $ return x
  in  R.unsafeBuildDynamic getV0 e'

mainWidget :: WidgetIO t m => StateProvider t m -> m ()
mainWidget stateProvider = do
  ref           <- liftIO $ newIORef 0
  (e, eTrigger) <- R.newTriggerEvent
  liftIO $ forkIO $ do
    threadDelay 1000000
    time <- liftIO getZonedTime
    eTrigger time
    threadDelay 1000000
    count <- readIORef ref
    if count == 1
      then do
        putStrLn
          "It appears there is no bug here! Trigger was counted exactly once!"
        exitSuccess
      else do
        putStrLn $ "Bug! Triggers counted: " <> show count
        exitFailure

  D.divClass "header" $ D.text "Kassandra Taskmanagement"
  time    <- liftIO getZonedTime
  timeDyn <- countTriggers ref <$> R.holdDyn time e
--    fmap (utcToZonedTime (zonedTimeZone time) . (^. lensVL R.tickInfo_lastUTC))
--      <$> R.clockLossy 1 (zonedTimeToUTC time)
  let filterState = R.constDyn (FilterState 0 60)
  rec let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNE <$> stateChanges
          taskState = R.constDyn mempty --stateProvider dataChangeEvents
      dragDyn   <- R.holdDyn NoDrag $ last <$> appChangeEvents
      (_, stateChanges :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT $ runReaderT
          (do
            taskDiagnosticsWidget
            D.divClass "pane" (listWidget $ R.constDyn (TagList "root"))
          )
          (AppState taskState timeDyn dragDyn filterState)
  pass

taskDiagnosticsWidget :: (StandardWidget t m r e) => m ()
taskDiagnosticsWidget = do
  tasks <- getTasks
  D.dynText $ do
    tasksMap <- tasks
    let uuids = HashMap.keys tasksMap
        hasLoop :: [UUID] -> UUID -> Maybe UUID
        hasLoop seen new | new `elem` seen = Just new
                         | otherwise = firstJust (hasLoop (new : seen)) nexts
          where nexts = maybe [] (^. #children) $ HashMap.lookup new tasksMap
    pure $ firstJust (hasLoop []) uuids & \case
      Just uuid -> "Found a loop for uuid " <> show uuid
      Nothing   -> "" -- everything fine
