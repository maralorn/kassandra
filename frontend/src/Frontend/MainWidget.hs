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
import           Frontend.State                 ( StateProvider )
import           Frontend.TaskWidget            ( taskTreeWidget, taskList )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Debug.Trace                   as Trace
import           Control.Concurrent


bugFactor :: Int
bugFactor = 100

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
  time    <- liftIO getZonedTime
  liftIO $ forkIO $ do
    threadDelay 1000000
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
  timeDyn <- countTriggers ref <$> R.holdDyn time e
  let filterState = R.constDyn (FilterState 0 60)
  rec let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNE <$> stateChanges
          taskState = R.constDyn mempty --stateProvider dataChangeEvents
          dragDyn = R.constDyn NoDrag
      (_, stateChanges :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT $ runReaderT
          (taskList
                    (R.constDyn [0 .. bugFactor])
                    (R.constDyn [])
                    taskTreeWidget
          )
          (AppState taskState timeDyn dragDyn filterState)
  pass
