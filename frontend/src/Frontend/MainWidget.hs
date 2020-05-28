{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex                        as R
import           Taskwarrior.IO                 ( createTask )
import           Frontend.Types                 ( al
                                                , fl
                                                , TaskInfos(TaskInfos)
                                                , getAppState
                                                , TaskTreeState
                                                , TaskTreeWidget
                                                , TaskTreeStateChange
                                                , ToggleEvent(ToggleEvent)
                                                , DragState(NoDrag)
                                                , AppState(AppState)
                                                , FilterState(FilterState)
                                                , TaskInfos
                                                , AppStateChange
                                                , WidgetIO
                                                , StandardWidget
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Debug.Trace                   as Trace
import           Control.Concurrent
import           Reflex.Network


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

mainWidget :: WidgetIO t m => m ()
mainWidget = do
  ref           <- liftIO $ newIORef 0
  (e, eTrigger) <- R.newTriggerEvent
  time          <- liftIO getZonedTime
  void $ liftIO $ forkIO $ do
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
          dragDyn   = R.constDyn NoDrag
      (_, stateChanges :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT $ runReaderT
          (taskList (R.constDyn [0 .. bugFactor]) (R.constDyn []) taskTreeWidget
          )
          (AppState taskState timeDyn dragDyn filterState)
  pass

taskList
  :: StandardWidget t m r e
  => R.Dynamic t [Int]
  -> R.Dynamic t [UUID]
  -> (R.Dynamic t Int -> m ())
  -> m ()
taskList childrenD blacklistD elementWidget = do
  void
    $ R.simpleList ((\xs -> zip xs (Nothing : fmap Just xs)) <$> childrenD)
    $ \childD -> elementWidget $ childD ^. fl _1

taskTreeWidget
  :: forall t m r e . StandardWidget t m r e => R.Dynamic t Int -> m ()
taskTreeWidget taskInfosD = do
  (appState :: AppState t) <- getAppState
  rec treeState <- R.foldDyn
        (flip $ foldr
          (\case
            ToggleEvent uuid False -> id
            ToggleEvent uuid True  -> id
          ) :: NonEmpty ToggleEvent -> HashSet UUID -> HashSet UUID
        )
        mempty
        treeStateChanges
      (_, events :: R.Event t (NonEmpty TaskTreeStateChange)) <-
        R.runEventWriterT
          $ runReaderT (taskWidget taskInfosD) (appState, treeState)
      let (appStateChanges, treeStateChanges) =
            R.fanThese $ partitionEithersNE <$> events
  R.tellEvent (fmap (_Typed #) <$> appStateChanges)

taskWidget
  :: forall t m r e . (TaskTreeWidget t m r e) => R.Dynamic t Int -> m ()
taskWidget taskInfos' = do
  task <- liftIO $ createTask "TestTask"
  let taskInfosD = R.constDyn $ TaskInfos task [] [] [] False
  appState  <- getAppState :: m (AppState t)
  treeState <- ask ^. al (typed @(TaskTreeState t))
  networkView $ taskInfosD <&> \taskInfos ->
    runReaderT widgets (appState, taskInfos, treeState)
  pass
 where
  widgets :: ReaderT (AppState t, TaskInfos, TaskTreeState t) m ()
  widgets = do
    (state, _, _) <- ask
    void $ networkView $ const pass <$> state ^. #currentTime
