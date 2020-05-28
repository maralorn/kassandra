{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex                        as R
import           Frontend.Types                 ( al
                                                , fl
                                                , getAppState
                                                , TaskTreeState
                                                , TaskTreeWidget
                                                , TaskTreeStateChange
                                                , ToggleEvent(ToggleEvent)
                                                , DragState(NoDrag)
                                                , AppState(AppState)
                                                , FilterState(FilterState)
                                                , AppStateChange
                                                , StandardWidget
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
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
  :: (R.Reflex t, Show a) => IORef Int -> R.Dynamic t a -> R.Dynamic t a
countTriggers ref d =
  let e'    = R.traceEventWith (incrementRef ref) $ R.updated d
      getV0 = R.sample $ R.current d
  in  R.unsafeBuildDynamic getV0 e'


mainWidget
  :: forall m t
   . ( MonadFix m
     , R.MonadHold t m
     , R.PostBuild t m
     , MonadIO m
     , R.TriggerEvent t m
     , R.PerformEvent t m
     , R.NotReady t m
     , R.Adjustable t m
     , MonadIO (R.Performable m)
     , HasCallStack
     )
  => m (R.Event t ())
mainWidget = do
  ref                   <- liftIO $ newIORef 0
  (e    , eTrigger    ) <- R.newTriggerEvent
  (close, closeTrigger) <- R.newTriggerEvent
  time                  <- liftIO getZonedTime
  void $ liftIO $ forkIO $ do
    threadDelay 1000000
    eTrigger time
    threadDelay 1000000
    count <- readIORef ref
    if count == 1
      then do
        putStrLn
          "It appears there is no bug here! Trigger was counted exactly once!"
        closeTrigger ()
      else do
        putStrLn $ "Bug! Triggers counted: " <> show count
        exitFailure
  timeDyn <- countTriggers ref <$> R.holdDyn time e
  let filterState = R.constDyn (FilterState 0 60)
  let taskState = R.constDyn mempty --stateProvider dataChangeEvents
      dragDyn   = R.constDyn NoDrag
  void $ R.runEventWriterT $ runReaderT
    (taskList (R.constDyn [0 .. bugFactor]) (const taskTreeWidget) :: ( ReaderT
          (AppState t)
          (R.EventWriterT t (NonEmpty AppStateChange) m)
          ()
      )
    )
    (AppState taskState timeDyn dragDyn filterState :: AppState t)
  pure close

taskList
  :: (StandardWidget t m r e, R.Adjustable t m)
  => R.Dynamic t [Int]
  -> (R.Dynamic t Int -> m ())
  -> m ()
taskList childrenD elementWidget = do
  void
    $ R.simpleList ((\xs -> zip xs (Nothing : fmap Just xs)) <$> childrenD)
    $ \childD -> elementWidget $ childD ^. fl _1

taskTreeWidget
  :: forall t m r e
   . (StandardWidget t m r e, R.NotReady t m, R.Adjustable t m)
  => m ()
taskTreeWidget = do
  (appState :: AppState t) <- getAppState
  let treeState = R.constDyn mempty
  (_, events :: R.Event t (NonEmpty TaskTreeStateChange)) <-
    R.runEventWriterT $ runReaderT taskWidget (appState, treeState)
  let (appStateChanges, treeStateChanges) =
        R.fanThese $ partitionEithersNE <$> events
  R.tellEvent (fmap (_Typed #) <$> appStateChanges)

taskWidget
  :: forall t m r e
   . (R.NotReady t m, R.Adjustable t m, TaskTreeWidget t m r e)
  => m ()
taskWidget = do
  appState <- getAppState :: m (AppState t)
  let time = appState ^. #currentTime
  treeState <- ask ^. al (typed @(TaskTreeState t))
  void $ networkView $ time <&> \time ->
    runReaderT widgets (appState, R.constDyn time, treeState)
 where
  widgets = do
    (_, time, _) <- ask
    void $ networkView $ const pass <$> time
