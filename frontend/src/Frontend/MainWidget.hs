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
                                                , TaskTreeStateChange
                                                , ToggleEvent(ToggleEvent)
                                                , DragState(NoDrag)
                                                , AppState(AppState)
                                                , FilterState(FilterState)
                                                , AppStateChange
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

type Have m r s = (MonadReader r m, HasType s r)
type HaveApp t m r = (R.Reflex t, Have m r (AppState t))
type HaveTaskTree t m r = (Have m r (TaskTreeState t))
type Write t m e s = (R.Reflex t, R.EventWriter t (NonEmpty e) m, AsType s e)
type WriteApp t m e = (Write t m e AppStateChange)
type WriteTaskTree t m e = (Write t m e TaskTreeStateChange)
type StandardWidget t m r e
  = ( HaveApp t m r
    , WriteApp t m e
    , MonadFix m
    , R.MonadHold t m
    , R.PostBuild t m
    , MonadIO m
    , R.TriggerEvent t m
    , R.PerformEvent t m
    , MonadIO (R.Performable m)
    , HasCallStack
    )
type TaskTreeWidget t m r e
  = (StandardWidget t m r e, HaveTaskTree t m r, WriteTaskTree t m e)


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
  void
    $ (R.runEventWriterT :: (  R.EventWriterT
            t
            (NonEmpty TaskTreeStateChange)
            m
            a
        -> m (a, R.Event t (NonEmpty TaskTreeStateChange))
        )
      )
    $
       (void $ networkView $ (appState ^. #currentTime) <&> \time -> pure ())
