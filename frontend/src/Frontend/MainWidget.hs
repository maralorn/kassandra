{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex                        as R
import           System.IO.Unsafe               ( unsafePerformIO )
import           Control.Concurrent
import           Reflex.Network


bugFactor :: Int
bugFactor = 1000

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
     , R.NotReady t m
     , R.Adjustable t m
     )
  => m (R.Event t ())
mainWidget = do
  ref                   <- liftIO $ newIORef 0
  (e    , eTrigger    ) <- R.newTriggerEvent
  (close, closeTrigger) <- R.newTriggerEvent
  void $ liftIO $ forkIO $ do
    threadDelay 1000000
    eTrigger ()
    threadDelay 1000000
    count <- readIORef ref
    if count == 1
      then do
        putStrLn
          "It appears there is no bug here! Trigger was counted exactly once!"
        closeTrigger ()
      else do
        putStrLn $ "Bug! Triggers counted: " <> show count
        closeTrigger ()
  timeDyn <- countTriggers ref <$> R.holdDyn () e
  void $ R.simpleList (R.constDyn [0 .. bugFactor]) $ const
    (void $ networkView $ pure () <$ timeDyn)
  pure close
