{-# LANGUAGE TypeApplications, TupleSections, FlexibleContexts, ConstraintKinds, StandaloneDeriving, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, GADTs, TemplateHaskell, OverloadedLabels, ViewPatterns #-}
module State
  ( stateProvider
  )
where

import           Taskwarrior.Task               ( Task )
import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                , createTask
                                                )
import           Data.UUID                      ( UUID )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import           Data.String.Interpolate        ( i )
import qualified Network.Simple.TCP            as Net
import           Control.Concurrent             ( forkIO )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Dependent.Map            as DMap
import qualified Data.GADT.Compare.TH          as TH
import           Control.Exception              ( IOException
                                                , catch
                                                )
import           Types                   hiding ( getTasks )

data FanTag a where
   ToggleEventTag ::FanTag (NonEmpty (UUID, Bool))
   ChangeTaskTag ::FanTag (NonEmpty Task)
   CreateTaskTag ::FanTag (NonEmpty (Text, Task -> Task))

TH.deriveGEq ''FanTag
TH.deriveGCompare ''FanTag

newtype Cache = Cache { collapseState :: HashMap UUID Bool } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

type WidgetIO t m
  = ( MonadIO m
    , Widget t m
    , R.TriggerEvent t m
    , R.PerformEvent t m
    , MonadIO (R.Performable m)
    )
cacheProvider
  :: (WidgetIO t m)
  => R.Event t (NonEmpty (UUID, Bool))
  -> m (R.Dynamic t Cache)
cacheProvider toggleEvent = do
  firstCache <- liftIO getCache
  rec cache         <- R.holdDyn firstCache newCacheEvent
      newCacheEvent <- R.performEventAsync $ R.attachWith
        (\Cache { collapseState } newPairs cb -> liftIO $ do
          let newCache = Cache $ foldr
                (\(key, value) object -> HashMap.insert key value object)
                collapseState
                newPairs
          void . forkIO $ Aeson.encodeFile cacheFile newCache
          cb newCache
        )
        (R.current cache)
        toggleEvent
  pure cache

taskProvider
  :: (WidgetIO t m)
  => R.Event t (NonEmpty Task)
  -> m (R.Dynamic t (HashMap UUID Task))
taskProvider changeTaskEvent = do
  void
    .   R.performEvent
    $   liftIO
    .   saveTasks
    .   NonEmpty.toList
    <$> changeTaskEvent
  (tasksEvent :: R.Event t (NonEmpty Task), newTasksCallBack) <-
    R.newTriggerEvent
  tasks <- R.foldDyn (flip . foldr . join $ HashMap.insert . (^. #uuid))
                     HashMap.empty
                     (changeTaskEvent <> tasksEvent)
  liftIO $ do
    void . forkIO $ getTasks [] >>= maybe pass newTasksCallBack . nonEmpty
    putStrLn "Listening for changed or new tasks on 127.0.0.1:6545."
    void . forkIO $ Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) ->
      Net.recv socket 4096 >>= maybe
        (putStrLn "Unsuccessful connection attempt.")
        (\changes ->
          either
              (\err -> putStrLn [i|Couldn‘t decode #{changes} as Task: #{err}|])
              (newTasksCallBack . pure)
            $ Aeson.eitherDecodeStrict @Task changes
        )
  R.holdUniqDyn tasks

getParents :: HashMap UUID Task -> UUID -> [UUID]
getParents tasks = go [] (\uuid -> (^. #partof) =<< tasks ^. at uuid)
 where
  go :: (Eq a, Show a) => [a] -> (a -> Maybe a) -> a -> [a]
  go accu f x | x `elem` accu    = []
              | Just next <- f x = next : go (x : accu) f next
              | otherwise        = []

stateProvider
  :: forall t m
   . (WidgetIO t m)
  => R.Event t (NonEmpty DataChange)
  -> m (R.Dynamic t TaskState)
stateProvider stateChange = do
  createdTaskEvents <-
    R.performEventAsync
    $   (\list cb -> liftIO $ do
          tasks <- mapM
            (\(description, handler) -> handler <$> createTask description)
            list
          cb tasks
        )
    <$> R.select fannedEvent CreateTaskTag
  cache <- cacheProvider $ R.select fannedEvent ToggleEventTag
  tasks <-
    taskProvider $ R.select fannedEvent ChangeTaskTag <> createdTaskEvents
  pure $ R.zipDynWith buildTaskInfosMap tasks cache
 where
  mapToMap = \case
    ToggleEvent a b -> DMap.singleton ToggleEventTag (Identity $ pure (a, b))
    ChangeTask a    -> DMap.singleton ChangeTaskTag (Identity $ pure a)
    CreateTask a b  -> DMap.singleton CreateTaskTag (Identity $ pure (a, b))
  proofAdd :: FanTag a -> Identity a -> Identity a -> Identity a
  proofAdd = liftA2 . \case
    ToggleEventTag -> (<>)
    ChangeTaskTag  -> (<>)
    CreateTaskTag  -> (<>)
  fannedEvent =
    R.fan
      $   DMap.unionsWithKey proofAdd
      .   fmap mapToMap
      .   NonEmpty.toList
      <$> stateChange
  buildChildrenMap :: HashMap a Task -> HashMap UUID [a]
  buildChildrenMap =
    HashMap.fromListWith (++)
      . mapMaybe (\(uuid, task) -> (, pure uuid) <$> task ^. #partof)
      . HashMap.toList
  buildTaskInfosMap tasks cache = HashMap.mapWithKey
    (\u t -> TaskInfos t
                       (HashMap.lookupDefault False u (collapseState cache))
                       (HashMap.lookupDefault [] u childrenMap)
                       (getParents tasks u)
    )
    tasks
    where childrenMap = buildChildrenMap tasks

getCache :: IO Cache
getCache = catch
  (do
    Just cache :: Maybe Cache <- Aeson.decodeFileStrict cacheFile
    pure cache
  )
  (\(_ :: IOException) -> pure $ Cache HashMap.empty)

cacheFile = "/home/maralorn/.kassandra_cache"
