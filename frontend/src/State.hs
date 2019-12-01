{-# LANGUAGE TypeApplications, TupleSections, FlexibleContexts, ConstraintKinds, StandaloneDeriving, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables #-}
module State
  ( stateProvider
  )
where

import           ClassyPrelude
import           Taskwarrior.Task               ( Task )
import qualified Taskwarrior.Task              as Task
import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
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
import qualified Data.Maybe                    as Maybe
import           Control.Concurrent             ( forkIO )
import           Types                          ( StateChange
                                                  ( ToggleEvent
                                                  , ChangeTask
                                                  )
                                                , TaskState
                                                , TaskInfos(TaskInfos)
                                                , Widget
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Util                           ( partof )

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
  rec firstCache    <- liftIO getCache
      cache         <- R.holdDyn firstCache newCacheEvent
      newCacheEvent <- R.performEventAsync $ R.attachPromptlyDynWith
        (\Cache { collapseState } newPairs cb -> do
          let newCache = Cache $ foldr
                (\(key, value) object -> HashMap.insert key value object)
                collapseState
                newPairs
          void . liftIO . forkIO $ Aeson.encodeFile cacheFile newCache
          liftIO $ cb newCache
        )
        cache
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
  (tasksEvent, newTasksCallBack) <- R.newTriggerEvent
  tasks <- R.foldDyn (flip . foldr . join $ HashMap.insert . Task.uuid)
                     HashMap.empty
                     (R.ffilter (not . null) (tasksEvent))
  void . liftIO . forkIO $ do
    putStrLn "Listening for changed or new tasks on 127.0.0.1:6545."
    Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) ->
      Net.recv socket 4096 >>= maybe
        (putStrLn "Unsuccessful connection attempt.")
        (\changes ->
          either
              (\err -> putStrLn [i|Couldn‘t decode #{changes} as Task: #{err}|])
              (newTasksCallBack . singleton)
            $ Aeson.eitherDecodeStrict @Task changes
        )
  void . liftIO . forkIO $ (getTasks [] >>= newTasksCallBack)
  R.holdUniqDyn tasks

stateProvider
  :: (WidgetIO t m)
  => R.Event t (NonEmpty StateChange)
  -> m (R.Dynamic t TaskState)
stateProvider stateChange = do
  cache <- cacheProvider $ R.fmapMaybe
    (traverse
      (\case
        ToggleEvent a -> Just a
        _             -> Nothing
      )
    )
    stateChange
  tasks <- taskProvider $ R.fmapMaybe
    (traverse
      (\case
        ChangeTask a -> Just a
        _            -> Nothing
      )
    )
    stateChange
  let children =
        ( HashMap.fromListWith (++)
          . Maybe.mapMaybe (\(uuid, task) -> (, [uuid]) <$> partof task)
          )
          .   HashMap.toList
          <$> tasks
      taskState = do
        innerCache    <- cache
        innerChildren <- children
        HashMap.mapWithKey
            (\u t -> TaskInfos
              t
              (HashMap.lookupDefault False u (collapseState innerCache))
              (HashMap.lookupDefault [] u innerChildren)
            )
          <$> tasks
  pure taskState

getCache :: IO Cache
getCache = catch
  (do
    Just cache :: Maybe Cache <- Aeson.decodeFileStrict cacheFile
    pure cache
  )
  (\(_ :: IOException) -> pure $ Cache HashMap.empty)

cacheFile = "/home/maralorn/.kassandra_cache"
