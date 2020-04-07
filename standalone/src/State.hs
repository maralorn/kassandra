{-# LANGUAGE TypeApplications, TupleSections, FlexibleContexts, ConstraintKinds, StandaloneDeriving, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, GADTs, TemplateHaskell, OverloadedLabels, ViewPatterns #-}
module State
  ( ioStateProvider
  , ioStateFeeder
  )
where

import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import           Frontend.Types          hiding ( getTasks )
import           Frontend.State                 ( stateProvider
                                                , TaskProvider
                                                , CacheProvider
                                                , Cache(Cache, collapseState)
                                                , StateProvider
                                                )
import           Backend                        ( taskMonitor )

getCache :: IO Cache
getCache = catch
  (do
    Just cache :: Maybe Cache <- Aeson.decodeFileStrict cacheFile
    pure cache
  )
  (\(_ :: IOException) -> pure $ Cache HashMap.empty)

cacheFile = "/home/maralorn/.kassandra_cache"

ioCacheProvider :: (WidgetIO t m) => CacheProvider t m
ioCacheProvider toggleEvent = do
  firstCache <- liftIO getCache
  rec cache         <- R.holdDyn firstCache newCacheEvent
      newCacheEvent <- R.performEventAsync $ R.attachWith
        (\Cache { collapseState } newPairs cb -> liftIO $ do
          let newCache = Cache $ foldr
                (\(key, value) object -> HashMap.insert key value object)
                collapseState
                newPairs
          concurrently_ (Aeson.encodeFile cacheFile newCache) (cb newCache)
        )
        (R.current cache)
        toggleEvent
  pure cache


ioTaskProvider
  :: (WidgetIO t m) => MVar (NonEmpty Task -> IO ()) -> TaskProvider t m
ioTaskProvider callbackSlot changeTaskEvent = do
  void . R.performEvent $ liftIO . saveTasks . toList <$> changeTaskEvent
  (tasksEvent, newTasksCallBack) <- R.newTriggerEvent
  tasks <- R.foldDyn (flip . foldr . join $ HashMap.insert . (^. #uuid))
                     HashMap.empty
                     (changeTaskEvent <> tasksEvent)
  putMVar callbackSlot newTasksCallBack
  R.holdUniqDyn tasks

ioStateFeeder :: MVar (NonEmpty Task -> IO ()) -> IO ()
ioStateFeeder callbackSlot = do
  callback <- takeMVar callbackSlot
  concurrently_ (whenNotNullM (getTasks []) callback) (taskMonitor callback)

ioStateProvider
  :: WidgetIO t m => MVar (NonEmpty Task -> IO ()) -> (StateProvider t m)
ioStateProvider callbackSlot =
  stateProvider ioCacheProvider (ioTaskProvider callbackSlot)
