{-# LANGUAGE TypeApplications, TupleSections, FlexibleContexts, ConstraintKinds, StandaloneDeriving, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, GADTs, TemplateHaskell, OverloadedLabels, ViewPatterns #-}
module State
  ( ioStateProvider
  )
where

import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                , createTask
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import qualified Network.Simple.TCP            as Net
import           Frontend.Types          hiding ( getTasks )
import           Frontend.State                 ( stateProvider
                                                , TaskProvider
                                                , CacheProvider
                                                , Cache(Cache, collapseState)
                                                , StateProvider
                                                )

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
          void . forkIO $ Aeson.encodeFile cacheFile newCache
          cb newCache
        )
        (R.current cache)
        toggleEvent
  pure cache


ioTaskProvider :: (WidgetIO t m) => TaskProvider t m
ioTaskProvider changeTaskEvent createTaskEvent = do
  createdTaskEvents <-
    R.performEventAsync
    $   (\list cb -> liftIO
          (mapM (\(description, handler) -> handler <$> createTask description)
                list

          >>= cb
          )
        )
    <$> createTaskEvent
  void
    .   R.performEvent
    $   liftIO
    .   saveTasks
    .   toList
    <$> (changeTaskEvent <> createdTaskEvents)
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

ioStateProvider :: WidgetIO t m => StateProvider t m
ioStateProvider = stateProvider ioCacheProvider ioTaskProvider
