{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving, NamedFieldPuns #-}
{-# LANGUAGE LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables #-}
module Main
  ( main
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text.IO
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.Task               ( getTasks
                                                , Task
                                                )
import qualified Taskwarrior.Task              as Task
import qualified Data.Aeson                    as Aeson
import           Data.HashSet                   ( fromList
                                                , toList
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( HashMap )
import           Data.Foldable                  ( fold )
import qualified Data.Maybe                    as Maybe
import           Data.UUID                      ( UUID )
import           Control.Monad                  ( join
                                                , void
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Data.String.Interpolate        ( i )
import qualified Network.Simple.TCP            as Net

partof :: Task -> Maybe UUID
partof t = do
  p <- HashMap.lookup ("partof" :: Text) $ Task.uda t
  case Aeson.fromJSON p of
    Aeson.Success a -> Just a
    _               -> Nothing

type Widget t m
  = (D.DomBuilder t m, MonadFix m, R.MonadHold t m, R.PostBuild t m)
type WidgetIO t m
  = ( MonadIO m
    , Widget t m
    , R.TriggerEvent t m
    , R.PerformEvent t m
    , MonadIO (R.Performable m)
    )

newtype Cache = Cache { collapseState :: HashMap UUID Bool } deriving (Show, Read, Eq, Generic, Aeson.ToJSON, Aeson.FromJSON)

main :: IO ()
main = do
  Text.IO.putStrLn "Started kassandra"
  D.mainWidget $ do
    D.text "Welcome to kassandra!"
    rec taskState    <- stateProvider stateChanges
        stateChanges <- listsWidget taskState
    pure ()

listsWidget
  :: (Widget t m) => R.Dynamic t TaskState -> m (R.Event t StateChange)
listsWidget taskState = do
  D.text "Select a list"
  list <- listSelector (getLists <$> taskState)
  renderList taskState list
 where
  getLists :: TaskState -> [TaskList]
  getLists =
    fmap TagList
      . toList
      . fold
      . fmap (fromList . Task.tags)
      . filter ((Status.Pending ==) . Task.status)
      . fmap task
      . HashMap.elems
  listSelector
    :: (Widget t m) => R.Dynamic t [TaskList] -> m (R.Dynamic t TaskList)
  listSelector lists = do
    buttons   <- D.dyn $ mapM listButton <$> lists
    buttonSum <- R.switchHold R.never $ R.leftmost <$> buttons
    R.holdDyn (SubList []) buttonSum
  listButton :: (Widget t m) => TaskList -> m (R.Event t TaskList)
  listButton list | TagList tag <- list = (const list <$>) <$> D.button tag
                  | otherwise = (const list <$>) <$> D.button "Anonymous List"

renderList
  :: forall t m
   . (Widget t m)
  => R.Dynamic t TaskState
  -> R.Dynamic t TaskList
  -> m (R.Event t StateChange)
renderList tasks list = (D.dyn $ innerRenderList <$> list)
  >>= D.switchHold R.never
 where
  innerRenderList :: TaskList -> m (R.Event t StateChange)
  innerRenderList list'
    | TagList tag <- list' = do
      D.text tag
      R.switchDyn <$> (R.leftmost <$>) <$> flip D.simpleList
                                                (renderTask tasks)
                                                (tasksToShow tag <$> tasks)
    | SubList sublists <- list' = R.switchDyn
    <$> (R.leftmost <$>)
    <$> D.simpleList (D.constDyn sublists) (renderList tasks)

  tasksToShow :: Text -> TaskState -> [TaskInfos]
  tasksToShow tag taskState =
    Maybe.mapMaybe (maybePredicate) . HashMap.elems $ taskState
   where
    maybePredicate :: TaskInfos -> Maybe TaskInfos
    maybePredicate taskInfo = if inList taskInfo && not (parentInList taskInfo)
      then Just taskInfo
      else Nothing
    inList :: TaskInfos -> Bool
    inList (TaskInfos { task = Task.Task { tags } }) = elem tag tags
    parentInList :: TaskInfos -> Bool
    parentInList (TaskInfos { task }) =
      maybe False inList (partof task >>= flip HashMap.lookup taskState)

renderTask
  :: forall m t
   . (Widget t m)
  => R.Dynamic t TaskState
  -> R.Dynamic t TaskInfos
  -> m (R.Event t StateChange)
renderTask taskState taskInfos' =
  D.elAttr "div" ("style" D.=: "padding-left:20px") $ do
    taskInfos <- R.holdUniqDyn taskInfos'
    D.dyn_ $ D.text . status <$> taskInfos
    let showChilds = showChildren <$> taskInfos
    children <-
      R.holdUniqDyn
      $   (Maybe.mapMaybe <$> flip HashMap.lookup)
      <$> taskState
      <*> (children <$> taskInfos)
    toggleStateChange <-
      (   D.dyn
      $   (\s ->
            if not $ null s then collapseButton (showChilds) else pure R.never
          )
      <$> children
      )
      >>= (D.attachPromptlyDynWith
            ((ToggleEvent .) . ((,) . Task.uuid . task))
            taskInfos <$>
          )
      .   R.switchHold R.never
    childStateChanges <-
      (   D.dyn
      $   (\s -> if s
            then flip R.simpleList (renderTask taskState) children
            else pure (R.constDyn [])
          )
      <$> showChilds
      )
      >>= (R.switchDyn <$> fmap R.leftmost <$> join <$>)
      .   R.holdDyn (R.constDyn [])

    pure $ R.leftmost [toggleStateChange, childStateChanges]
 where
  status :: TaskInfos -> Text
  status TaskInfos { task } =
    (case Task.status task of
      Status.Pending       -> [i| ☐ #{Task.description task}|]
      (Status.Completed _) -> [i| ☑ #{Task.description task}|]
      (Status.Deleted   _) -> [i| ☒ #{Task.description task}|]
      _                    -> Task.description task
    )


collapseButton :: (Widget t m) => R.Dynamic t Bool -> m (R.Event t Bool)
collapseButton open = do
  let dynLabel = fmap (\s -> if s then ">" else "V") open
      button   = fmap D.button dynLabel
  buttonEventEvent <- D.dyn $ button
  toggleShow       <- R.switchHold R.never buttonEventEvent
  let boolEvent = fmap not $ R.tagPromptlyDyn open toggleShow
  pure boolEvent

data TaskInfos = TaskInfos { task :: Task, showChildren :: Bool, children :: [UUID]} deriving (Eq, Show)

type TaskState = (HashMap UUID TaskInfos)

data StateChange = ToggleEvent (UUID, Bool) | ChangeTask Task deriving (Eq, Show)

cacheProvider
  :: (WidgetIO t m) => R.Event t (UUID, Bool) -> m (R.Dynamic t Cache)
cacheProvider toggleEvent = do
  rec firstCache    <- liftIO getCache
      cache         <- R.holdDyn firstCache newCacheEvent
      newCacheEvent <- R.performEventAsync $ R.attachPromptlyDynWith
        (\Cache { collapseState } (uuid, state) cb -> do
          let newCache = Cache (HashMap.insert uuid state collapseState)
          void $ liftIO $ forkIO $ Aeson.encodeFile cacheFile newCache
          liftIO $ cb newCache
        )
        cache
        toggleEvent
  pure cache

taskProvider
  :: (WidgetIO t m) => R.Event t Task -> m (R.Dynamic t (HashMap UUID Task))
taskProvider _event = do
  (taskEvent, cb) <- R.newTriggerEvent
  tasks           <- R.foldDyn
    (flip (foldr (\t o -> (HashMap.insert (Task.uuid t) t o))))
    HashMap.empty
    taskEvent
  void . liftIO . forkIO $ do
    Text.IO.putStrLn "Listening for changed or new Tasks on 127.0.0.1:6545."
    Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) ->
      Net.recv socket 4096
        >>= ( maybe (Text.IO.putStrLn "Unsuccessful connection attempt.")
            $ \changes ->
                either
                    (\err -> Text.IO.putStrLn
                      [i|Couldn‘t decode #{changes} as Task: #{err}|]
                    )
                    cb
                  $ Aeson.eitherDecodeStrict changes
            )
  void . liftIO . forkIO $ (getTasks [] >>= cb)
  pure tasks

stateProvider
  :: (WidgetIO t m) => R.Event t StateChange -> m (R.Dynamic t TaskState)
stateProvider stateChange = do
  cache <- cacheProvider $ R.fforMaybe
    stateChange
    (\case
      ToggleEvent a -> Just a
      _             -> Nothing
    )
  tasks <- taskProvider $ R.fforMaybe
    stateChange
    (\case
      ChangeTask a -> Just a
      _            -> Nothing
    )
  let children =
        HashMap.fromListWith (++)
          .   Maybe.mapMaybe
                (\(uuid, task) -> fmap (\parent -> (parent, [uuid])) $ partof task
                )
          <$> HashMap.toList
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
  pure $ taskState

getCache :: IO Cache
getCache = catch
  (do
    Just cache :: Maybe Cache <- Aeson.decodeFileStrict cacheFile
    pure cache
  )
  (\(_ :: IOException) -> pure $ Cache HashMap.empty)

cacheFile = "/home/maralorn/.kassandra_cache"
data TaskList = TagList Text | SubList [TaskList] deriving (Eq, Show, Read)
