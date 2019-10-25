{-# LANGUAGE TypeApplications, TupleSections, FlexibleContexts, ConstraintKinds, StandaloneDeriving, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables #-}
module Main
  ( main
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text.IO
import qualified Reflex.Dom                    as D
import           Reflex.Dom                     ( (=:) )
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
import           Data.Time                      ( UTCTime )

data Query = And [Query] | Or [Query] | Not Query | HasTag Text | DescriptionContains Text | Pending | CompletedAfter UTCTime | Deleted | CloseUpwards Query | Waiting | CloseDownwards Query deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

filterTasks :: HashMap UUID TaskInfos -> Query -> HashMap UUID TaskInfos
filterTasks tasks = \case
  And queries -> foldr (HashMap.intersection . filterTasks tasks) tasks queries
  Or     queries -> HashMap.unions . fmap (filterTasks tasks) $ queries
  Not    query   -> HashMap.difference tasks (filterTasks tasks query)
  HasTag tag     -> HashMap.filter (elem tag . Task.tags . task) tasks
  DescriptionContains text ->
    HashMap.filter (Text.isInfixOf text . Task.description . task) tasks
  Pending -> HashMap.filter ((Status.Pending ==) . Task.status . task) tasks
  CompletedAfter end -> HashMap.filter (f . Task.status . task) tasks
   where
    f = \case
      Status.Completed { end = e } -> e >= end
      _                            -> False
  Deleted -> HashMap.filter (f . Task.status . task) tasks
   where
    f = \case
      Status.Deleted{} -> True
      _                -> False
  Waiting -> HashMap.filter (f . Task.status . task) tasks
   where
    f = \case
      Status.Waiting{} -> True
      _                -> False
  CloseUpwards query -> HashMap.foldr (HashMap.union . addParents)
                                      HashMap.empty
                                      (filterTasks tasks query)
   where
    addParents :: TaskInfos -> HashMap UUID TaskInfos
    addParents taskinfo =
      HashMap.singleton (Task.uuid $ task taskinfo) taskinfo
        `HashMap.union` maybe
                          HashMap.empty
                          addParents
                          (partof (task taskinfo) >>= (`HashMap.lookup` tasks))
  CloseDownwards query -> HashMap.foldr (HashMap.union . addChilds)
                                        HashMap.empty
                                        (filterTasks tasks query)
   where
    addChilds :: TaskInfos -> HashMap UUID TaskInfos
    addChilds taskinfo = HashMap.unions
      ( HashMap.singleton (Task.uuid $ task taskinfo) taskinfo
      : fmap
          addChilds
          (Maybe.mapMaybe (`HashMap.lookup` tasks) $ children taskinfo)
      )

partof :: Task -> Maybe UUID
partof t = do
  p <- HashMap.lookup ("partof" :: Text) $ Task.uda t
  case Aeson.fromJSON p of
    Aeson.Success a -> Just a
    _               -> Nothing

type Widget t m
  = (D.DomBuilder t m, MonadFix m, R.MonadHold t m, R.PostBuild t m)
type ViewWidget t m = (Widget t m, R.EventWriter t [StateChange] m)
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
    rec taskState         <- stateProvider stateChanges
        (_, stateChanges) <- R.runEventWriterT $ widgetSwitcher taskState
    pure ()

widgets :: (ViewWidget t m) => [(Text, R.Dynamic t TaskState -> m ())]
widgets = [("Lists", listsWidget)]

widgetSwitcher :: forall t m . (ViewWidget t m) => R.Dynamic t TaskState -> m ()
widgetSwitcher taskState = do
  buttons  <- mapM (\l -> (l <$) <$> D.button (fst l)) $ widgets @t @m
  listName <- R.holdDyn ("No list", const $ pure ()) (R.leftmost buttons)
  _        <- D.dyn (($ taskState) . snd <$> listName)
  pure ()

listsWidget
  :: (Widget t m, R.EventWriter t [StateChange] m)
  => R.Dynamic t TaskState
  -> m ()
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
   . (ViewWidget t m)
  => R.Dynamic t TaskState
  -> R.Dynamic t TaskList
  -> m ()
renderList tasks list = D.dyn_ (innerRenderList <$> list)
 where
  innerRenderList :: TaskList -> m ()
  innerRenderList list'
    | TagList tag <- list'
    = do
      D.text tag
      void . D.simpleList (tasksToShow tag <$> tasks) $ renderTask tasks
    | SubList sublists <- list'
    = void . D.simpleList (D.constDyn sublists) $ renderList tasks

  tasksToShow :: Text -> TaskState -> [TaskInfos]
  tasksToShow tag taskState =
    Maybe.mapMaybe maybePredicate . HashMap.elems $ taskState
   where
    maybePredicate :: TaskInfos -> Maybe TaskInfos
    maybePredicate taskInfo = if inList taskInfo && not (parentInList taskInfo)
      then Just taskInfo
      else Nothing
    inList :: TaskInfos -> Bool
    inList TaskInfos { task = Task.Task { tags } } = tag `elem` tags
    parentInList :: TaskInfos -> Bool
    parentInList TaskInfos { task } =
      maybe False inList (partof task >>= flip HashMap.lookup taskState)

renderTask
  :: forall m t
   . (ViewWidget t m)
  => R.Dynamic t TaskState
  -> R.Dynamic t TaskInfos
  -> m ()
renderTask taskState taskInfos' =
  D.elAttr "div" ("style" =: "padding-left:20px") $ do
    taskInfos <- R.holdUniqDyn taskInfos'
    D.dyn_ $ D.text . status <$> taskInfos
    let showChilds = showChildren <$> taskInfos
    children <-
      R.holdUniqDyn
      $   (Maybe.mapMaybe <$> flip HashMap.lookup)
      <$> taskState
      <*> (children <$> taskInfos)
    D.dyn
        ((\s -> if not $ null s then collapseButton showChilds else pure R.never
         )
        <$> children
        )
      >>= (D.attachPromptlyDynWith
            ((((: []) . ToggleEvent) .) . ((,) . Task.uuid . task))
            taskInfos <$>
          )
      .   R.switchHold R.never
      >>= R.tellEvent
    D.dyn_
      $   (\s -> if s
            then R.simpleList children (renderTask taskState)
            else pure (R.constDyn [])
          )
      <$> showChilds
 where
  status :: TaskInfos -> Text
  status TaskInfos { task } = case Task.status task of
    Status.Pending       -> [i| ☐ #{Task.description task}|]
    (Status.Completed _) -> [i| ☑ #{Task.description task}|]
    (Status.Deleted   _) -> [i| ☒ #{Task.description task}|]
    _                    -> Task.description task


collapseButton :: (Widget t m) => R.Dynamic t Bool -> m (R.Event t Bool)
collapseButton open = do
  let dynLabel = fmap (\s -> if s then ">" else "V") open
      button   = fmap D.button dynLabel
  buttonEventEvent <- D.dyn button
  toggleShow       <- R.switchHold R.never buttonEventEvent
  pure $ R.tagPromptlyDyn (not <$> open) toggleShow

data TaskInfos = TaskInfos { task :: Task, showChildren :: Bool, children :: [UUID]} deriving (Eq, Show)

type TaskState = (HashMap UUID TaskInfos)

data StateChange = ToggleEvent (UUID, Bool) | ChangeTask Task deriving (Eq, Show)

cacheProvider
  :: (WidgetIO t m) => R.Event t [(UUID, Bool)] -> m (R.Dynamic t Cache)
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
  :: (WidgetIO t m) => R.Event t [Task] -> m (R.Dynamic t (HashMap UUID Task))
taskProvider _changeTaskEvent = do
  (tasksEvent, newTasksCallBack) <- R.newTriggerEvent
  tasks <- R.foldDyn (flip . foldr . join $ HashMap.insert . Task.uuid)
                     HashMap.empty
                     (R.ffilter (not . null) (tasksEvent))
  void . liftIO . forkIO $ do
    Text.IO.putStrLn "Listening for changed or new Tasks on 127.0.0.1:6545."
    Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) ->
      Net.recv socket 4096 >>= maybe
        (Text.IO.putStrLn "Unsuccessful connection attempt.")
        (\changes ->
          either
              (\err -> Text.IO.putStrLn
                [i|Couldn‘t decode #{changes} as Task: #{err}|]
              )
              (newTasksCallBack . (: []))
            $ Aeson.eitherDecodeStrict @Task changes
        )
  void . liftIO . forkIO $ (getTasks [] >>= newTasksCallBack)
  R.holdUniqDyn tasks

stateProvider
  :: (WidgetIO t m) => R.Event t [StateChange] -> m (R.Dynamic t TaskState)
stateProvider stateChange = do
  cache <- cacheProvider $ fmap
    (Maybe.mapMaybe
      (\case
        ToggleEvent a -> Just a
        _             -> Nothing
      )
    )
    stateChange
  tasks <- taskProvider $ fmap
    (Maybe.mapMaybe
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

data TaskList = TagList Text | SubList [TaskList] deriving (Eq, Show, Read)
