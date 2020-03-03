{-# LANGUAGE TypeApplications,  LambdaCase, RecursiveDo, ScopedTypeVariables, OverloadedStrings, OverloadedLabels #-}
module Main
  ( main
  )
where

import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import           Data.UUID                      ( UUID )
import           Data.Time.LocalTime            ( getZonedTime
                                                , zonedTimeToUTC
                                                , zonedTimeZone
                                                , utcToZonedTime
                                                )
import           Taskwarrior.IO                 ( getUUIDs )
import           Data.List.Extra                ( firstJust )
import           Types
import           ListWidget
import           State
import           Css
import           TextEditWidget
import           TaskWidget                     ( taskWidget )

--data Query = And [Query] | Or [Query] | Not Query | HasTag Text | DescriptionContains Text | Pending | CompletedAfter UTCTime | Deleted | CloseUpwards Query | Waiting | CloseDownwards Query deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

--filterTasks :: HashMap UUID TaskInfos -> Query -> HashMap UUID TaskInfos
--filterTasks tasks = \case
  --And queries -> foldr (HashMap.intersection . filterTasks tasks) tasks queries
  --Or     queries -> HashMap.unions . fmap (filterTasks tasks) $ queries
  --Not    query   -> HashMap.difference tasks (filterTasks tasks query)
  --HasTag tag     -> HashMap.filter (elem tag . Task.tags . task) tasks
  --DescriptionContains text ->
    --HashMap.filter (isInfixOf text . Task.description . task) tasks
  --Pending -> HashMap.filter ((Status.Pending ==) . Task.status . task) tasks
  --CompletedAfter end -> HashMap.filter (f . Task.status . task) tasks
   --where
    --f = \case
      --Status.Completed { end = e } -> e >= end
      --_                            -> False
  --Deleted -> HashMap.filter (f . Task.status . task) tasks
   --where
    --f = \case
      --Status.Deleted{} -> True
      --_                -> False
  --Waiting -> HashMap.filter (f . Task.status . task) tasks
   --where
    --f = \case
      --Status.Waiting{} -> True
      --_                -> False
  --CloseUpwards query -> HashMap.foldr (HashMap.union . addParents)
                                      --HashMap.empty
                                      --(filterTasks tasks query)
   --where
    --addParents :: TaskInfos -> HashMap UUID TaskInfos
    --addParents taskinfo =
      --HashMap.singleton (Task.uuid $ task taskinfo) taskinfo
        --`HashMap.union` maybe
                          --HashMap.empty
                          --addParents
                          --(partof (task taskinfo) >>= (`HashMap.lookup` tasks))
  --CloseDownwards query -> HashMap.foldr (HashMap.union . addChilds)
                                        --HashMap.empty
                                        --(filterTasks tasks query)
   --where
    --addChilds :: TaskInfos -> HashMap UUID TaskInfos
    --addChilds taskinfo = HashMap.unions
      --( HashMap.singleton (Task.uuid $ task taskinfo) taskinfo : fmap
          --addChilds
          --(Maybe.mapMaybe (`HashMap.lookup` tasks) $ children taskinfo)
      --)


main :: IO ()
main = do
  putStrLn "Started kassandra"
  D.mainWidgetWithCss (encodeUtf8 $ toStrict css) $ do
    time    <- liftIO getZonedTime
    timeDyn <-
      fmap
          (utcToZonedTime (zonedTimeZone time) . (^. lensVL R.tickInfo_lastUTC))
        <$> R.clockLossy 1 (zonedTimeToUTC time)
    D.text "Welcome to kassandra!"
    let filterState = R.constDyn (FilterState 0 60)
    rec
      taskState         <- stateProvider dataChangeEvents
      (_, stateChanges) <- R.runEventWriterT $ runReaderT
        (do
          taskDiagnosticsWidget
          D.divClass "container" $ do
            D.divClass "pane" $ widgetSwitcher
            D.divClass "pane" $ widgetSwitcher
        )
        (AppState taskState timeDyn dragDyn filterState)
      let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      dragDyn <-
        R.holdDyn NoDrag $ (\(DragChange a) -> a) . last <$> appChangeEvents
    pass


taskDiagnosticsWidget :: (StandardWidget t m r) => m ()
taskDiagnosticsWidget = do
  tasks <- getTasks
  D.dynText $ do
    tasksMap <- tasks
    let uuids = HashMap.keys tasksMap
        hasLoop :: [UUID] -> UUID -> Maybe UUID
        hasLoop seen new | new `elem` seen = Just new
                         | otherwise = firstJust (hasLoop (new : seen)) nexts
          where nexts = maybe [] (^. #children) $ HashMap.lookup new tasksMap
    pure $ firstJust (hasLoop []) uuids & \case
      Just uuid -> "Found a loop for uuid " <> show uuid
      Nothing   -> "" -- everything fine

widgets
  :: ( StandardWidget t m r
     , R.PerformEvent t m
     , MonadIO (R.Performable m)
     , R.TriggerEvent t m
     )
  => [(Text, m ())]
widgets =
  [ ("Next"    , nextWidget)
  , ("Lists"   , listsWidget)
  , ("Search"  , searchWidget)
  , ("Inbox"   , inboxWidget)
  , ("Unsorted", unsortedWidget)
  ]

widgetSwitcher
  :: forall t m r
   . ( StandardWidget t m r
     , R.PerformEvent t m
     , MonadIO (R.Performable m)
     , R.TriggerEvent t m
     )
  => m ()

widgetSwitcher = do
  buttons  <- mapM (\l -> (l <$) <$> D.button (fst l)) $ widgets @t @m
  listName <- R.holdDyn ("No list", pass) (R.leftmost buttons)
  _        <- D.dyn (snd <$> listName)
  pass

filterInbox :: TaskState -> [TaskInfos]
filterInbox tasks =
  sortOn (^. #modified) . filter inInbox . HashMap.elems $ tasks
 where
  inInbox :: TaskInfos -> Bool
  inInbox taskInfos =
    has (#tags % _Empty) taskInfos
      && has (#status % #_Pending) taskInfos
      && has (#children % _Empty)  taskInfos
      && (  has _Empty
         .  filter (`notElem` ["kategorie", "project", "root"])
         .  join
         $  (lookupTasks tasks (taskInfos ^. #parents))
         ^. #tags
         )
      && ( has _Empty
         . filter (isn't (#status % #_Completed))
         . filter (isn't (#status % #_Deleted))
         $ lookupTasks tasks (taskInfos ^. #depends)
         )

lookupTasks :: TaskState -> [UUID] -> [TaskInfos]
lookupTasks tasks = mapMaybe (\uuid -> tasks ^. at uuid)

nextWidget :: (StandardWidget t m r) => m ()
nextWidget = do
  inboxTasks <- fmap filterInbox <$> getTasks
  D.dynText
    $   (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
    <$> inboxTasks
  void . flip R.simpleList taskWidget $ take 1 <$> inboxTasks

inboxWidget :: (StandardWidget t m r) => m ()
inboxWidget = do
  inboxTasks <- fmap filterInbox <$> getTasks
  D.dynText
    $   (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
    <$> inboxTasks
  void . flip R.simpleList taskWidget $ inboxTasks

unsortedWidget :: (StandardWidget t m r) => m ()
unsortedWidget = do
  unsortedTasks <-
    fmap
        ( filter
            (\task ->
              "root"
                `notElem` (task ^. #tags)
                &&        has (#partof % _Nothing)  task
                &&        has (#status % #_Pending) task
            )
        . HashMap.elems
        )
      <$> getTasks
  D.dynText
    $   (\x -> "There are " <> show (length x) <> " unsorted tasks.")
    <$> unsortedTasks
  void . flip R.simpleList taskWidget $ unsortedTasks

searchWidget
  :: ( StandardWidget t m r
     , R.PerformEvent t m
     , MonadIO (R.Performable m)
     , R.TriggerEvent t m
     )
  => m ()
searchWidget = do
  icon "" "search"
  searchInput <- D.inputElement D.def
  let searchText = R.tag (R.current $ D._inputElement_value searchInput)
                         (D.keypress D.Enter searchInput)
  uuidsEvent <-
    R.performEventAsync
    $   (\query cb -> liftIO (getUUIDs query >>= cb))
    .   ("(" :)
    .   (: [")", "+PENDING"])
    <$> searchText
  searchTextBeh <- R.hold "" searchText
  uuids         <- fmap UUIDList <$> R.holdDyn [] uuidsEvent
  message       <-
    R.holdDyn ""
    $   R.attachWith
          (\text leng -> "Found " <> show leng <> " tasks matching " <> text)
          searchTextBeh
    $   length
    <$> uuidsEvent
  D.el "div" $ D.dynText message
  listWidget uuids
  pass
