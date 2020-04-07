{-# LANGUAGE TypeApplications,  LambdaCase, RecursiveDo, ScopedTypeVariables, OverloadedStrings, OverloadedLabels#-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import           Frontend.Types
import           Frontend.ListWidget            ( listsWidget
                                                , listWidget
                                                , TaskList(TagList)
                                                )
import           Frontend.State                 ( StateProvider )
import           Frontend.TaskWidget            ( taskWidget )

mainWidget :: WidgetIO t m => StateProvider t m -> m ()
mainWidget stateProvider = do
  time    <- liftIO getZonedTime
  timeDyn <-
    fmap (utcToZonedTime (zonedTimeZone time) . (^. lensVL R.tickInfo_lastUTC))
      <$> R.clockLossy 1 (zonedTimeToUTC time)
  let filterState = R.constDyn (FilterState 0 60)
  rec
    let (appChangeEvents, dataChangeEvents) =
          R.fanThese $ partitionEithersNE <$> stateChanges
    taskState <- stateProvider dataChangeEvents
    dragDyn   <-
      R.holdDyn NoDrag $ (\(DragChange a) -> a) . last <$> appChangeEvents
    (_, stateChanges) <- R.runEventWriterT $ runReaderT
      (do
        taskDiagnosticsWidget
        D.divClass "container" $ do
          D.divClass "pane" widgetSwitcher
          D.divClass "pane" (listWidget $ R.constDyn (TagList "root"))
      )
      (AppState taskState timeDyn dragDyn filterState)
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

widgets :: StandardWidget t m r => [(Text, m ())]
widgets =
  [ ("Next"    , nextWidget)
  , ("Lists"   , listsWidget)
  , ("Inbox"   , inboxWidget)
  , ("Unsorted", unsortedWidget)
  ]

widgetSwitcher :: forall t m r . StandardWidget t m r => m ()
widgetSwitcher = D.el "div" $ do
  buttons <- forM (widgets @t @m) $ \l ->
    (l <$) . D.domEvent D.Click . fst <$> D.elClass' "a"
                                                     "selector"
                                                     (D.text $ fst l)
  listName <- R.holdDyn ("No list", pass) (R.leftmost buttons)
  D.el "div" $ D.dyn_ (snd <$> listName)

filterInbox :: TaskState -> [TaskInfos]
filterInbox tasks =
  sortOn (^. #modified) . filter inInbox . HashMap.elems $ tasks
 where
  inInbox :: TaskInfos -> Bool
  inInbox taskInfos =
    has (#tags % _Empty) taskInfos
      && has (#status % #_Pending) taskInfos
      && has (#children % _Empty)  taskInfos
      && (  null
         .  filter (`notElem` ["kategorie", "project", "root"])
         .  join
         $  lookupTasks tasks (taskInfos ^. #parents)
         ^. #tags
         )
      && not (taskInfos ^. #blocked)

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
