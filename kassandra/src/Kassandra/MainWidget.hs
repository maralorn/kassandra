{-# LANGUAGE PatternSynonyms #-}

module Kassandra.MainWidget (
  mainWidget,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Kassandra.BaseWidgets (button)
import Kassandra.Config (UIConfig)
import Kassandra.Debug (
  Severity (..),
  log,
  logR,
  setLogLevel,
 )
import Kassandra.ListWidget (listsWidget)
import Kassandra.State (StateProvider)
import Kassandra.TaskWidget (taskTreeWidget)
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types (
  AppState (AppState),
  AppStateChange,
  DragState (NoDrag),
  FilterState (FilterState),
  StandardWidget,
  TaskInfos,
  TaskState,
  WidgetIO,
  getDragState,
  getTasks,
 )
import Kassandra.Util (tellNewTask)
import qualified Reflex as R
import qualified Reflex.Dom as D

mainWidget :: WidgetIO t m => UIConfig -> StateProvider t m -> m ()
mainWidget _uiConfig stateProvider = do
  -- TODO: Use ui Config
  liftIO $ setLogLevel $ Just Debug
  log Info "Loaded Mainwidget"
  time <- liftIO getZonedTime
  timeDyn <-
    fmap (utcToZonedTime (zonedTimeZone time) . (^. lensVL R.tickInfo_lastUTC))
      <$> R.clockLossy 1 (zonedTimeToUTC time)
  let filterState = R.constDyn (FilterState 0 60)
  rec let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      taskState <- (^. #taskState) <<$>> stateProvider dataChangeEvents
      dragDyn <- R.holdDyn NoDrag $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT $
          runReaderT
            ( do
                taskDiagnosticsWidget
                D.divClass "content" widgetSwitcher
                infoFooter
            )
            (AppState taskState timeDyn dragDyn filterState)
      stateChanges <- logR Info (const "StateChange") stateChanges'
  pass

infoFooter :: (StandardWidget t m r e) => m ()
infoFooter = D.divClass "footer" $ do
  dragState <- getDragState
  D.dyn_ $
    dragState <&> \a -> do
      whenJust (preview #_DraggedTasks a) $ \(draggedTasksUuids :: NonEmpty UUID) -> do
        draggedTasksDyn <- (\taskMap -> catMaybes . toList $ (`HashMap.lookup` taskMap) <$> draggedTasksUuids) <<$>> getTasks
        D.dyn_ $
          draggedTasksDyn <&> \draggedTasks ->
            whenJust (nonEmpty draggedTasks) $ \tasks -> do
              D.text "Selected Tasks:"
              forM_ tasks $ \t -> D.divClass "selectedTask" $ taskTreeWidget (pure t)
  tellNewTask . fmap (,id)
    =<< createTextWidget
      (button "selector" $ D.text "New Task")
  tasks <- getTasks
  D.el "p" $
    D.dynText $
      tasks <&> \taskMap ->
        let taskList = HashMap.elems taskMap
            pending = length $ filter (has (#task % #status % #_Pending)) taskList
            waiting = length $ filter (has (#task % #status % #_Waiting)) taskList
            completed =
              length $ filter (has (#task % #status % #_Completed)) taskList
         in [i|#{pending} pending, #{waiting} waiting and #{completed} completed tasks. Kassandra-ToDo-Management|]

taskDiagnosticsWidget :: (StandardWidget t m r e) => m ()
taskDiagnosticsWidget = do
  tasks <- getTasks
  D.dynText $ do
    tasksMap <- tasks
    let uuids = HashMap.keys tasksMap
        hasLoop :: [UUID] -> UUID -> Maybe UUID
        hasLoop seen new
          | new `elem` seen = Just new
          | otherwise = firstJust (hasLoop (new : seen)) nexts
         where
          nexts = maybe [] (^. #children) $ HashMap.lookup new tasksMap
    pure $
      firstJust (hasLoop []) uuids & \case
        Just uuid -> "Found a loop for uuid " <> show uuid
        Nothing -> "" -- everything fine

widgets :: StandardWidget t m r e => [(Text, m ())]
widgets =
  [ ("Next", nextWidget)
  , ("Lists", listsWidget)
  , ("Inbox", inboxWidget)
  , ("Unsorted", unsortedWidget)
  ]

widgetSwitcher :: forall t m r e. StandardWidget t m r e => m ()
widgetSwitcher = D.el "div" $ do
  buttons <- forM (widgets @t @m) $ \l ->
    (l <$) . D.domEvent D.Click . fst
      <$> D.elClass'
        "a"
        "selector"
        (D.text $ fst l)
  listName <- R.holdDyn ("No list", pass) (R.leftmost buttons)
  D.el "div" $ D.dyn_ (snd <$> listName)

filterInbox :: TaskState -> [TaskInfos]
filterInbox tasks =
  sortOn (^. #modified) . toListOf (folded % filtered inInbox) $ tasks
 where
  inInbox :: TaskInfos -> Bool
  inInbox taskInfos =
    has (#tags % _Empty) taskInfos
      && has (#status % #_Pending) taskInfos
      && has (#children % _Empty) taskInfos
      && ( not
            . any (`notElem` ["kategorie", "project", "root"])
            . Set.unions
            . view (mapping #tags)
            $ lookupTasks tasks (taskInfos ^. #parents)
         )
      && not (taskInfos ^. #blocked)

lookupTasks :: TaskState -> [UUID] -> [TaskInfos]
lookupTasks tasks = mapMaybe (\uuid -> tasks ^. at uuid)

nextWidget :: (StandardWidget t m r e) => m ()
nextWidget = do
  inboxTasks <- fmap filterInbox <$> getTasks
  D.dynText $
    (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
      <$> inboxTasks
  void . flip R.simpleList taskTreeWidget $ take 1 <$> inboxTasks

inboxWidget :: (StandardWidget t m r e) => m ()
inboxWidget = do
  inboxTasks <- fmap filterInbox <$> getTasks
  D.dynText $
    (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
      <$> inboxTasks
  void . flip R.simpleList taskTreeWidget $ inboxTasks

unsortedWidget :: (StandardWidget t m r e) => m ()
unsortedWidget = do
  unsortedTasks <-
    fmap
      ( filter
          ( \task ->
              not (Set.member "root" (task ^. #tags))
                && has (#partof % _Nothing) task
                && has (#status % #_Pending) task
          )
          . HashMap.elems
      )
      <$> getTasks
  D.dynText $
    (\x -> "There are " <> show (length x) <> " unsorted tasks.")
      <$> unsortedTasks
  void . flip R.simpleList taskTreeWidget $ unsortedTasks
